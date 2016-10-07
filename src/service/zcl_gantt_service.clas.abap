class ZCL_GANTT_SERVICE definition
  public
  create public .

public section.

  interfaces ZIF_SWAG_HANDLER .
  interfaces IF_HTTP_EXTENSION .

  methods LIST_PROJECTS
    returning
      value(RT_LIST) type ZGANTT_PROJECTS_TT .
  methods LIST_TASKS
    importing
      !IV_PROJECT_ID type ZGANTT_PROJECT_ID
    returning
      value(RT_LIST) type ZGANTT_TASK_LIST_TT .
protected section.

  data MI_SERVER type ref to IF_HTTP_SERVER .

  methods READ_MIME
    importing
      !IV_URL type STRING .
  methods SERVE_REST .
  methods SERVE_STATIC .
private section.
ENDCLASS.



CLASS ZCL_GANTT_SERVICE IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

    DATA: lv_reason TYPE string,
          lv_path   TYPE string.


mi_server = server.
    lv_path = mi_server->request->get_header_field( '~path' ).

    IF lv_path CP '/sap/zgantt/rest/*'.
      serve_rest( ).
      COMMIT WORK.
    ELSE.
      serve_static( ).
    ENDIF.

  ENDMETHOD.


  METHOD list_projects.

    rt_list = zcl_gantt_project=>list( ).

  ENDMETHOD.


  METHOD list_tasks.

    rt_list = zcl_gantt_task=>list( iv_project_id ).

  ENDMETHOD.


  METHOD read_mime.

    DATA: li_api       TYPE REF TO if_mr_api,
          lv_data      TYPE xstring,
          lv_changed   TYPE smimphio-chng_time,
          lv_timestamp TYPE char14,
          lv_modified  TYPE string,
          lv_mime      TYPE string,
          lv_url       TYPE string.


    CONCATENATE '/SAP/PUBLIC/zgantt/' iv_url INTO lv_url.

    li_api = cl_mime_repository_api=>if_mr_api~get_api( ).

    li_api->get(
      EXPORTING
        i_url                  = lv_url
      IMPORTING
        e_content              = lv_data
        e_mime_type            = lv_mime
        e_content_last_changed = lv_changed
      EXCEPTIONS
        not_found              = 1 ).
    IF sy-subrc = 1.
      mi_server->response->set_cdata( '404' ).
      mi_server->response->set_status( code = 404 reason = '404' ).
      RETURN.
    ENDIF.

    lv_timestamp = lv_changed.
    lv_modified = cl_bsp_utility=>date_to_string_http( lv_timestamp ).

    IF lv_modified = mi_server->request->get_header_field( 'If-Modified-Since' ) ##NO_TEXT.
      mi_server->response->set_status( code = 304 reason = '' ).
      RETURN.
    ENDIF.

    mi_server->response->set_header_field(
      name  = 'Cache-Control'
      value = 'max-age=86400' ) ##NO_TEXT.

    mi_server->response->set_header_field(
      name  = 'Last-Modified'
      value = lv_modified ) ##NO_TEXT.

    mi_server->response->set_compression( ).
    mi_server->response->set_content_type( lv_mime ).
    mi_server->response->set_data( lv_data ).

  ENDMETHOD.


  METHOD serve_rest.

    DATA: lo_swag TYPE REF TO zcl_swag.


    CREATE OBJECT lo_swag
      EXPORTING
        ii_server = mi_server
        iv_base   = '/sap/zgantt/rest'
        iv_title  = 'Gantt'.
    lo_swag->register( me ).

    lo_swag->run( ).

  ENDMETHOD.


  METHOD serve_static.

    DATA: lv_name TYPE string,
          lv_path TYPE string.


    lv_path = mi_server->request->get_header_field( '~path' ).

    FIND REGEX '/sap/zgantt/static/(.*)'
      IN lv_path
      SUBMATCHES lv_name ##NO_TEXT.

    IF lv_name IS INITIAL.
      lv_name = 'index.html' ##NO_TEXT.
    ENDIF.

    read_mime( lv_name ).

  ENDMETHOD.


  METHOD zif_swag_handler~meta.

    FIELD-SYMBOLS: <ls_meta> LIKE LINE OF rt_meta.


    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'List Projects'.
    <ls_meta>-url-regex = '/projects$'.
    <ls_meta>-method    = zcl_swag=>c_method-get.
    <ls_meta>-handler   = 'LIST_PROJECTS'.

    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'List Tasks'.
    <ls_meta>-url-regex = '/tasks/(\d+)$'.
    APPEND 'IV_PROJECT_ID' TO <ls_meta>-url-group_names.
    <ls_meta>-method    = zcl_swag=>c_method-get.
    <ls_meta>-handler   = 'LIST_TASKS'.

  ENDMETHOD.
ENDCLASS.