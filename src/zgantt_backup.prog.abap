REPORT zgantt_backup.

PARAMETERS: p_mail TYPE text200 OBLIGATORY.

START-OF-SELECTION.
  PERFORM run.

CLASS lcl_xml_pretty DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: print
      IMPORTING iv_xml           TYPE string
                iv_ignore_errors TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(rv_xml)    TYPE string.

ENDCLASS.

CLASS lcl_xml_pretty IMPLEMENTATION.

  METHOD print.

    DATA: li_ixml           TYPE REF TO if_ixml,
          li_xml_doc        TYPE REF TO if_ixml_document,
          li_stream_factory TYPE REF TO if_ixml_stream_factory,
          li_istream        TYPE REF TO if_ixml_istream,
          li_parser         TYPE REF TO if_ixml_parser,
          li_ostream        TYPE REF TO if_ixml_ostream,
          li_renderer       TYPE REF TO if_ixml_renderer.


    ASSERT NOT iv_xml IS INITIAL.

    li_ixml = cl_ixml=>create( ).
    li_xml_doc = li_ixml->create_document( ).

    li_stream_factory = li_ixml->create_stream_factory( ).
    li_istream = li_stream_factory->create_istream_string( iv_xml ).
    li_parser = li_ixml->create_parser( stream_factory = li_stream_factory
                                        istream        = li_istream
                                        document       = li_xml_doc ).
    li_parser->set_normalizing( abap_true ).
    IF li_parser->parse( ) <> 0.
      IF iv_ignore_errors = abap_true.
        rv_xml = iv_xml.
        RETURN.
      ELSE.
* error while parsing
        ASSERT 0 = 1.
      ENDIF.
    ENDIF.
    li_istream->close( ).

    li_ostream = li_stream_factory->create_ostream_cstring( rv_xml ).

    li_renderer = li_ixml->create_renderer( ostream  = li_ostream
                                            document = li_xml_doc ).

    li_renderer->set_normalizing( abap_true ).

    li_renderer->render( ).

  ENDMETHOD.

ENDCLASS.

FORM run.

  DATA: BEGIN OF ls_data,
          projects TYPE STANDARD TABLE OF zgantt_projects WITH DEFAULT KEY,
          tasks    TYPE STANDARD TABLE OF zgantt_tasks WITH DEFAULT KEY,
          deps     TYPE STANDARD TABLE OF zgantt_deps WITH DEFAULT KEY,
        END OF ls_data.

  DATA: lv_xml TYPE string.


  SELECT * FROM zgantt_projects INTO TABLE ls_data-projects.
  SELECT * FROM zgantt_tasks INTO TABLE ls_data-tasks.
  SELECT * FROM zgantt_deps INTO TABLE ls_data-deps.

  CALL TRANSFORMATION id
    SOURCE data = ls_data
    RESULT XML lv_xml.

  lv_xml = lcl_xml_pretty=>print( lv_xml ).

  PERFORM mail USING lv_xml.

ENDFORM.

FORM mail USING iv_xml TYPE string RAISING cx_bcs.

  DATA: lt_soli TYPE soli_tab.

  FIELD-SYMBOLS: <ls_soli> LIKE LINE OF lt_soli.


  DATA(lo_request) = cl_bcs=>create_persistent( ).
  DATA(lo_adr) = cl_cam_address_bcs=>create_internet_address( CONV #( p_mail ) ).

  lo_request->add_recipient(
    i_recipient = lo_adr
    i_express   = abap_true ).

  SPLIT iv_xml AT cl_abap_char_utilities=>newline INTO TABLE lt_soli.
  LOOP AT lt_soli ASSIGNING <ls_soli>.
    CONDENSE <ls_soli>-line.
  ENDLOOP.

  DATA(lo_document) = cl_document_bcs=>create_document(
    i_type    = 'RAW'
    i_text    = lt_soli
    i_subject = 'Gantt - XML backup' ).
  lo_request->set_document( lo_document ).

  lo_request->send( ).

  COMMIT WORK.

  WRITE: / 'Done'(001).

ENDFORM.
