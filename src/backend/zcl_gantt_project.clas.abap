CLASS zcl_gantt_project DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS list
      RETURNING
        VALUE(rt_list) TYPE zgantt_projects_tt.
    CLASS-METHODS create
      IMPORTING
        !is_data             TYPE zgantt_projects_data
      RETURNING
        VALUE(rv_project_id) TYPE zgantt_project_id.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_GANTT_PROJECT IMPLEMENTATION.


  METHOD create.

    DATA: ls_data TYPE zgantt_projects.


    zcl_gantt_number_range=>number_get_next(
      EXPORTING
        iv_nrrangenr = '01'
        iv_object    = 'ZGANTT_PRO'
      IMPORTING
        ev_id        = ls_data-project_id ).
    MOVE-CORRESPONDING is_data TO ls_data.

    INSERT zgantt_projects FROM ls_data.
    ASSERT sy-subrc = 0.

    rv_project_id = ls_data-project_id.

  ENDMETHOD.


  METHOD list.

    SELECT * FROM zgantt_projects
      INTO TABLE rt_list
      ORDER BY PRIMARY KEY.

  ENDMETHOD.
ENDCLASS.