CLASS zcl_gantt_task DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS list
      IMPORTING
        !iv_project_id TYPE zgantt_project_id
      RETURNING
        VALUE(rt_list) TYPE zgantt_task_data_tt .
    CLASS-METHODS create
      IMPORTING
        !is_data          TYPE zgantt_tasks_data
      RETURNING
        VALUE(rv_task_id) TYPE zgantt_tasks-task_id .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_GANTT_TASK IMPLEMENTATION.


  METHOD create.

    DATA: ls_data TYPE zgantt_tasks.


    zcl_gantt_number_range=>number_get_next(
      EXPORTING
        iv_nrrangenr = '01'
        iv_object    = 'ZGANTT_TAS'
      IMPORTING
        ev_id        = ls_data-task_id ).
    MOVE-CORRESPONDING is_data TO ls_data.

    ls_data-created_by = sy-uname.
    ls_data-created_at = zcl_gantt_time=>get( ).
    ls_data-changed_by = sy-uname.
    ls_data-changed_at = zcl_gantt_time=>get( ).

* todo, validations?
    ASSERT NOT ls_data-project_id IS INITIAL.
    ASSERT NOT ls_data-task_id IS INITIAL.

    INSERT zgantt_tasks FROM ls_data.
    ASSERT sy-subrc = 0.

    rv_task_id = ls_data-task_id.

  ENDMETHOD.


  METHOD list.

    DATA: lt_tasks TYPE STANDARD TABLE OF zgantt_tasks WITH DEFAULT KEY,
          lt_deps  TYPE STANDARD TABLE OF zgantt_deps WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_task>   LIKE LINE OF lt_tasks,
                   <ls_sub>    LIKE LINE OF lt_tasks,
                   <ls_result> LIKE LINE OF rt_list,
                   <ls_dep>    LIKE LINE OF <ls_result>-dependencies.


    SELECT * FROM zgantt_tasks
      INTO TABLE lt_tasks
      WHERE project_id = iv_project_id
      ORDER BY PRIMARY KEY.

    SELECT * FROM zgantt_deps
      INTO TABLE lt_deps
      ORDER BY PRIMARY KEY.

    LOOP AT lt_tasks ASSIGNING <ls_task>.
      APPEND INITIAL LINE TO rt_list ASSIGNING <ls_result>.
      <ls_result>-header = <ls_task>.

      LOOP AT lt_tasks ASSIGNING <ls_sub> WHERE task_id <> <ls_task>-task_id.
        APPEND INITIAL LINE TO <ls_result>-dependencies ASSIGNING <ls_dep>.
        <ls_dep>-task_id = <ls_sub>-task_id.
        READ TABLE lt_deps
          WITH KEY task_id = <ls_task>-task_id
          dependency = <ls_sub>-task_id
          TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          <ls_dep>-dependency = abap_true.
        ELSE.
          <ls_dep>-dependency = abap_false.
        ENDIF.
        <ls_dep>-description = <ls_sub>-description.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.