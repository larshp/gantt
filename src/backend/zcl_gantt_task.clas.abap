class ZCL_GANTT_TASK definition
  public
  create public .

public section.

  class-methods LIST
    importing
      !IV_PROJECT_ID type ZGANTT_PROJECT_ID
    returning
      value(RT_LIST) type ZGANTT_TASK_DATA_TT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_GANTT_TASK IMPLEMENTATION.


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
      WHERE project_id = iv_project_id
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