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

    DATA: lt_tasks TYPE STANDARD TABLE OF zgantt_tasks WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_task>   LIKE LINE OF lt_tasks,
                   <ls_result> LIKE LINE OF rt_list.


    SELECT * FROM zgantt_tasks
      INTO TABLE lt_tasks
      WHERE project_id = iv_project_id
      ORDER BY PRIMARY KEY.

    LOOP AT lt_tasks ASSIGNING <ls_task>.
      APPEND INITIAL LINE TO rt_list ASSIGNING <ls_result>.
      <ls_result>-header = <ls_task>.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.