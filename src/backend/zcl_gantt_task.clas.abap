class ZCL_GANTT_TASK definition
  public
  create public .

public section.

  class-methods LIST
    importing
      !IV_PROJECT_ID type ZGANTT_PROJECT_ID
    returning
      value(RT_LIST) type ZGANTT_TASK_LIST_TT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_GANTT_TASK IMPLEMENTATION.


  METHOD list.

    SELECT * FROM zgantt_tasks
      INTO TABLE rt_list
      WHERE project_id = iv_project_id
      ORDER BY PRIMARY KEY.

  ENDMETHOD.
ENDCLASS.