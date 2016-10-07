class ZCL_GANTT_PROJECT definition
  public
  create public .

public section.

  class-methods LIST
    returning
      value(RT_LIST) type ZGANTT_PROJECTS_TT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_GANTT_PROJECT IMPLEMENTATION.


  METHOD list.

    SELECT * FROM zgantt_projects
      INTO TABLE rt_list
      ORDER BY PRIMARY KEY.

  ENDMETHOD.
ENDCLASS.