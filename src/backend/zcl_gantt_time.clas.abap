class ZCL_GANTT_TIME definition
  public
  create public .

public section.

  class-methods GET
    returning
      value(RV_TIME) type ZGANTT_TIMESTAMP .
protected section.
private section.
ENDCLASS.



CLASS ZCL_GANTT_TIME IMPLEMENTATION.


  METHOD get.

    GET TIME STAMP FIELD rv_time.

  ENDMETHOD.
ENDCLASS.