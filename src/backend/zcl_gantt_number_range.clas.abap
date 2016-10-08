class ZCL_GANTT_NUMBER_RANGE definition
  public
  create public .

public section.

  class-methods NUMBER_GET_NEXT
    importing
      !IV_NRRANGENR type INRI-NRRANGENR
      !IV_OBJECT type INRI-OBJECT
    exporting
      !EV_ID type CLIKE .
protected section.

  class-methods CREATE_NUMBER_INTERVAL
    importing
      !IV_NRRANGENR type INRI-NRRANGENR
      !IV_OBJECT type INRI-OBJECT .
private section.
ENDCLASS.



CLASS ZCL_GANTT_NUMBER_RANGE IMPLEMENTATION.


  METHOD create_number_interval.

    DATA: lv_error    TYPE c LENGTH 1,
          ls_error    TYPE inrer,
          lt_list     TYPE STANDARD TABLE OF inriv WITH DEFAULT KEY,
          lt_error_iv TYPE STANDARD TABLE OF inriv WITH DEFAULT KEY,
          lt_interval TYPE STANDARD TABLE OF inriv WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_interval> LIKE LINE OF lt_interval.


    CALL FUNCTION 'NUMBER_RANGE_INTERVAL_LIST'
      EXPORTING
        object                     = iv_object
      TABLES
        interval                   = lt_list
      EXCEPTIONS
        nr_range_nr1_not_found     = 1
        nr_range_nr1_not_intern    = 2
        nr_range_nr2_must_be_space = 3
        nr_range_nr2_not_extern    = 4
        nr_range_nr2_not_found     = 5
        object_not_found           = 6
        subobject_must_be_space    = 7
        subobject_not_found        = 8
        OTHERS                     = 9.
    ASSERT sy-subrc = 0.

    IF lines( lt_list ) > 0.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO lt_interval ASSIGNING <ls_interval>.
*    <ls_interval>-subobject
    <ls_interval>-nrrangenr = iv_nrrangenr.
*    <ls_interval>-toyear
    <ls_interval>-fromnumber = '00000001'.
    <ls_interval>-tonumber = '99999999'.
*    <ls_interval>-nrlevel
*    <ls_interval>-externind
    <ls_interval>-procind = 'I'.

    CALL FUNCTION 'NUMBER_RANGE_INTERVAL_UPDATE'
      EXPORTING
        object           = iv_object
      IMPORTING
        error            = ls_error
        error_occured    = lv_error
      TABLES
        error_iv         = lt_error_iv
        interval         = lt_interval
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0 OR lv_error = abap_true.
      BREAK-POINT.
    ENDIF.

    CALL FUNCTION 'NUMBER_RANGE_UPDATE_CLOSE'
      EXPORTING
        object                 = iv_object
      EXCEPTIONS
        no_changes_made        = 1
        object_not_initialized = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      BREAK-POINT.
    ENDIF.

  ENDMETHOD.


  METHOD number_get_next.

    CLEAR ev_id.

    create_number_interval( iv_nrrangenr = iv_nrrangenr
                            iv_object    = iv_object ).

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = iv_nrrangenr
        object                  = iv_object
      IMPORTING
        number                  = ev_id
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.