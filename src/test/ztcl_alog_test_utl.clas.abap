"! ABAP Unit test utilities
CLASS ztcl_alog_test_utl DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  FOR TESTING.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! @parameter rv_text | Random text between 1 and 200 characters long
      get_random_log_text RETURNING VALUE(rv_text) TYPE string,
      "! @parameter ro_type | Random log entry type
      get_random_log_type RETURNING VALUE(ro_type) TYPE REF TO zcl_alog_entry_type.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ztcl_alog_test_utl IMPLEMENTATION.
  METHOD get_random_log_text.
    STATICS: so_rnd TYPE REF TO cl_abap_random_int.

    IF so_rnd IS NOT BOUND.
      so_rnd = cl_abap_random_int=>create( min = 1 max = 200 ).
    ENDIF.

    CALL FUNCTION 'GENERAL_GET_RANDOM_STRING'
      EXPORTING
        number_chars  = so_rnd->get_next( )
      IMPORTING
        random_string = rv_text.
  ENDMETHOD.

  METHOD get_random_log_type.
    STATICS: so_rnd TYPE REF TO cl_abap_random_int.

    IF so_rnd IS NOT BOUND.
      so_rnd = cl_abap_random_int=>create( min = 1 max = 4 ).
    ENDIF.

    ro_type = SWITCH #( LET rnd = so_rnd->get_next( ) IN rnd
                        WHEN 1 THEN zcl_alog_entry_type=>go_info
                        WHEN 2 THEN zcl_alog_entry_type=>go_warning
                        WHEN 3 THEN zcl_alog_entry_type=>go_error
                        WHEN 4 THEN zcl_alog_entry_type=>go_debug ).
  ENDMETHOD.
ENDCLASS.
