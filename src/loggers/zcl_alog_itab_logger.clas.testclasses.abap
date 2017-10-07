CLASS lcl_test DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PUBLIC SECTION.
    METHODS:
      test_entry FOR TESTING.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      setup,
      teardown.
    DATA:
      mo_logger TYPE REF TO zcl_alog_itab_logger.
ENDCLASS.

CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    mo_logger = NEW #( ).
  ENDMETHOD.

  METHOD teardown.
    FREE mo_logger.
  ENDMETHOD.

  METHOD test_entry.
    TYPES: BEGIN OF lty_test,
             text        TYPE zalog_s_logentry-text,
             type        TYPE REF TO zcl_alog_entry_type,
             description TYPE zalog_s_logentry-type_descr,
           END OF lty_test,
           lty_test_tab TYPE STANDARD TABLE OF lty_test WITH DEFAULT KEY.
    DATA: lt_test TYPE lty_test_tab.

    DO 400 TIMES.
      APPEND VALUE #( LET type = ztcl_alog_test_utl=>get_random_log_type( ) IN
                      text = ztcl_alog_test_utl=>get_random_log_text( )
                      type = type
                      description = type->mv_description
                    ) TO lt_test.
    ENDDO.

    LOOP AT lt_test ASSIGNING FIELD-SYMBOL(<ls_test>).
      mo_logger->entry( iv_text = <ls_test>-text io_type = <ls_test>-type ).
      FREE <ls_test>-type.
    ENDLOOP.

    cl_abap_unit_assert=>assert_equals(
      exp = lt_test
      act = CORRESPONDING lty_test_tab(
              mo_logger->get_log_table( ) MAPPING text        = text
                                                  description = type_descr
            )
    ).
  ENDMETHOD.
ENDCLASS.
