CLASS lcl_logger DEFINITION INHERITING FROM zcl_alog_logger_base.
  PUBLIC SECTION.
    METHODS:
      reset.
    DATA:
      mv_called TYPE abap_bool.
  PROTECTED SECTION.
    METHODS:
      entry_internal REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_logger IMPLEMENTATION.
  METHOD entry_internal.
    mv_called = abap_true.
  ENDMETHOD.

  METHOD reset.
    mv_called = abap_false.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_test DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PUBLIC SECTION.
    METHODS:
      test_priority FOR TESTING.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      setup,
      teardown.
    DATA:
      mo_logger TYPE REF TO lcl_logger.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD setup.
    mo_logger = NEW lcl_logger( ).
  ENDMETHOD.

  METHOD teardown.
    FREE mo_logger.
  ENDMETHOD.

  METHOD test_priority.
    " Default minimal log level (DEBUG)
    mo_logger->debug( 'Test' ).
    cl_abap_unit_assert=>assert_true( act = mo_logger->mv_called ).
    mo_logger->reset( ).
    mo_logger->info( 'Test' ).
    cl_abap_unit_assert=>assert_true( act = mo_logger->mv_called ).
    mo_logger->reset( ).
    mo_logger->warning( 'Test' ).
    cl_abap_unit_assert=>assert_true( act = mo_logger->mv_called ).
    mo_logger->reset( ).
    mo_logger->error( 'Test' ).
    cl_abap_unit_assert=>assert_true( act = mo_logger->mv_called ).
    mo_logger->reset( ).
    mo_logger->exception( NEW zcx_alog_argument_null( ) ).
    cl_abap_unit_assert=>assert_true( act = mo_logger->mv_called ).
    mo_logger->reset( ).

    " Info
    mo_logger->set_minimal_log_level( zcl_alog_entry_type=>go_info ).
    cl_abap_unit_assert=>assert_equals( act = mo_logger->get_minimal_log_level( )
                                        exp = zcl_alog_entry_type=>go_info ).
    mo_logger->debug( 'Test' ).
    cl_abap_unit_assert=>assert_false( act = mo_logger->mv_called ).
    mo_logger->reset( ).
    mo_logger->info( 'Test' ).
    cl_abap_unit_assert=>assert_true( act = mo_logger->mv_called ).
    mo_logger->reset( ).
    mo_logger->warning( 'Test' ).
    cl_abap_unit_assert=>assert_true( act = mo_logger->mv_called ).
    mo_logger->reset( ).
    mo_logger->error( 'Test' ).
    cl_abap_unit_assert=>assert_true( act = mo_logger->mv_called ).
    mo_logger->reset( ).
    mo_logger->exception( NEW zcx_alog_argument_null( ) ).
    cl_abap_unit_assert=>assert_true( act = mo_logger->mv_called ).
    mo_logger->reset( ).

    " Warning
    mo_logger->set_minimal_log_level( zcl_alog_entry_type=>go_warning ).
    cl_abap_unit_assert=>assert_equals( act = mo_logger->get_minimal_log_level( )
                                        exp = zcl_alog_entry_type=>go_warning ).
    mo_logger->debug( 'Test' ).
    cl_abap_unit_assert=>assert_false( act = mo_logger->mv_called ).
    mo_logger->reset( ).
    mo_logger->info( 'Test' ).
    cl_abap_unit_assert=>assert_false( act = mo_logger->mv_called ).
    mo_logger->reset( ).
    mo_logger->warning( 'Test' ).
    cl_abap_unit_assert=>assert_true( act = mo_logger->mv_called ).
    mo_logger->reset( ).
    mo_logger->error( 'Test' ).
    cl_abap_unit_assert=>assert_true( act = mo_logger->mv_called ).
    mo_logger->reset( ).
    mo_logger->exception( NEW zcx_alog_argument_null( ) ).
    cl_abap_unit_assert=>assert_true( act = mo_logger->mv_called ).
    mo_logger->reset( ).

    " Error
    mo_logger->set_minimal_log_level( zcl_alog_entry_type=>go_error ).
    cl_abap_unit_assert=>assert_equals( act = mo_logger->get_minimal_log_level( )
                                        exp = zcl_alog_entry_type=>go_error ).
    mo_logger->debug( 'Test' ).
    cl_abap_unit_assert=>assert_false( act = mo_logger->mv_called ).
    mo_logger->reset( ).
    mo_logger->info( 'Test' ).
    cl_abap_unit_assert=>assert_false( act = mo_logger->mv_called ).
    mo_logger->reset( ).
    mo_logger->warning( 'Test' ).
    cl_abap_unit_assert=>assert_false( act = mo_logger->mv_called ).
    mo_logger->reset( ).
    mo_logger->error( 'Test' ).
    cl_abap_unit_assert=>assert_true( act = mo_logger->mv_called ).
    mo_logger->reset( ).
    mo_logger->exception( NEW zcx_alog_argument_null( ) ).
    cl_abap_unit_assert=>assert_true( act = mo_logger->mv_called ).
    mo_logger->reset( ).
  ENDMETHOD.
ENDCLASS.
