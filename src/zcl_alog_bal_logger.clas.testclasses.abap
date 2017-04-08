CLASS lcl_test DEFINITION
  FOR TESTING
  RISK LEVEL CRITICAL
  DURATION SHORT.

  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor.
    METHODS:
      test_entry FOR TESTING,
      test_exception FOR TESTING,
      test_save FOR TESTING.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF gty_test,
        text  TYPE zalog_s_logentry-text,
        type  TYPE REF TO zcl_alog_entry_type,
        msgty TYPE symsgty,
      END OF gty_test,
      gty_test_tab TYPE STANDARD TABLE OF gty_test WITH DEFAULT KEY.
    CONSTANTS:
      gc_bal_obj    TYPE balobj_d VALUE 'ZALOG_TEST',
      gc_bal_subobj TYPE balsubobj VALUE 'SUB'.
    CLASS-METHODS:
      class_setup,
      class_teardown,
      get_bal_entries IMPORTING iv_handle         TYPE balloghndl
                      RETURNING VALUE(rt_entries) TYPE bal_t_msgr.
    METHODS:
      setup,
      teardown.
    CLASS-DATA:
      gs_bal_obj_entry    TYPE balobj,
      gs_bal_subobj_entry TYPE balsub.
    DATA:
      mo_logger TYPE REF TO zcl_alog_bal_logger.
ENDCLASS.

CLASS lcl_test IMPLEMENTATION.
  METHOD class_constructor.
    gs_bal_obj_entry = VALUE #( object = gc_bal_obj ).
    gs_bal_subobj_entry = VALUE #( object    = gs_bal_obj_entry-object
                                   subobject = gc_bal_subobj ).
  ENDMETHOD.

  METHOD class_setup.
    " This is the reason for RISK LEVEL CRITICAL

    INSERT balobj FROM gs_bal_obj_entry.
*    ASSERT sy-subrc = 0.

    INSERT balsub FROM gs_bal_subobj_entry.
*    ASSERT sy-subrc = 0.

    COMMIT WORK AND WAIT.
  ENDMETHOD.

  METHOD class_teardown.
    DELETE balobj FROM gs_bal_obj_entry.
    ASSERT sy-subrc = 0.

    DELETE balsub FROM gs_bal_subobj_entry.
    ASSERT sy-subrc = 0.

    COMMIT WORK AND WAIT.
  ENDMETHOD.

  METHOD setup.
    mo_logger = NEW zcl_alog_bal_logger(
      iv_object    = gc_bal_obj
      iv_subobject = gc_bal_subobj
    ).
  ENDMETHOD.

  METHOD teardown.
    FREE mo_logger.
  ENDMETHOD.

  METHOD test_entry.
    DATA: lt_test TYPE gty_test_tab.

    DO 400 TIMES.
      APPEND VALUE #( LET type = ztcl_alog_test_utl=>get_random_log_type( ) IN
                      text  = ztcl_alog_test_utl=>get_random_log_text( )
                      type  = type
                      msgty = type->mv_message_type
                    ) TO lt_test.
    ENDDO.

    LOOP AT lt_test ASSIGNING FIELD-SYMBOL(<ls_test>).
      mo_logger->entry( iv_text = <ls_test>-text io_type = <ls_test>-type ).
      FREE <ls_test>-type.
    ENDLOOP.

    DATA(lt_read_messages) = get_bal_entries( mo_logger->get_log_handle( ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = lt_test
      act = CORRESPONDING gty_test_tab( lt_read_messages MAPPING text  = msg_txt
                                                                 msgty = msgty )
    ).
  ENDMETHOD.

  METHOD test_exception.
    DATA(lx_ex) = NEW zcx_alog_already_attached( ).
    mo_logger->exception( lx_ex ).
    DATA(lt_entries) = get_bal_entries( mo_logger->get_log_handle( ) ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_entries ) ).
    cl_abap_unit_assert=>assert_equals(
      msg = 'Exception not logged properly'
      exp = VALUE gty_test( text  = lx_ex->get_text( )
                            msgty = zcl_alog_entry_type=>go_error->mv_message_type )
      act = CORRESPONDING gty_test( lt_entries[ 1 ] MAPPING text  = msg_txt
                                                            msgty = msgty )
    ).
  ENDMETHOD.

  METHOD test_save.
    CONSTANTS: lc_text TYPE string VALUE `Test message that is saved`.

    DATA(ls_test_entry) = VALUE gty_test( text = lc_text msgty = 'I' ).

    mo_logger->info( ls_test_entry-text ).
    mo_logger->save( ).

    " Remove the saved log from memory to make sure it was persisted
    CALL FUNCTION 'BAL_LOG_REFRESH'
      EXPORTING
        i_log_handle  = mo_logger->get_log_handle( )
      EXCEPTIONS
        log_not_found = 1
        OTHERS        = 2 ##FM_SUBRC_OK.
    cl_abap_unit_assert=>assert_subrc( msg = 'Log not found' ).

    " Load log from database
    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_handle     = VALUE bal_t_logh( ( mo_logger->get_log_handle( ) ) )
      EXCEPTIONS
        no_logs_specified  = 1
        log_not_found      = 2
        log_already_loaded = 3
        OTHERS             = 4 ##FM_SUBRC_OK.
    cl_abap_unit_assert=>assert_subrc( msg = 'Loading log from db failed' ).

    " Retrieve log from memory
    DATA(lt_entries) = get_bal_entries( mo_logger->get_log_handle( ) ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_entries ) ).
    cl_abap_unit_assert=>assert_equals(
      msg = 'Log entry not saved properly'
      exp = ls_test_entry
      act = CORRESPONDING gty_test( lt_entries[ 1 ] MAPPING text  = msg_txt
                                                            msgty = msgty )
    ).
  ENDMETHOD.

  METHOD get_bal_entries.
    CALL FUNCTION 'BAL_LOG_READ'
      EXPORTING
        i_log_handle  = iv_handle
        i_read_texts  = abap_true
      IMPORTING
        et_msg        = rt_entries
      EXCEPTIONS
        log_not_found = 1
        OTHERS        = 2 ##FM_SUBRC_OK.

    cl_abap_unit_assert=>assert_subrc( msg = 'Log not found' ).
  ENDMETHOD.
ENDCLASS.
