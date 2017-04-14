REPORT zalog_example_dbtab_logger.

**********************************************************************
* To use this example a number range might need to be added in number*
* range object ZALOG_DBL with id 01                                  *
**********************************************************************

CLASS lcl_db_logger DEFINITION INHERITING FROM zcl_alog_dbtab_logger.
  PUBLIC SECTION.
    METHODS:
      "! @parameter iv_secondary_conn_key | Key for secondary database connection
      "! @parameter iv_use_secondary_connection | Use secondary database connection
      "! @parameter iv_always_implicit_commit | Always commit after adding a log entry
      constructor IMPORTING iv_secondary_conn_key       TYPE string OPTIONAL
                            iv_use_secondary_connection TYPE abap_bool DEFAULT abap_false
                            iv_always_implicit_commit   TYPE abap_bool DEFAULT abap_false.
  PROTECTED SECTION.
    METHODS:
      get_next_key REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_db_logger IMPLEMENTATION.
  METHOD constructor.
    super->constructor(
      iv_tabname                  = 'ZALOG_TADBLOGEXA'
      is_mapping                  = VALUE #( text_target      = 'TEXT'
                                             type_target      = 'MSG_TYPE'
                                             timestamp_target = 'TIMESTAMP'
                                             username_target  = 'USERNAME' )
      it_key_components           = VALUE #( ( 'ENTRY_NUMBER' ) )
      iv_secondary_conn_key       = iv_secondary_conn_key
      iv_use_secondary_connection = iv_use_secondary_connection
      iv_always_implicit_commit   = iv_always_implicit_commit
    ).
  ENDMETHOD.

  METHOD get_next_key.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr        = '01'
        object             = 'ZALOG_DBL'
      IMPORTING
        number             = rv_key
      EXCEPTIONS
        interval_not_found = 1.
    IF sy-subrc = 1.
      MESSAGE `Please create number range id 01 in transaction SNUM for object` &&
              `ZALOG_DBL to use this example` TYPE 'X' ##NO_TEXT.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.
  DATA: gt_result TYPE STANDARD TABLE OF zalog_tadblogexa.

  DATA(go_logger) = NEW lcl_db_logger( ).

  go_logger->info( `Hello world.` ) ##NO_TEXT.
  go_logger->warning( `Hello darkness my old friend.` ) ##NO_TEXT.
  go_logger->error( `I've come to talk with you again.` ) ##NO_TEXT.
  go_logger->debug( `BEEP BOOP` ) ##NO_TEXT.
  go_logger->exception( NEW zcx_alog_argument_null( ) ).

  MESSAGE s000(zalog) WITH 'Hello from message class' INTO DATA(gv_dummy) ##NEEDED ##NO_TEXT.
  go_logger->info_msg( ).

  go_logger->warning_msg(
    iv_msgid = 'ZALOG'
    iv_msgno = '001'
    iv_msgv1 = 'Hello from message class'
    iv_msgv2 = 'without where used list support...'
  ) ##NO_TEXT.

  COMMIT WORK.

  SELECT * INTO TABLE @gt_result FROM zalog_tadblogexa.

  cl_salv_table=>factory(
    IMPORTING
      r_salv_table   = DATA(go_alv)
    CHANGING
      t_table        = gt_result
  ).

  go_alv->get_columns( )->set_optimize( ).
  go_alv->display( ).
