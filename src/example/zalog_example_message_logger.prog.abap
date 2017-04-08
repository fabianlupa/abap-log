REPORT zalog_example_message_logger.

PARAMETERS: p_dummy ##SEL_WRONG.

INITIALIZATION.
  DATA(gt_excl_ucomms) = VALUE syucomm_t( ( 'ONLI' ) ( 'PRIN' ) ).
  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = gt_excl_ucomms.

START-OF-SELECTION.
  IF sy-batch IS INITIAL.
    MESSAGE 'Program needs to be executed in background' TYPE 'E' ##NO_TEXT.
  ENDIF.

  DATA(go_logger) = NEW zcl_alog_message_logger( ).
  go_logger->info( 'Logging stuff in background' ) ##NO_TEXT.
  go_logger->error( 'Oh no, an error' ) ##NO_TEXT.
  go_logger->exception( NEW zcx_alog_illegal_argument( 'Argument error' ) ) ##NO_TEXT.
  MESSAGE w000(zalog) WITH 'Message class message' INTO DATA(gv_dummy) ##NEEDED ##NO_TEXT.
  go_logger->entry_msg( ).
