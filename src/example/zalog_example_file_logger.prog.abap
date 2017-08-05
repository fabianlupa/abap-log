REPORT zalog_example_file_logger.

PARAMETERS: p_fnam TYPE string LOWER CASE.

CONSTANTS: gc_logpath_tmp TYPE fileintern VALUE 'TMP'.

START-OF-SELECTION.
  DATA(go_logger) = zcl_alog_file_logger=>from_logical_path( iv_logpath  = gc_logpath_tmp
                                                             iv_filename = p_fnam ).

  go_logger->info( `Hello world.` ) ##NO_TEXT.
  go_logger->warning( `Hello darkness my old friend.` ) ##NO_TEXT.
  go_logger->error( `I've come to talk with you again.` ) ##NO_TEXT.
  go_logger->debug( `BEEP BOOP` ) ##NO_TEXT.
  go_logger->exception( NEW zcx_alog_argument_null( ) ).

  CALL FUNCTION 'RZL_SLEEP'.

  MESSAGE s000(zalog) WITH 'Hello from message class' INTO DATA(gv_dummy) ##NEEDED ##NO_TEXT.
  go_logger->info_msg( ).

  go_logger->warning_msg(
    iv_msgid = 'ZALOG'
    iv_msgno = '001'
    iv_msgv1 = 'Hello from message class'
    iv_msgv2 = 'without where used list support...'
  ) ##NO_TEXT.

  MESSAGE 'Use transaction AL11 to display the file in DIR_TEMP.' TYPE 'S' ##NO_TEXT.
