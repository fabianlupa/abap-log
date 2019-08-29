REPORT zalog_example_prog_rep_logger.

INITIALIZATION.
  DATA(go_logger) = NEW zcl_alog_progress_rep_logger( ).

* EP Set Total Entries
  go_logger->set_TOTAL_ENTRIES( 10 ).

  DO 10 TIMES.
*   EP Set Actual Entries
    go_logger->set_actual_ENTRIES( sy-index ).
    go_logger->info( |Test message { sy-index }| ) ##NO_TEXT.

    CALL FUNCTION 'RZL_SLEEP'
      EXPORTING
        seconds = 2
      EXCEPTIONS
        OTHERS  = 0.
  ENDDO.
