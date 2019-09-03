REPORT zalog_example_prog_rep_logger.

INITIALIZATION.
  DATA(go_logger) = NEW zcl_alog_progress_rep_logger( ).

  go_logger->set_total_entries( 10 ).

  DO 10 TIMES.
    go_logger->set_actual_entries( sy-index ).
    go_logger->info( |Test message { sy-index }| ) ##NO_TEXT.

    CALL FUNCTION 'RZL_SLEEP'
      EXPORTING
        seconds = 2
      EXCEPTIONS
        OTHERS  = 0.
  ENDDO.
