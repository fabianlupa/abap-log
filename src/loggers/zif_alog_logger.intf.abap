"! Logger
INTERFACE zif_alog_logger PUBLIC.
  METHODS:
    "! Log an informational message
    "! @parameter iv_text | Message text
    "! @raising zcx_alog_logging_failed | Logging failed
    info IMPORTING iv_text TYPE csequence
         RAISING   zcx_alog_logging_failed,
    "! Log a warning
    "! @parameter iv_text | Message text
    "! @raising zcx_alog_logging_failed | Logging failed
    warning IMPORTING iv_text TYPE csequence
            RAISING   zcx_alog_logging_failed,
    "! Log an error
    "! @parameter iv_text | Message text
    "! @raising zcx_alog_logging_failed | Logging failed
    error IMPORTING iv_text TYPE csequence
          RAISING   zcx_alog_logging_failed,
    "! Log a debug message
    "! @parameter iv_text | Message text
    "! @raising zcx_alog_logging_failed | Logging failed
    debug IMPORTING iv_text TYPE csequence
          RAISING   zcx_alog_logging_failed,
    "! Log a message of the specified type
    "! @parameter iv_text | Message text
    "! @parameter io_type | Type of the message
    "! @raising zcx_alog_logging_failed | Logging failed
    "! @raising zcx_alog_unsupported_msgty | Unsupported message type
    "! @raising zcx_alog_argument_null | io_type cannot be null
    entry IMPORTING iv_text TYPE csequence
                    io_type TYPE REF TO zcl_alog_entry_type
          RAISING   zcx_alog_logging_failed
                    zcx_alog_unsupported_msgty
                    zcx_alog_argument_null,
    "! Log an exception (as an error)
    "! @parameter ix_ex | The exception to log
    "! @raising zcx_alog_logging_failed | Logging failed
    "! @raising zcx_alog_argument_null | ix_ex cannot be null
    exception IMPORTING ix_ex TYPE REF TO cx_root
              RAISING   zcx_alog_logging_failed
                        zcx_alog_argument_null.
ENDINTERFACE.
