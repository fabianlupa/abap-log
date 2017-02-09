"! Logger
INTERFACE zif_alog_logger PUBLIC.
  METHODS:
    "! Log an informational message
    "! @parameter iv_text | Message text
    info IMPORTING iv_text TYPE csequence,
    "! Log a warning
    "! @parameter iv_text | Message text
    warning IMPORTING iv_text TYPE csequence,
    "! Log an error
    "! @parameter iv_text | Message text
    error IMPORTING iv_text TYPE csequence,
    "! Log a debug message
    "! @parameter iv_text | Message text
    debug IMPORTING iv_text TYPE csequence,
    "! Log a message of the specified type
    "! @parameter iv_text | Message text
    "! @parameter io_type | Type of the message
    entry IMPORTING iv_text TYPE csequence
                    io_type TYPE REF TO zcl_alog_entry_type,
    "! Log an exception (as an error)
    "! @parameter ix_ex | The exception to log
    exception IMPORTING ix_ex TYPE REF TO cx_root.
ENDINTERFACE.
