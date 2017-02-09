"! Logger with message class support
INTERFACE zif_alog_msg_logger PUBLIC.
  INTERFACES:
    zif_alog_logger.
  ALIASES:
    info FOR zif_alog_logger~info,
    warning FOR zif_alog_logger~warning,
    error FOR zif_alog_logger~error,
    debug FOR zif_alog_logger~debug,
    entry FOR zif_alog_logger~entry,
    exception FOR zif_alog_logger~exception.
  METHODS:
    "! Log an informational message using message classes
    "! @parameter iv_msgid | Message id
    "! @parameter iv_msgno | Message number
    "! @parameter iv_msgv1 | Message variable 1
    "! @parameter iv_msgv2 | Message variable 2
    "! @parameter iv_msgv3 | Message variable 3
    "! @parameter iv_msgv4 | Message variable 4
    "! @raising zcx_alog_logging_failed | Logging failed
    info_msg IMPORTING iv_msgid TYPE syst_msgid DEFAULT sy-msgid
                       iv_msgno TYPE syst_msgno DEFAULT sy-msgno
                       iv_msgv1 TYPE syst_msgv DEFAULT sy-msgv1
                       iv_msgv2 TYPE syst_msgv DEFAULT sy-msgv2
                       iv_msgv3 TYPE syst_msgv DEFAULT sy-msgv3
                       iv_msgv4 TYPE syst_msgv DEFAULT sy-msgv4
             RAISING   zcx_alog_logging_failed,
    "! Log a warning using message classes
    "! @parameter iv_msgid | Message id
    "! @parameter iv_msgno | Message number
    "! @parameter iv_msgv1 | Message variable 1
    "! @parameter iv_msgv2 | Message variable 2
    "! @parameter iv_msgv3 | Message variable 3
    "! @parameter iv_msgv4 | Message variable 4
    "! @raising zcx_alog_logging_failed | Logging failed
    warning_msg IMPORTING iv_msgid TYPE syst_msgid DEFAULT sy-msgid
                          iv_msgno TYPE syst_msgno DEFAULT sy-msgno
                          iv_msgv1 TYPE syst_msgv DEFAULT sy-msgv1
                          iv_msgv2 TYPE syst_msgv DEFAULT sy-msgv2
                          iv_msgv3 TYPE syst_msgv DEFAULT sy-msgv3
                          iv_msgv4 TYPE syst_msgv DEFAULT sy-msgv4
                RAISING   zcx_alog_logging_failed,
    "! Log an error using message classes
    "! @parameter iv_msgid | Message id
    "! @parameter iv_msgno | Message number
    "! @parameter iv_msgv1 | Message variable 1
    "! @parameter iv_msgv2 | Message variable 2
    "! @parameter iv_msgv3 | Message variable 3
    "! @parameter iv_msgv4 | Message variable 4
    "! @raising zcx_alog_logging_failed | Logging failed
    error_msg IMPORTING iv_msgid TYPE syst_msgid DEFAULT sy-msgid
                        iv_msgno TYPE syst_msgno DEFAULT sy-msgno
                        iv_msgv1 TYPE syst_msgv DEFAULT sy-msgv1
                        iv_msgv2 TYPE syst_msgv DEFAULT sy-msgv2
                        iv_msgv3 TYPE syst_msgv DEFAULT sy-msgv3
                        iv_msgv4 TYPE syst_msgv DEFAULT sy-msgv4
              RAISING   zcx_alog_logging_failed,
    "! Log a debug message using message classes
    "! @parameter iv_msgid | Message id
    "! @parameter iv_msgno | Message number
    "! @parameter iv_msgv1 | Message variable 1
    "! @parameter iv_msgv2 | Message variable 2
    "! @parameter iv_msgv3 | Message variable 3
    "! @parameter iv_msgv4 | Message variable 4
    "! @raising zcx_alog_logging_failed | Logging failed
    debug_msg IMPORTING iv_msgid TYPE syst_msgid DEFAULT sy-msgid
                        iv_msgno TYPE syst_msgno DEFAULT sy-msgno
                        iv_msgv1 TYPE syst_msgv DEFAULT sy-msgv1
                        iv_msgv2 TYPE syst_msgv DEFAULT sy-msgv2
                        iv_msgv3 TYPE syst_msgv DEFAULT sy-msgv3
                        iv_msgv4 TYPE syst_msgv DEFAULT sy-msgv4
              RAISING   zcx_alog_logging_failed,
    "! Log a message of the specified type using message classes
    "! @parameter iv_msgid | Message id
    "! @parameter iv_msgno | Message number
    "! @parameter iv_msgty | Message type
    "! @parameter iv_msgv1 | Message variable 1
    "! @parameter iv_msgv2 | Message variable 2
    "! @parameter iv_msgv3 | Message variable 3
    "! @parameter iv_msgv4 | Message variable 4
    "! @raising zcx_alog_logging_failed | Logging failed
    "! @raising zcx_alog_call_error | Message type unknown
    entry_msg IMPORTING iv_msgid TYPE syst_msgid DEFAULT sy-msgid
                        iv_msgno TYPE syst_msgno DEFAULT sy-msgno
                        iv_msgty TYPE syst_msgty DEFAULT sy-msgty
                        iv_msgv1 TYPE syst_msgv DEFAULT sy-msgv1
                        iv_msgv2 TYPE syst_msgv DEFAULT sy-msgv2
                        iv_msgv3 TYPE syst_msgv DEFAULT sy-msgv3
                        iv_msgv4 TYPE syst_msgv DEFAULT sy-msgv4
              RAISING   zcx_alog_logging_failed
                        zcx_alog_call_error.
ENDINTERFACE.
