"! Object where loggers can be attached to
INTERFACE zif_alog_attachable PUBLIC.
  METHODS:
    "! Attach a logger to this object
    "! @parameter ii_logger | Logger
    "! @raising zcx_alog_already_attached | The logger is already attached
    "! @raising zcx_alog_argument_null | ii_logger cannot be null
    attach IMPORTING ii_logger TYPE REF TO zif_alog_logger
           RAISING   zcx_alog_already_attached
                     zcx_alog_argument_null,
    "! Detach an attached logger from this object
    "! @parameter ii_logger | Logger
    "! @raising zcx_alog_not_attached | Logger is not attached
    "! @raising zcx_alog_argument_null | ii_logger cannot be null
    detach IMPORTING ii_logger TYPE REF TO zif_alog_logger
           RAISING   zcx_alog_not_attached
                     zcx_alog_argument_null.
ENDINTERFACE.
