"! Object where loggers can be attached to
INTERFACE zif_alog_attachable PUBLIC.
  METHODS:
    "! Attach a logger to this object
    "! @parameter ii_logger | Logger
    "! @raising zcx_alog_already_attached | The logger is already attached
    attach IMPORTING ii_logger TYPE REF TO zif_alog_logger
           RAISING   zcx_alog_already_attached,
    "! Detach an attached logger from this object
    "! @parameter ii_logger | Logger
    "! @raising zcx_alog_not_attached | Logger is not attached
    detach IMPORTING ii_logger TYPE REF TO zif_alog_logger
           RAISING   zcx_alog_not_attached.
ENDINTERFACE.
