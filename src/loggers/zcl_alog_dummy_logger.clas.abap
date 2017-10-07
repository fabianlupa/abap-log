"! Dummy logger
"! <p>
"! The main use of the class is to use <em>ZIF_ALOG_ATTACHABLE</em> to attach loggers to it that
"! actually to something.
"! </p>
CLASS zcl_alog_dummy_logger DEFINITION
  PUBLIC
  INHERITING FROM zcl_alog_msg_logger_base
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS:
      entry_internal REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_alog_dummy_logger IMPLEMENTATION.
  METHOD entry_internal ##NEEDED.
    " Doing nothing here...
  ENDMETHOD.
ENDCLASS.
