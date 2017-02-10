"! Logger base class
CLASS zcl_alog_logger_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_alog_logger ABSTRACT METHODS entry.
    ALIASES:
      debug FOR zif_alog_logger~debug,
      entry FOR zif_alog_logger~entry,
      error FOR zif_alog_logger~error,
      exception FOR zif_alog_logger~exception,
      info FOR zif_alog_logger~info,
      warning FOR zif_alog_logger~warning.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_alog_logger_base IMPLEMENTATION.
  METHOD zif_alog_logger~debug.
    entry( iv_text = iv_text io_type = zcl_alog_entry_type=>go_debug ).
  ENDMETHOD.

  METHOD zif_alog_logger~error.
    entry( iv_text = iv_text io_type = zcl_alog_entry_type=>go_error ).
  ENDMETHOD.

  METHOD zif_alog_logger~exception.
    entry( iv_text = ix_ex->get_text( ) io_type = zcl_alog_entry_type=>go_debug ).
  ENDMETHOD.

  METHOD zif_alog_logger~info.
    entry( iv_text = iv_text io_type = zcl_alog_entry_type=>go_info ).
  ENDMETHOD.

  METHOD zif_alog_logger~warning.
    entry( iv_text = iv_text io_type = zcl_alog_entry_type=>go_warning ).
  ENDMETHOD.
ENDCLASS.
