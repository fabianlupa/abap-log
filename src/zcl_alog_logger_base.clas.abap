"! Logger base class
class ZCL_ALOG_LOGGER_BASE definition
  public
  abstract
  create public .

public section.

  interfaces ZIF_ALOG_LOGGER
      abstract methods ENTRY .

  aliases DEBUG
    for ZIF_ALOG_LOGGER~DEBUG .
  aliases ENTRY
    for ZIF_ALOG_LOGGER~ENTRY .
  aliases ERROR
    for ZIF_ALOG_LOGGER~ERROR .
  aliases EXCEPTION
    for ZIF_ALOG_LOGGER~EXCEPTION .
  aliases INFO
    for ZIF_ALOG_LOGGER~INFO .
  aliases WARNING
    for ZIF_ALOG_LOGGER~WARNING .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ALOG_LOGGER_BASE IMPLEMENTATION.


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
