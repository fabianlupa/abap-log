"! Logger base class
CLASS zcl_alog_logger_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_alog_logger,
      zif_alog_attachable.
    ALIASES:
      debug FOR zif_alog_logger~debug,
      entry FOR zif_alog_logger~entry,
      error FOR zif_alog_logger~error,
      exception FOR zif_alog_logger~exception,
      info FOR zif_alog_logger~info,
      warning FOR zif_alog_logger~warning,
      attach FOR zif_alog_attachable~attach,
      detach FOR zif_alog_attachable~detach.
  PROTECTED SECTION.
    METHODS:
      "! Inform attached loggers
      "! @parameter iv_text | Message text
      "! @parameter io_type | Message type
      "! @raising zcx_alog_logging_failed | Logging failed
      "! @raising zcx_alog_call_error | io_type cannot be null or is unsupported
      inform_attached_loggers IMPORTING iv_text TYPE csequence
                                        io_type TYPE REF TO zcl_alog_entry_type
                              RAISING   zcx_alog_logging_failed
                                        zcx_alog_call_error.
    DATA:
      mt_attached_loggers TYPE HASHED TABLE OF REF TO zif_alog_logger WITH UNIQUE KEY table_line.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_alog_logger_base IMPLEMENTATION.
  METHOD zif_alog_logger~entry.
    inform_attached_loggers( iv_text = iv_text io_type = io_type ).
  ENDMETHOD.

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

  METHOD zif_alog_attachable~attach.
    TRY.
        INSERT ii_logger INTO TABLE mt_attached_loggers.
        ASSERT sy-subrc = 0.
      CATCH cx_sy_itab_duplicate_key INTO DATA(lx_ex).
        RAISE EXCEPTION TYPE zcx_alog_already_attached
          EXPORTING
            ix_previous = lx_ex.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_alog_attachable~detach.
    DELETE TABLE mt_attached_loggers WITH TABLE KEY table_line = ii_logger.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_alog_not_attached.
    ENDIF.
  ENDMETHOD.

  METHOD inform_attached_loggers.
    LOOP AT mt_attached_loggers ASSIGNING FIELD-SYMBOL(<li_logger>).
      <li_logger>->entry( iv_text = iv_text io_type = io_type ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
