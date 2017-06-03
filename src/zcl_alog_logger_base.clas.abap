"! Logger base class
"! <p>
"! Inherit from this class and implement <em>entry_internal</em> for the logging logic.
"! </p>
CLASS zcl_alog_logger_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_alog_logger FINAL METHODS entry,
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
    METHODS:
      constructor,
      "! Sets the minimal log level
      "! <p>
      "! If a new log entry's entry type does not have at least the priority of the entry type set
      "! by this method the new entry will not be logged by this logger. It will however be
      "! propagated to all attached loggers that will evaluate the relevance of the new log entry
      "! based on their configured minimal log level.
      "! </p>
      "! @parameter io_type | Entry type with the new minimal priority
      "! @raising zcx_alog_argument_null |
      set_minimal_log_level IMPORTING io_type TYPE REF TO zcl_alog_entry_type
                            RAISING   zcx_alog_argument_null,
      "! Gets the minimal log level
      "! @parameter ro_type | Entry type with the current minimal priority
      get_minimal_log_level RETURNING VALUE(ro_type) TYPE REF TO zcl_alog_entry_type.
  PROTECTED SECTION.
    METHODS:
      "! Internal implementation of <em>zif_alog_logger-&gtentry_msg( )</em> to do the logging
      "! <p>
      "! Implement in non abstract subclasses.
      "! </p>
      "! @parameter iv_text | Message text
      "! @parameter io_type | Type of the message
      "! @raising zcx_alog_logging_failed | Logging failed
      "! @raising zcx_alog_unsupported_msgty | Message type unsupported
      entry_internal ABSTRACT IMPORTING iv_text TYPE csequence
                                        io_type TYPE REF TO zcl_alog_entry_type
                              RAISING   zcx_alog_logging_failed
                                        zcx_alog_unsupported_msgty,
      "! Inform attached loggers
      "! @parameter iv_text | Message text
      "! @parameter io_type | Message type
      "! @raising zcx_alog_logging_failed | Logging failed
      "! @raising zcx_alog_unsupported_msgty | Unsupported message type
      "! @raising zcx_alog_argument_null | io_type cannot be null
      inform_attached_loggers IMPORTING iv_text TYPE csequence
                                        io_type TYPE REF TO zcl_alog_entry_type
                              RAISING   zcx_alog_logging_failed
                                        zcx_alog_unsupported_msgty
                                        zcx_alog_argument_null.
    DATA:
      mt_attached_loggers TYPE HASHED TABLE OF REF TO zif_alog_logger WITH UNIQUE KEY table_line.
  PRIVATE SECTION.
    DATA:
      mo_minimal_log_level TYPE REF TO zcl_alog_entry_type.
ENDCLASS.



CLASS zcl_alog_logger_base IMPLEMENTATION.
  METHOD constructor.
    mo_minimal_log_level = zcl_alog_entry_type=>go_debug.
  ENDMETHOD.

  METHOD zif_alog_logger~entry.
    IF io_type IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_alog_argument_null
        EXPORTING
          iv_variable_name = 'IO_TYPE'.
    ENDIF.

    inform_attached_loggers( iv_text = iv_text io_type = io_type ).

    IF mo_minimal_log_level->compare_priority_to( io_type ) <= 0.
      entry_internal( iv_text = iv_text io_type = io_type ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_alog_logger~debug.
    entry( iv_text = iv_text io_type = zcl_alog_entry_type=>go_debug ).
  ENDMETHOD.

  METHOD zif_alog_logger~error.
    entry( iv_text = iv_text io_type = zcl_alog_entry_type=>go_error ).
  ENDMETHOD.

  METHOD zif_alog_logger~exception.
    entry( iv_text = ix_ex->get_text( ) io_type = zcl_alog_entry_type=>go_error ).
  ENDMETHOD.

  METHOD zif_alog_logger~info.
    entry( iv_text = iv_text io_type = zcl_alog_entry_type=>go_info ).
  ENDMETHOD.

  METHOD zif_alog_logger~warning.
    entry( iv_text = iv_text io_type = zcl_alog_entry_type=>go_warning ).
  ENDMETHOD.

  METHOD zif_alog_attachable~attach.
    IF ii_logger IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_alog_argument_null
        EXPORTING
          iv_variable_name = 'II_LOGGER'.
    ENDIF.

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
    IF ii_logger IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_alog_argument_null
        EXPORTING
          iv_variable_name = 'II_LOGGER'.
    ENDIF.

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

  METHOD get_minimal_log_level.
    ro_type = mo_minimal_log_level.
  ENDMETHOD.

  METHOD set_minimal_log_level.
    IF io_type IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_alog_argument_null
        EXPORTING
          iv_variable_name = 'IO_TYPE'.
    ENDIF.

    mo_minimal_log_level = io_type.
  ENDMETHOD.
ENDCLASS.
