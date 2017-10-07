"! Statically accessible logger
"! <p>
"! This is a static facade interface for <em>ZIF_ALOG_LOGGER</em> and <em>ZIF_ALOG_MSG_LOGGER</em>.
"! The logger used is specified by using <em>ZCL_ALOG_STATIC_CONFIGURATION</em>.
"! </p>
CLASS zcl_alog_static_logger DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! Get a (prefixed) logger instance
      "! @parameter iv_prefix | Prefix to use (e. g. class name)
      "! @parameter ri_logger | Logger instance
      get_logger IMPORTING iv_prefix        TYPE csequence OPTIONAL
                 RETURNING VALUE(ri_logger) TYPE REF TO zif_alog_logger,
      "! Get a (prefixed) logger instance
      "! @parameter ig_any | Variable whose relative type name will be the logger prefix
      "! @parameter ri_logger | Logger instance
      get_logger_for_any IMPORTING ig_any           TYPE any
                         RETURNING VALUE(ri_logger) TYPE REF TO zif_alog_logger,
      "! Get a (prefixed) message logger instance
      "! @parameter iv_prefix | Prefix to use (e. g. class name)
      "! @parameter ri_msg_logger | Logger instance
      get_msg_logger IMPORTING iv_prefix            TYPE csequence OPTIONAL
                     RETURNING VALUE(ri_msg_logger) TYPE REF TO zif_alog_msg_logger,
      "! Get a prefixed logger for a variable
      "! @parameter ig_any | Variable whose relative type name will be the logger prefix
      "! @parameter ri_msg_logger | Message logger instance
      get_msg_logger_for_any IMPORTING ig_any               TYPE any
                             RETURNING VALUE(ri_msg_logger) TYPE REF TO zif_alog_msg_logger,
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
                          zcx_alog_argument_null,
      "! Log an informational message using message classes
      "! @parameter iv_msgid | Message id
      "! @parameter iv_msgno | Message number
      "! @parameter iv_msgv1 | Message variable 1
      "! @parameter iv_msgv2 | Message variable 2
      "! @parameter iv_msgv3 | Message variable 3
      "! @parameter iv_msgv4 | Message variable 4
      "! @raising zcx_alog_logging_failed | Logging failed
      "! @raising zcx_alog_unsupported_operation | Configured logger does not support message
      "!                                           classes
      info_msg IMPORTING VALUE(iv_msgid) TYPE syst_msgid DEFAULT sy-msgid
                         VALUE(iv_msgno) TYPE syst_msgno DEFAULT sy-msgno
                         VALUE(iv_msgv1) TYPE syst_msgv DEFAULT sy-msgv1
                         VALUE(iv_msgv2) TYPE syst_msgv DEFAULT sy-msgv2
                         VALUE(iv_msgv3) TYPE syst_msgv DEFAULT sy-msgv3
                         VALUE(iv_msgv4) TYPE syst_msgv DEFAULT sy-msgv4
               RAISING   zcx_alog_logging_failed
                         zcx_alog_unsupported_operation,
      "! Log a warning using message classes
      "! @parameter iv_msgid | Message id
      "! @parameter iv_msgno | Message number
      "! @parameter iv_msgv1 | Message variable 1
      "! @parameter iv_msgv2 | Message variable 2
      "! @parameter iv_msgv3 | Message variable 3
      "! @parameter iv_msgv4 | Message variable 4
      "! @raising zcx_alog_logging_failed | Logging failed
      "! @raising zcx_alog_unsupported_operation | Configured logger does not support message
      "!                                           classes
      warning_msg IMPORTING VALUE(iv_msgid) TYPE syst_msgid DEFAULT sy-msgid
                            VALUE(iv_msgno) TYPE syst_msgno DEFAULT sy-msgno
                            VALUE(iv_msgv1) TYPE syst_msgv DEFAULT sy-msgv1
                            VALUE(iv_msgv2) TYPE syst_msgv DEFAULT sy-msgv2
                            VALUE(iv_msgv3) TYPE syst_msgv DEFAULT sy-msgv3
                            VALUE(iv_msgv4) TYPE syst_msgv DEFAULT sy-msgv4
                  RAISING   zcx_alog_logging_failed
                            zcx_alog_unsupported_operation,
      "! Log an error using message classes
      "! @parameter iv_msgid | Message id
      "! @parameter iv_msgno | Message number
      "! @parameter iv_msgv1 | Message variable 1
      "! @parameter iv_msgv2 | Message variable 2
      "! @parameter iv_msgv3 | Message variable 3
      "! @parameter iv_msgv4 | Message variable 4
      "! @raising zcx_alog_logging_failed | Logging failed
      "! @raising zcx_alog_unsupported_operation | Configured logger does not support message
      "!                                           classes
      error_msg IMPORTING VALUE(iv_msgid) TYPE syst_msgid DEFAULT sy-msgid
                          VALUE(iv_msgno) TYPE syst_msgno DEFAULT sy-msgno
                          VALUE(iv_msgv1) TYPE syst_msgv DEFAULT sy-msgv1
                          VALUE(iv_msgv2) TYPE syst_msgv DEFAULT sy-msgv2
                          VALUE(iv_msgv3) TYPE syst_msgv DEFAULT sy-msgv3
                          VALUE(iv_msgv4) TYPE syst_msgv DEFAULT sy-msgv4
                RAISING   zcx_alog_logging_failed
                          zcx_alog_unsupported_operation,
      "! Log a debug message using message classes
      "! @parameter iv_msgid | Message id
      "! @parameter iv_msgno | Message number
      "! @parameter iv_msgv1 | Message variable 1
      "! @parameter iv_msgv2 | Message variable 2
      "! @parameter iv_msgv3 | Message variable 3
      "! @parameter iv_msgv4 | Message variable 4
      "! @raising zcx_alog_logging_failed | Logging failed
      "! @raising zcx_alog_unsupported_operation | Configured logger does not support message
      "!                                           classes
      debug_msg IMPORTING VALUE(iv_msgid) TYPE syst_msgid DEFAULT sy-msgid
                          VALUE(iv_msgno) TYPE syst_msgno DEFAULT sy-msgno
                          VALUE(iv_msgv1) TYPE syst_msgv DEFAULT sy-msgv1
                          VALUE(iv_msgv2) TYPE syst_msgv DEFAULT sy-msgv2
                          VALUE(iv_msgv3) TYPE syst_msgv DEFAULT sy-msgv3
                          VALUE(iv_msgv4) TYPE syst_msgv DEFAULT sy-msgv4
                RAISING   zcx_alog_logging_failed
                          zcx_alog_unsupported_operation,
      "! Log a message of the specified type using message classes
      "! @parameter iv_msgid | Message id
      "! @parameter iv_msgno | Message number
      "! @parameter iv_msgty | Message type
      "! @parameter iv_msgv1 | Message variable 1
      "! @parameter iv_msgv2 | Message variable 2
      "! @parameter iv_msgv3 | Message variable 3
      "! @parameter iv_msgv4 | Message variable 4
      "! @raising zcx_alog_logging_failed | Logging failed
      "! @raising zcx_alog_unsupported_msgty | Message type unsupported
      "! @raising zcx_alog_unsupported_operation | Configured logger does not support message
      "!                                           classes
      entry_msg IMPORTING VALUE(iv_msgid) TYPE syst_msgid DEFAULT sy-msgid
                          VALUE(iv_msgno) TYPE syst_msgno DEFAULT sy-msgno
                          VALUE(iv_msgty) TYPE syst_msgty DEFAULT sy-msgty
                          VALUE(iv_msgv1) TYPE syst_msgv DEFAULT sy-msgv1
                          VALUE(iv_msgv2) TYPE syst_msgv DEFAULT sy-msgv2
                          VALUE(iv_msgv3) TYPE syst_msgv DEFAULT sy-msgv3
                          VALUE(iv_msgv4) TYPE syst_msgv DEFAULT sy-msgv4
                RAISING   zcx_alog_logging_failed
                          zcx_alog_unsupported_msgty
                          zcx_alog_unsupported_operation.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS:
      get_logger_internal RETURNING VALUE(ri_logger) TYPE REF TO zif_alog_logger,
      get_msg_logger_internal RETURNING VALUE(ri_msg_logger) TYPE REF TO zif_alog_msg_logger
                              RAISING   zcx_alog_unsupported_operation,
      resolve_type_name IMPORTING ig_any         TYPE any
                        RETURNING VALUE(rv_type) TYPE string.
ENDCLASS.



CLASS zcl_alog_static_logger IMPLEMENTATION.
  METHOD error.
    entry( iv_text = iv_text io_type = zcl_alog_entry_type=>go_error ).
  ENDMETHOD.

  METHOD debug.
    entry( iv_text = iv_text io_type = zcl_alog_entry_type=>go_debug ).
  ENDMETHOD.

  METHOD exception.
    entry( iv_text = ix_ex->get_text( ) io_type = zcl_alog_entry_type=>go_error ).
  ENDMETHOD.

  METHOD info.
    entry( iv_text = iv_text io_type = zcl_alog_entry_type=>go_info ).
  ENDMETHOD.

  METHOD warning.
    entry( iv_text = iv_text io_type = zcl_alog_entry_type=>go_warning ).
  ENDMETHOD.

  METHOD entry.
    get_logger_internal( )->entry( iv_text = iv_text io_type = io_type ).
  ENDMETHOD.

  METHOD entry_msg.
    get_msg_logger_internal( )->entry_msg( iv_msgid = iv_msgid
                                           iv_msgno = iv_msgno
                                           iv_msgty = iv_msgty
                                           iv_msgv1 = iv_msgv1
                                           iv_msgv2 = iv_msgv2
                                           iv_msgv3 = iv_msgv3
                                           iv_msgv4 = iv_msgv4 ).
  ENDMETHOD.

  METHOD debug_msg.
    entry_msg( iv_msgid = iv_msgid
               iv_msgno = iv_msgno
               iv_msgty = 'I'
               iv_msgv1 = iv_msgv1
               iv_msgv2 = iv_msgv2
               iv_msgv3 = iv_msgv3
               iv_msgv4 = iv_msgv4 ).
  ENDMETHOD.

  METHOD error_msg.
    entry_msg( iv_msgid = iv_msgid
               iv_msgno = iv_msgno
               iv_msgty = 'E'
               iv_msgv1 = iv_msgv1
               iv_msgv2 = iv_msgv2
               iv_msgv3 = iv_msgv3
               iv_msgv4 = iv_msgv4 ).
  ENDMETHOD.

  METHOD info_msg.
    entry_msg( iv_msgid = iv_msgid
               iv_msgno = iv_msgno
               iv_msgty = 'I'
               iv_msgv1 = iv_msgv1
               iv_msgv2 = iv_msgv2
               iv_msgv3 = iv_msgv3
               iv_msgv4 = iv_msgv4 ).
  ENDMETHOD.

  METHOD warning_msg.
    entry_msg( iv_msgid = iv_msgid
               iv_msgno = iv_msgno
               iv_msgty = 'W'
               iv_msgv1 = iv_msgv1
               iv_msgv2 = iv_msgv2
               iv_msgv3 = iv_msgv3
               iv_msgv4 = iv_msgv4 ).
  ENDMETHOD.

  METHOD get_msg_logger_internal.
    DATA(li_logger) = get_logger_internal( ).

    IF zcl_alog_static_configuration=>get_active_configuration( )->mv_supports_t100_msg = abap_true.
      ri_msg_logger ?= li_logger.
    ELSE.
      RAISE EXCEPTION TYPE zcx_alog_unsupported_operation
        EXPORTING
          iv_operation = 'message logging' ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD get_logger_internal.
    ri_logger = zcl_alog_static_configuration=>get_active_configuration( )->mi_logger.
  ENDMETHOD.

  METHOD get_logger.
    ri_logger = NEW lcl_logger_proxy( iv_prefix ).
  ENDMETHOD.

  METHOD get_logger_for_any.
    ri_logger = get_logger( resolve_type_name( ig_any ) ).
  ENDMETHOD.

  METHOD get_msg_logger.
    ri_msg_logger = NEW lcl_logger_proxy( iv_prefix ).
  ENDMETHOD.

  METHOD get_msg_logger_for_any.
    ri_msg_logger = get_msg_logger( resolve_type_name( ig_any ) ).
  ENDMETHOD.

  METHOD resolve_type_name.
    DATA(lo_descr) = cl_abap_typedescr=>describe_by_data( ig_any ).

    TRY.
        rv_type = CAST cl_abap_refdescr( lo_descr )->get_referenced_type( )->get_relative_name( ).
      CATCH cx_sy_move_cast_error.
        rv_type = lo_descr->get_relative_name( ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
