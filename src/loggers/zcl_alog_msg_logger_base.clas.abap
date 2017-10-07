"! Message logger base class
"! <p>
"! Inherit from this class and implement <em>entry_internal</em> and ideally overwrite
"! <em>entry_msg_internal</em> for the logging logic.
"! </p>
CLASS zcl_alog_msg_logger_base DEFINITION
  PUBLIC
  ABSTRACT
  INHERITING FROM zcl_alog_logger_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_alog_msg_logger FINAL METHODS entry_msg.
    ALIASES:
      debug_msg FOR zif_alog_msg_logger~debug_msg,
      entry_msg FOR zif_alog_msg_logger~entry_msg,
      error_msg FOR zif_alog_msg_logger~error_msg,
      info_msg FOR zif_alog_msg_logger~info_msg,
      warning_msg FOR zif_alog_msg_logger~warning_msg.
  PROTECTED SECTION.
    METHODS:
      inform_attached_loggers REDEFINITION,
      "! Inform attached loggers using a message class message
      "! @parameter iv_msgid | Message id
      "! @parameter iv_msgno | Message number
      "! @parameter iv_msgty | Message type
      "! @parameter iv_msgv1 | Message variable 1
      "! @parameter iv_msgv2 | Message variable 2
      "! @parameter iv_msgv3 | Message variable 3
      "! @parameter iv_msgv4 | Message variable 4
      "! @raising zcx_alog_logging_failed | Logging failed
      "! @raising zcx_alog_unsupported_msgty | Message type unsupported
      inform_attached_loggers_msg IMPORTING VALUE(iv_msgid) TYPE syst_msgid DEFAULT sy-msgid
                                            VALUE(iv_msgno) TYPE syst_msgno DEFAULT sy-msgno
                                            VALUE(iv_msgty) TYPE syst_msgty DEFAULT sy-msgty
                                            VALUE(iv_msgv1) TYPE syst_msgv DEFAULT sy-msgv1
                                            VALUE(iv_msgv2) TYPE syst_msgv DEFAULT sy-msgv2
                                            VALUE(iv_msgv3) TYPE syst_msgv DEFAULT sy-msgv3
                                            VALUE(iv_msgv4) TYPE syst_msgv DEFAULT sy-msgv4
                                  RAISING   zcx_alog_logging_failed
                                            zcx_alog_unsupported_msgty,
      "! Internal implementation of <em>zif_alog_msg_logger-&gtentry_msg( )</em> to do the logging
      "! <p>
      "! Default implementation redirects the message to <em>entry( )</em>, overwrite in subclasses
      "! to provide message class specific logic.
      "! </p>
      "! @parameter iv_msgid | Message id
      "! @parameter iv_msgno | Message number
      "! @parameter iv_msgty | Message type
      "! @parameter iv_msgv1 | Message variable 1
      "! @parameter iv_msgv2 | Message variable 2
      "! @parameter iv_msgv3 | Message variable 3
      "! @parameter iv_msgv4 | Message variable 4
      "! @raising zcx_alog_logging_failed | Logging failed
      "! @raising zcx_alog_unsupported_msgty | Message type unsupported
      entry_msg_internal IMPORTING VALUE(iv_msgid) TYPE syst_msgid DEFAULT sy-msgid
                                   VALUE(iv_msgno) TYPE syst_msgno DEFAULT sy-msgno
                                   VALUE(iv_msgty) TYPE syst_msgty DEFAULT sy-msgty
                                   VALUE(iv_msgv1) TYPE syst_msgv DEFAULT sy-msgv1
                                   VALUE(iv_msgv2) TYPE syst_msgv DEFAULT sy-msgv2
                                   VALUE(iv_msgv3) TYPE syst_msgv DEFAULT sy-msgv3
                                   VALUE(iv_msgv4) TYPE syst_msgv DEFAULT sy-msgv4
                         RAISING   zcx_alog_logging_failed
                                   zcx_alog_unsupported_msgty.
  PRIVATE SECTION.
    DATA:
      mv_block_entry_inform TYPE abap_bool.
ENDCLASS.



CLASS zcl_alog_msg_logger_base IMPLEMENTATION.
  METHOD zif_alog_msg_logger~entry_msg.
    inform_attached_loggers_msg( iv_msgid = iv_msgid
                                 iv_msgno = iv_msgno
                                 iv_msgty = iv_msgty
                                 iv_msgv1 = iv_msgv1
                                 iv_msgv2 = iv_msgv2
                                 iv_msgv3 = iv_msgv3
                                 iv_msgv4 = iv_msgv4 ).

    IF get_minimal_log_level( )->compare_priority_to(
         zcl_alog_entry_type=>from_msgty( iv_msgty )
       ) <= 0.

      entry_msg_internal( iv_msgid = iv_msgid
                          iv_msgno = iv_msgno
                          iv_msgty = iv_msgty
                          iv_msgv1 = iv_msgv1
                          iv_msgv2 = iv_msgv2
                          iv_msgv3 = iv_msgv3
                          iv_msgv4 = iv_msgv4 ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_alog_msg_logger~debug_msg.
    entry_msg( iv_msgid = iv_msgid
               iv_msgno = iv_msgno
               iv_msgty = 'I'
               iv_msgv1 = iv_msgv1
               iv_msgv2 = iv_msgv2
               iv_msgv3 = iv_msgv3
               iv_msgv4 = iv_msgv4 ).
  ENDMETHOD.

  METHOD zif_alog_msg_logger~error_msg.
    entry_msg( iv_msgid = iv_msgid
               iv_msgno = iv_msgno
               iv_msgty = 'E'
               iv_msgv1 = iv_msgv1
               iv_msgv2 = iv_msgv2
               iv_msgv3 = iv_msgv3
               iv_msgv4 = iv_msgv4 ).
  ENDMETHOD.

  METHOD zif_alog_msg_logger~info_msg.
    entry_msg( iv_msgid = iv_msgid
               iv_msgno = iv_msgno
               iv_msgty = 'I'
               iv_msgv1 = iv_msgv1
               iv_msgv2 = iv_msgv2
               iv_msgv3 = iv_msgv3
               iv_msgv4 = iv_msgv4 ).
  ENDMETHOD.

  METHOD zif_alog_msg_logger~warning_msg.
    entry_msg( iv_msgid = iv_msgid
               iv_msgno = iv_msgno
               iv_msgty = 'W'
               iv_msgv1 = iv_msgv1
               iv_msgv2 = iv_msgv2
               iv_msgv3 = iv_msgv3
               iv_msgv4 = iv_msgv4 ).
  ENDMETHOD.

  METHOD inform_attached_loggers_msg.
    DATA: li_msg_logger TYPE REF TO zif_alog_msg_logger.

    LOOP AT mt_attached_loggers ASSIGNING FIELD-SYMBOL(<li_logger>).
      " Is it a message logger?
      IF CAST cl_abap_objectdescr(
           CAST cl_abap_refdescr( cl_abap_typedescr=>describe_by_data( li_msg_logger )
           )->get_referenced_type( )
         )->applies_to( <li_logger> ) = abap_true.

        " If it is inform it using entry_msg( )
        li_msg_logger ?= <li_logger>.
        li_msg_logger->entry_msg( iv_msgid = iv_msgid
                                  iv_msgno = iv_msgno
                                  iv_msgty = iv_msgty
                                  iv_msgv1 = iv_msgv1
                                  iv_msgv2 = iv_msgv2
                                  iv_msgv3 = iv_msgv3
                                  iv_msgv4 = iv_msgv4 ).
        FREE li_msg_logger.
      ELSE. " If it is not fall back to entry( )
        MESSAGE ID iv_msgid TYPE iv_msgty NUMBER iv_msgno
                WITH iv_msgv1 iv_msgv2 iv_msgv3 iv_msgv4
                INTO DATA(lv_msg_text).

        <li_logger>->entry( iv_text = lv_msg_text
                            io_type = zcl_alog_entry_type=>from_msgty( iv_msgty ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD inform_attached_loggers.
    " Inform attached loggers only if they were not already already informed by entry_msg( )
    IF mv_block_entry_inform = abap_false.
      super->inform_attached_loggers( iv_text = iv_text io_type = io_type ).
    ENDIF.
  ENDMETHOD.

  METHOD entry_msg_internal.
    " Default implementation just redirects to entry( )
    MESSAGE ID iv_msgid TYPE iv_msgty NUMBER iv_msgno
            WITH iv_msgv1 iv_msgv2 iv_msgv3 iv_msgv4
            INTO DATA(lv_msg_text).

    " Attached loggers are already informed
    mv_block_entry_inform = abap_true.
    entry( iv_text = lv_msg_text io_type = zcl_alog_entry_type=>from_msgty( iv_msgty ) ).
    mv_block_entry_inform = abap_false.
  ENDMETHOD.
ENDCLASS.
