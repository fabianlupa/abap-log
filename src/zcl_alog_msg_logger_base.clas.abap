"! Message logger base class
CLASS zcl_alog_msg_logger_base DEFINITION
  PUBLIC
  ABSTRACT
  INHERITING FROM zcl_alog_logger_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_alog_msg_logger.
    ALIASES:
      debug_msg FOR zif_alog_msg_logger~debug_msg,
      entry_msg FOR zif_alog_msg_logger~entry_msg,
      error_msg FOR zif_alog_msg_logger~error_msg,
      info_msg FOR zif_alog_msg_logger~info_msg,
      warning_msg FOR zif_alog_msg_logger~warning_msg.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_alog_msg_logger_base IMPLEMENTATION.
  METHOD zif_alog_msg_logger~entry_msg.
    " Default implementation just redirects to entry( )

    MESSAGE ID iv_msgid TYPE iv_msgty NUMBER iv_msgno
            WITH iv_msgv1 iv_msgv2 iv_msgv3 iv_msgv4
            INTO DATA(lv_msg_text).

    entry( iv_text = lv_msg_text io_type = zcl_alog_entry_type=>from_msgty( iv_msgty ) ).
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
ENDCLASS.
