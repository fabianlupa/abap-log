"! Call error exception
CLASS zcx_alog_call_error DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_no_arguments,
        msgid TYPE symsgid VALUE 'ZALOG',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_no_arguments,
      BEGIN OF gc_with_reason,
        msgid TYPE symsgid VALUE 'ZALOG',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE 'MV_REASON',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_with_reason,
      BEGIN OF gc_msgty_unknown,
        msgid TYPE symsgid VALUE 'ZALOG',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE 'MV_MSGTY',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_msgty_unknown.
    INTERFACES:
      if_t100_message.
    METHODS:
      "! @parameter is_textid | Textid
      "! @parameter ix_previous | Previous exception
      "! @parameter iv_reason | Reason
      "! @parameter iv_msgty | Unknown message type
      constructor IMPORTING is_textid   LIKE if_t100_message=>t100key OPTIONAL
                            ix_previous LIKE previous OPTIONAL
                            iv_reason   TYPE csequence OPTIONAL
                            iv_msgty    TYPE syst_msgty OPTIONAL.
    DATA:
      mv_reason TYPE string READ-ONLY,
      mv_msgty  TYPE syst_msgty READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_alog_call_error IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = ix_previous ).

    mv_reason = iv_reason.
    mv_msgty = iv_msgty.

    CLEAR me->textid.
    IF is_textid IS INITIAL.
      if_t100_message~t100key = gc_no_arguments.
    ELSE.
      if_t100_message~t100key = is_textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
