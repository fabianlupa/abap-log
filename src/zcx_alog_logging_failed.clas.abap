"! Logging failed exception
CLASS zcx_alog_logging_failed DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_no_arguments,
        msgid TYPE symsgid VALUE 'ZALOG',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_no_arguments,
      BEGIN OF gc_with_reason,
        msgid TYPE symsgid VALUE 'ZALOG',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'MV_REASON',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_with_reason.
    INTERFACES:
      if_t100_message.
    METHODS:
      "! @parameter ix_previous | Previous exception
      "! @parameter iv_reason | Reason
      constructor IMPORTING ix_previous LIKE previous OPTIONAL
                            iv_reason   TYPE csequence OPTIONAL.
    DATA:
      mv_reason TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_alog_logging_failed IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = ix_previous ).

    mv_reason = iv_reason.

    CLEAR me->textid.
    if_t100_message~t100key = COND #( WHEN iv_reason IS NOT INITIAL THEN gc_with_reason
                                      ELSE gc_no_arguments ).
  ENDMETHOD.
ENDCLASS.
