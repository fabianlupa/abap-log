"! Illegal argument exception
CLASS zcx_alog_illegal_argument DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_illegal_argument,
        msgid TYPE symsgid VALUE 'ZALOG',
        msgno TYPE symsgno VALUE '013',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_illegal_argument,
      BEGIN OF gc_reason,
        msgid TYPE symsgid VALUE 'ZALOG',
        msgno TYPE symsgno VALUE '016',
        attr1 TYPE scx_attrname VALUE 'MV_REASON',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_reason.
    INTERFACES:
      if_t100_message.
    METHODS:
      "! @parameter ix_previous | Previous exception
      "! @parameter iv_reason | Reason
      "! @parameter iv_value | Value
      constructor IMPORTING ix_previous LIKE previous OPTIONAL
                            iv_reason   TYPE csequence OPTIONAL
                            iv_value    TYPE csequence OPTIONAL
                              PREFERRED PARAMETER iv_reason.
    DATA:
      mv_value  TYPE string READ-ONLY,
      mv_reason TYPE string READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_alog_illegal_argument IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = ix_previous ).

    mv_reason = iv_reason.
    mv_value = iv_value.

    CLEAR me->textid.
    if_t100_message~t100key = COND #( WHEN iv_reason IS NOT INITIAL
                                      THEN gc_reason
                                      ELSE gc_illegal_argument ).
  ENDMETHOD.
ENDCLASS.
