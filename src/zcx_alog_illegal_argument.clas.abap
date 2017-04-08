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
      BEGIN OF gc_name,
        msgid TYPE symsgid VALUE 'ZALOG',
        msgno TYPE symsgno VALUE '014',
        attr1 TYPE scx_attrname VALUE 'MV_NAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_name,
      BEGIN OF gc_name_and_reason,
        msgid TYPE symsgid VALUE 'ZALOG',
        msgno TYPE symsgno VALUE '015',
        attr1 TYPE scx_attrname VALUE 'MV_NAME',
        attr2 TYPE scx_attrname VALUE 'MV_REASON',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_name_and_reason,
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
      "! @parameter iv_name | Name
      constructor IMPORTING ix_previous LIKE previous OPTIONAL
                            iv_reason   TYPE csequence OPTIONAL
                            iv_value    TYPE csequence OPTIONAL
                            iv_name     TYPE csequence OPTIONAL
                              PREFERRED PARAMETER iv_reason.
    DATA:
      mv_value  TYPE string READ-ONLY,
      mv_name   TYPE string READ-ONLY,
      mv_reason TYPE string READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_alog_illegal_argument IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = ix_previous ).

    mv_reason = iv_reason.
    mv_name = iv_name.
    mv_value = iv_value.

    CLEAR me->textid.
    if_t100_message~t100key = COND #( WHEN iv_reason IS NOT INITIAL AND iv_name IS NOT INITIAL
                                      THEN gc_name_and_reason
                                      WHEN iv_reason IS NOT INITIAL
                                      THEN gc_reason
                                      WHEN iv_name IS NOT INITIAL
                                      THEN gc_name
                                      ELSE gc_illegal_argument ).
  ENDMETHOD.
ENDCLASS.
