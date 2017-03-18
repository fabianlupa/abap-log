"! Illegal argument exception
CLASS zcx_alog_illegal_argument DEFINITION
  PUBLIC
  INHERITING FROM zcx_alog_call_error
  FINAL
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
    METHODS:
      "! @parameter ix_previous | Previous exception
      constructor IMPORTING ix_previous LIKE previous OPTIONAL
                            iv_reason   TYPE csequence OPTIONAL
                            iv_value    TYPE csequence OPTIONAL
                            iv_name     TYPE csequence OPTIONAL.
    DATA:
      mv_value TYPE string READ-ONLY,
      mv_name  TYPE string READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_alog_illegal_argument IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( ix_previous = ix_previous
                        is_textid   = COND #( WHEN iv_reason IS SUPPLIED
                                                   AND iv_name IS SUPPLIED
                                                   THEN gc_name_and_reason
                                              WHEN iv_reason IS SUPPLIED
                                                   THEN gc_reason
                                              ELSE gc_illegal_argument ) ).
    mv_value = iv_value.
    mv_name = iv_name.
  ENDMETHOD.
ENDCLASS.
