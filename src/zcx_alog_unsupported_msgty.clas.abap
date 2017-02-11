"! Unsupported message type exception
CLASS zcx_alog_unsupported_msgty DEFINITION
  PUBLIC
  INHERITING FROM zcx_alog_call_error
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_msgty_unknown,
        msgid TYPE symsgid VALUE 'ZALOG',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE 'MV_MSGTY',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_msgty_unknown.
    METHODS:
      "! @parameter ix_previous | Previous exception
      "! @parameter iv_msgty | Message type
      constructor IMPORTING ix_previous LIKE previous OPTIONAL
                            iv_msgty    TYPE syst_msgty OPTIONAL.
    DATA:
      mv_msgty TYPE syst_msgty READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_alog_unsupported_msgty IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( ix_previous = ix_previous
                        is_textid   = gc_msgty_unknown ).
    mv_msgty = iv_msgty.
  ENDMETHOD.
ENDCLASS.
