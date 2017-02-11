"! Logger already attached exception
CLASS zcx_alog_already_attached DEFINITION
  PUBLIC
  INHERITING FROM zcx_alog_call_error
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_already_attached,
        msgid TYPE symsgid VALUE 'ZALOG',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_already_attached.
    METHODS:
      "! @parameter ix_previous | Previous exception
      constructor IMPORTING ix_previous  LIKE previous OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_alog_already_attached IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( ix_previous = ix_previous
                        is_textid   = gc_already_attached ).
  ENDMETHOD.
ENDCLASS.
