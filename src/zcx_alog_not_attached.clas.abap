"! Logger not attached exception
CLASS zcx_alog_not_attached DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_not_attached,
        msgid TYPE symsgid VALUE 'ZALOG',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_not_attached.
    INTERFACES:
      if_t100_message.
    METHODS:
      "! @parameter ix_previous | Previous exception
      constructor IMPORTING ix_previous  LIKE previous OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_alog_not_attached IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = ix_previous ).

    CLEAR me->textid.
    if_t100_message~t100key = gc_not_attached.
  ENDMETHOD.
ENDCLASS.
