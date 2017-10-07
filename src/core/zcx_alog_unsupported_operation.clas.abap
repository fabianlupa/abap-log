"! Unsupported operation exception
CLASS zcx_alog_unsupported_operation DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_no_arguments,
        msgid TYPE symsgid VALUE 'ZALOG',
        msgno TYPE symsgno VALUE '017',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_no_arguments,
      BEGIN OF gc_with_operation,
        msgid TYPE symsgid VALUE 'ZALOG',
        msgno TYPE symsgno VALUE '018',
        attr1 TYPE scx_attrname VALUE 'MV_OPERATION',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_with_operation.
    INTERFACES:
      if_t100_message.
    METHODS:
      "! @parameter ix_previous | Previous exception
      "! @parameter iv_operation | Operation that is unsupported
      constructor IMPORTING ix_previous  LIKE previous OPTIONAL
                            iv_operation TYPE csequence OPTIONAL.
    DATA:
      mv_operation TYPE string READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_alog_unsupported_operation IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = ix_previous ).

    mv_operation = iv_operation.

    CLEAR me->textid.
    if_t100_message~t100key = COND #( WHEN iv_operation IS NOT INITIAL
                                      THEN gc_with_operation
                                      ELSE gc_no_arguments ).
  ENDMETHOD.
ENDCLASS.
