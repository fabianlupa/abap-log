"! Nullpointer exception
CLASS zcx_alog_argument_null DEFINITION
  PUBLIC
  INHERITING FROM zcx_alog_illegal_argument
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_nullpointer,
        msgid TYPE symsgid VALUE 'ZALOG',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_nullpointer,
      BEGIN OF gc_nullpointer_with_name,
        msgid TYPE symsgid VALUE 'ZALOG',
        msgno TYPE symsgno VALUE '012',
        attr1 TYPE scx_attrname VALUE 'MV_VARIABLE_NAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_nullpointer_with_name.
    METHODS:
      "! @parameter ix_previous | Previous exception
      "! @parameter iv_variable_name | Name of the nullpointer variable
      constructor IMPORTING ix_previous      LIKE previous OPTIONAL
                            iv_variable_name TYPE csequence OPTIONAL.
    DATA:
      mv_variable_name TYPE string READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_alog_argument_null IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( ix_previous = ix_previous ).

    mv_variable_name = iv_variable_name.

    if_t100_message~t100key = COND #( WHEN iv_variable_name IS INITIAL
                                      THEN gc_nullpointer
                                      ELSE gc_nullpointer_with_name ).
  ENDMETHOD.
ENDCLASS.
