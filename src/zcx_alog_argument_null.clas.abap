"! Nullpointer exception
CLASS zcx_alog_argument_null DEFINITION
  PUBLIC
  INHERITING FROM zcx_alog_call_error
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
    CLASS-METHODS:
      "! Raise nullpointer exception if object reference is null
      "! @parameter io_object | Object reference to check
      "! @parameter iv_variable_name | Name of the variable
      "! @raising zcx_alog_nullpointer | io_object is null
      raise_if_nullpointer IMPORTING io_object        TYPE REF TO object
                                     iv_variable_name TYPE csequence OPTIONAL
                           RAISING   zcx_alog_argument_null.
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
    super->constructor( ix_previous = ix_previous
                        is_textid   = COND #( WHEN iv_variable_name IS INITIAL
                                              THEN gc_nullpointer
                                              ELSE gc_nullpointer_with_name ) ).
    mv_variable_name = iv_variable_name.
  ENDMETHOD.

  METHOD raise_if_nullpointer.
    IF io_object IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_alog_argument_null
        EXPORTING
          iv_variable_name = iv_variable_name.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
