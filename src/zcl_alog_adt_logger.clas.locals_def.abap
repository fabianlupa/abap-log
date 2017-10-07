"! Internal adapter class for <em>IF_OO_ADT_INTRNL_CLASSRUN</em>
CLASS lcl_adapter DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING io_out TYPE REF TO object
                  RAISING   zcx_alog_argument_null
                            zcx_alog_illegal_argument,
      write IMPORTING ig_data TYPE any
                      iv_name TYPE string OPTIONAL,
      write_data IMPORTING ig_value TYPE data
                           iv_name  TYPE string OPTIONAL,
      write_text IMPORTING iv_text TYPE clike,
      line,
      begin_section IMPORTING iv_title TYPE clike OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      gc_out_intfname TYPE abap_intfname VALUE 'IF_OO_ADT_INTRNL_CLASSRUN'.
    CLASS-METHODS:
      get_full_method_name IMPORTING iv_method               TYPE abap_methname
                           RETURNING VALUE(rv_full_methname) TYPE abap_methname.
    DATA:
      mo_out TYPE REF TO object.
ENDCLASS.
