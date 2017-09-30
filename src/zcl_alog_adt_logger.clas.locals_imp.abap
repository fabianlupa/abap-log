CLASS lcl_adapter IMPLEMENTATION.
  METHOD constructor.
    IF io_out IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_alog_argument_null
        EXPORTING
          iv_variable_name = 'IO_OUT'.
    ENDIF.

    DATA(lo_descr) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( io_out ) ).
    IF NOT line_exists( lo_descr->interfaces[ name = gc_out_intfname ] ).
      RAISE EXCEPTION TYPE zcx_alog_illegal_argument
        EXPORTING
          iv_reason = |io_out does not implement '{ gc_out_intfname }'|.
    ENDIF.

    mo_out = io_out.
  ENDMETHOD.

  METHOD line.
    DATA(lv_method) = get_full_method_name( 'LINE' ).
    CALL METHOD mo_out->(lv_method).
  ENDMETHOD.

  METHOD write.
    DATA(lv_method) = get_full_method_name( 'WRITE' ).
    CALL METHOD mo_out->(lv_method)
      EXPORTING
        data = ig_data
        name = iv_name.
  ENDMETHOD.

  METHOD write_data.
    DATA(lv_method) = get_full_method_name( 'WRITE_DATA' ).
    CALL METHOD mo_out->(lv_method)
      EXPORTING
        value = ig_value
        name  = iv_name.
  ENDMETHOD.

  METHOD write_text.
    DATA(lv_method) = get_full_method_name( 'WRITE_TEXT' ).
    CALL METHOD mo_out->(lv_method)
      EXPORTING
        text = iv_text.
  ENDMETHOD.

  METHOD begin_section.
    DATA(lv_method) = get_full_method_name( 'BEGIN_SECTION' ).
    CALL METHOD mo_out->(lv_method)
      EXPORTING
        title = iv_title.
  ENDMETHOD.

  METHOD get_full_method_name.
    rv_full_methname = |{ gc_out_intfname }~{ iv_method }|.
  ENDMETHOD.
ENDCLASS.
