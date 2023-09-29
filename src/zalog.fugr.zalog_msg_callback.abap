FUNCTION zalog_msg_callback.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  TABLES
*"      I_T_PARAMS STRUCTURE  SPAR
*"----------------------------------------------------------------------
  DATA(lv_lognumber) = i_t_params[ param = '%LOGNUMBER' ]-value.
  IF lv_lognumber(1) = '$'.
    " This is not possible because message details table is not exported if log is not saved to database
    RETURN.
  ENDIF.

  DATA(lv_msgnr) = CONV balmnr( i_t_params[ param = 'MSGNR'  ]-value ).

  DATA lt_table_types TYPE string_table.
  LOOP AT i_t_params REFERENCE INTO DATA(lr_param) WHERE param CP 'TS_*'.
    APPEND lr_param->*-value TO lt_table_types.
  ENDLOOP.

  zcl_alog_bal_logger=>get_msg_details_abap_descr( EXPORTING it_table_struc_type_names = lt_table_types
                                                   IMPORTING eo_table_def              = DATA(lo_table_def) ).
  FIELD-SYMBOLS <lt_msg_details> TYPE ANY TABLE.
  DATA lt_msg_details TYPE REF TO data.
  CREATE DATA lt_msg_details TYPE HANDLE lo_table_def.
  ASSIGN lt_msg_details->* TO <lt_msg_details>.

  IMPORT mt_msg_details TO <lt_msg_details> FROM DATABASE bal_indx(al) ID lv_lognumber.
  IF sy-subrc <> 0.
    CLEAR <lt_msg_details>.
    RETURN.
  ENDIF.

  zcl_alog_bal_logger=>show_message_alv( iv_msgnr           = lv_msgnr
                                         it_message_details = <lt_msg_details> ).
ENDFUNCTION.
