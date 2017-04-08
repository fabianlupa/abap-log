*CLASS lcl_logger_proxy DEFINITION DEFERRED.
*CLASS zcl_alog_static_logger DEFINITION LOCAL FRIENDS lcl_logger_proxy.

CLASS lcl_logger_proxy DEFINITION
  INHERITING FROM zcl_alog_msg_logger_base.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_prefix TYPE csequence OPTIONAL.
  PROTECTED SECTION.
    METHODS:
      entry_internal REDEFINITION,
      entry_msg_internal REDEFINITION.
  PRIVATE SECTION.
    DATA:
      mv_prefix TYPE string.
ENDCLASS.

CLASS lcl_logger_proxy IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_prefix = iv_prefix.
  ENDMETHOD.

  METHOD entry_internal.
    DATA(lo_config) = zcl_alog_static_configuration=>get_active_configuration( ).
    DATA(lv_prefix) = COND #( WHEN lo_config->mv_with_prefix = abap_true
                               AND mv_prefix IS NOT INITIAL
                              THEN |{ mv_prefix }: | ).
    lo_config->mi_logger->entry(
      iv_text = |{ lv_prefix }{ iv_text }|
      io_type = io_type
    ).
  ENDMETHOD.

  METHOD entry_msg_internal.

  ENDMETHOD.
ENDCLASS.
