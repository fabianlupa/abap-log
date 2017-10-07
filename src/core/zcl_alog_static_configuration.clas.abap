"! Configuration for <em>ZCL_ALOG_STATIC_LOGGER</em>
CLASS zcl_alog_static_configuration DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor,
      "! Set the current configuration
      "! @parameter ii_logger | Logger to use
      "! @parameter iv_with_prefix | Prefix log entries if possible
      "! @raising zcx_alog_argument_null | ii_logger cannot be null
      set_configuration IMPORTING ii_logger      TYPE REF TO zif_alog_logger
                                  iv_with_prefix TYPE abap_bool DEFAULT abap_true
                        RAISING   zcx_alog_argument_null,
      "! Set the current configuration using a configuration instance
      "! @parameter io_config | The configuration to use
      "! @raising zcx_alog_argument_null | io_config cannot be null
      set_configuration_instance IMPORTING io_config TYPE REF TO zcl_alog_static_configuration
                                 RAISING   zcx_alog_argument_null,
      "! Get the currently active configuration
      "! @parameter ro_config | Active configuration instance
      get_active_configuration RETURNING VALUE(ro_config) TYPE REF TO zcl_alog_static_configuration.
    METHODS:
      "! @parameter ii_logger | Logger instance
      "! @parameter iv_with_prefix | Prefix log entries if possible
      "! @raising zcx_alog_argument_null | ii_logger cannot be null
      constructor IMPORTING ii_logger      TYPE REF TO zif_alog_logger
                            iv_with_prefix TYPE abap_bool DEFAULT abap_true
                  RAISING   zcx_alog_argument_null.
    DATA:
      "! Class descriptor for the logger
      mo_logger_descr      TYPE REF TO cl_abap_classdescr READ-ONLY,
      "! The logger supports message class based logging (<em>ZIF_ALOG_MSG_LOGGER</em>)
      mv_supports_t100_msg TYPE abap_bool READ-ONLY,
      "! The logger should use entry prefixes if possible
      mv_with_prefix       TYPE abap_bool READ-ONLY,
      "! Logger reference
      mi_logger            TYPE REF TO zif_alog_logger READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      go_active_configuration TYPE REF TO zcl_alog_static_configuration.
ENDCLASS.



CLASS zcl_alog_static_configuration IMPLEMENTATION.
  METHOD class_constructor.
    " Dummy configuration as a preset
    set_configuration( NEW zcl_alog_dummy_logger( ) ).
  ENDMETHOD.


  METHOD constructor.
    CONSTANTS: lv_msg_internal_methname TYPE abap_methname VALUE 'ENTRY_MSG_INTERNAL'.
    DATA: li_msg_logger TYPE REF TO zif_alog_msg_logger ##NEEDED,
          lo_msg_base   TYPE REF TO zcl_alog_msg_logger_base ##NEEDED.

    IF ii_logger IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_alog_argument_null
        EXPORTING
          iv_variable_name = 'II_LOGGER'.
    ENDIF.

    mo_logger_descr = CAST #( cl_abap_typedescr=>describe_by_object_ref( ii_logger ) ).

    DATA(lv_implements_msg) = CONV abap_bool( boolc(
                                CAST cl_abap_objectdescr(
                                  CAST cl_abap_refdescr(
                                   cl_abap_typedescr=>describe_by_data( li_msg_logger )
                                  )->get_referenced_type( )
                                )->applies_to( ii_logger ) ) ).
    DATA(lo_msg_base_descr) = CAST cl_abap_classdescr(
                                CAST cl_abap_refdescr(
                                  cl_abap_typedescr=>describe_by_data( lo_msg_base )
                                )->get_referenced_type( )
                              ).
    IF lo_msg_base_descr->applies_to( ii_logger ) = abap_true.
      DATA(lv_overrides_msg) = CONV abap_bool( boolc( line_exists(
                                 mo_logger_descr->methods[
                                   is_redefined = abap_true
                                   is_inherited = abap_true
                                   name         = lv_msg_internal_methname
                                 ]
                               ) ) ).
    ENDIF.

    " Native T100 message support means implementing ZIF_ALOG_MSG_LOGGER and either not having
    " ZCL_ALOG_MSG_LOGGER_BASE as a base class or redefining MSG_ENTRY_INTERNAL. These additional
    " checks are needed, because otherwise the fallback implementation of the logger base class
    " will prevent the potential prefix to be added to the log entry even though messages aren't
    " handled natively at all.
    mv_supports_t100_msg = boolc( lv_implements_msg = abap_true AND lv_overrides_msg = abap_true ).

    mi_logger = ii_logger.
    mv_with_prefix = iv_with_prefix.
  ENDMETHOD.


  METHOD get_active_configuration.
    ASSERT go_active_configuration IS BOUND.
    ro_config = go_active_configuration.
  ENDMETHOD.


  METHOD set_configuration.
    set_configuration_instance( NEW #( ii_logger = ii_logger iv_with_prefix = iv_with_prefix ) ).
  ENDMETHOD.


  METHOD set_configuration_instance.
    IF io_config IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_alog_argument_null
        EXPORTING
          iv_variable_name = 'IO_CONFIG'.
    ENDIF.

    go_active_configuration = io_config.
  ENDMETHOD.
ENDCLASS.
