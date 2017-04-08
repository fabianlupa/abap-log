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
    DATA: li_msg_logger TYPE REF TO zif_alog_msg_logger ##NEEDED.

    zcx_alog_argument_null=>raise_if_nullpointer( io_object        = ii_logger
                                                  iv_variable_name = 'II_LOGGER' ).

    mv_supports_t100_msg = boolc( CAST cl_abap_objectdescr(
                                    CAST cl_abap_refdescr(
                                     cl_abap_typedescr=>describe_by_data( li_msg_logger )
                                    )->get_referenced_type( )
                                  )->applies_to( ii_logger ) ).
    mo_logger_descr = CAST #( cl_abap_typedescr=>describe_by_object_ref( ii_logger ) ).
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
    zcx_alog_argument_null=>raise_if_nullpointer( io_object        = io_config
                                                  iv_variable_name = 'IO_CONFIG' ).
    go_active_configuration = io_config.
  ENDMETHOD.
ENDCLASS.
