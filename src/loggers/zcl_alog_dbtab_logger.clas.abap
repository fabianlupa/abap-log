"! Database table logger
"! <p>
"! This logger class is designed as an abstract base class to inherit from to define concrete
"! logger classes, that can log to custom database tables with a compatible structure definition.
"! </p>
"! Features:
"! <ul>
"!   <li>Mapping of log entry text / type / username / timestamp</li>
"!   <li>Custom key configuration (e. g. using UUIDs or number range objects)</li>
"!   <li>Support for secondary database connections</li>
"! </ul>
CLASS zcl_alog_dbtab_logger DEFINITION
  PUBLIC
  ABSTRACT
  INHERITING FROM zcl_alog_msg_logger_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF gty_mapping,
        text_target      TYPE fieldname,
        type_target      TYPE fieldname,
        username_target  TYPE fieldname,
        timestamp_target TYPE fieldname,
      END OF gty_mapping,
      gty_key_component_tab TYPE STANDARD TABLE OF fieldname.
    METHODS:
      "! @parameter iv_tabname | Name of the database table
      "! @parameter is_mapping | Mapping configuration
      "! @parameter it_key_components | Key columns
      "! @parameter iv_secondary_conn_key | Key for secondary database connection
      "! @parameter iv_use_secondary_connection | Use secondary database connection
      "! @parameter iv_always_implicit_commit | Always commit after adding a log entry
      "! @raising zcx_alog_illegal_argument | Illegal argument
      constructor IMPORTING iv_tabname                  TYPE tabname
                            is_mapping                  TYPE gty_mapping
                            it_key_components           TYPE gty_key_component_tab
                            iv_secondary_conn_key       TYPE string OPTIONAL
                            iv_use_secondary_connection TYPE abap_bool DEFAULT abap_false
                            iv_always_implicit_commit   TYPE abap_bool DEFAULT abap_false
                  RAISING   zcx_alog_illegal_argument.
  PROTECTED SECTION.
    METHODS:
      entry_internal REDEFINITION,
      get_next_key ABSTRACT RETURNING VALUE(rv_key) TYPE string,
      "! Persist the new log entry
      "! <p>
      "! The default implementation of this method uses a dynamic OpenSQL db insert based on the
      "! current configuration regarding target table and secondary connection settings. If other
      "! logic should be used by the logger you may override this method and instead for example
      "! call an update function module to make use of update tasks.
      "! </p>
      "! @parameter ig_row | New log entry (type is the structure type of the db table)
      "! @raising zcx_alog_logging_failed | Logging failed
      insert_entry_in_db IMPORTING ig_row TYPE any
                         RAISING   zcx_alog_logging_failed.
  PRIVATE SECTION.
    CLASS-METHODS:
      write_to_component IMPORTING iv_component_name TYPE abap_compname
                                   iv_text           TYPE csequence
                         CHANGING  cg_structure      TYPE data,
      validate_mapping IMPORTING is_mapping TYPE gty_mapping
                                 io_descr   TYPE REF TO cl_abap_structdescr
                       RAISING   zcx_alog_illegal_argument,
      validate_key IMPORTING it_key   TYPE gty_key_component_tab
                             io_descr TYPE REF TO cl_abap_structdescr
                   RAISING   zcx_alog_illegal_argument.
    DATA:
      mv_tabname                  TYPE tabname,
      mo_descr                    TYPE REF TO cl_abap_structdescr,
      ms_mapping                  TYPE gty_mapping,
      mv_connection_key           TYPE string,
      mv_always_implicit_commit   TYPE abap_bool,
      mv_use_secondary_connection TYPE abap_bool,
      mt_key_components           TYPE gty_key_component_tab,
      mo_key_descr                TYPE REF TO cl_abap_structdescr.
ENDCLASS.



CLASS zcl_alog_dbtab_logger IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    mv_tabname = iv_tabname.
    mv_connection_key = iv_secondary_conn_key.
    mv_always_implicit_commit = iv_always_implicit_commit.
    mv_use_secondary_connection = iv_use_secondary_connection.
    mt_key_components = it_key_components.

    CALL FUNCTION 'DDIF_TABT_GET'
      EXPORTING
        name          = iv_tabname
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_alog_illegal_argument
        EXPORTING
          iv_reason = |{ iv_tabname } is not an active database table.|
          iv_value  = iv_tabname ##NO_TEXT.
    ENDIF.

    mo_descr = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name(  iv_tabname ) ).
    ASSERT mo_descr IS BOUND.

    validate_key( it_key = it_key_components io_descr = mo_descr ).
    mo_key_descr = cl_abap_structdescr=>get( VALUE #(
                     FOR comp IN mt_key_components
                     ( name = comp type = mo_descr->get_component_type( comp ) )
                   ) ).

    validate_mapping( is_mapping = is_mapping io_descr = mo_descr ).
    ms_mapping = is_mapping.
  ENDMETHOD.

  METHOD entry_internal.
    DATA: lr_table_line TYPE REF TO data,
          lr_key        TYPE REF TO data.
    FIELD-SYMBOLS: <lg_row> TYPE data,
                   <lg_key> TYPE data.

    CREATE DATA lr_table_line TYPE HANDLE mo_descr.
    ASSERT lr_table_line IS BOUND.

    ASSIGN lr_table_line->* TO <lg_row>.
    ASSERT <lg_row> IS ASSIGNED.

    CREATE DATA lr_key TYPE HANDLE mo_key_descr.
    ASSERT lr_key IS BOUND.
    ASSIGN lr_key->* TO <lg_key>.
    <lg_key> = get_next_key( ).

    <lg_row> = CORRESPONDING #( <lg_key> ).

    IF ms_mapping-text_target IS NOT INITIAL.
      write_to_component(
        EXPORTING
          iv_component_name = ms_mapping-text_target
          iv_text           = iv_text
        CHANGING
          cg_structure      = <lg_row>
      ).
    ENDIF.

    IF ms_mapping-timestamp_target IS NOT INITIAL.
      GET TIME STAMP FIELD DATA(lv_timestamp).
      write_to_component(
        EXPORTING
          iv_component_name = ms_mapping-timestamp_target
          iv_text           = CONV string( lv_timestamp )
        CHANGING
          cg_structure      = <lg_row>
      ).
    ENDIF.

    IF ms_mapping-type_target IS NOT INITIAL.
      write_to_component(
        EXPORTING
          iv_component_name = ms_mapping-type_target
          iv_text           = io_type->mv_message_type
        CHANGING
          cg_structure      = <lg_row>
      ).
    ENDIF.

    IF ms_mapping-username_target IS NOT INITIAL.
      write_to_component(
        EXPORTING
          iv_component_name = ms_mapping-username_target
          iv_text           = cl_abap_syst=>get_user_name( )
        CHANGING
          cg_structure      = <lg_row>
      ).
    ENDIF.

    insert_entry_in_db( <lg_row> ).

    IF mv_always_implicit_commit = abap_true.
      IF mv_use_secondary_connection = abap_false OR mv_connection_key IS INITIAL.
        COMMIT WORK.
      ELSE.
        COMMIT CONNECTION (mv_connection_key).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD insert_entry_in_db.
    IF mv_use_secondary_connection = abap_false OR mv_connection_key IS INITIAL.
      INSERT (mv_tabname) CONNECTION default FROM ig_row.
    ELSE.
      INSERT (mv_tabname) CONNECTION (mv_connection_key) FROM ig_row.
    ENDIF.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_alog_logging_failed
        EXPORTING
          iv_reason = |Dynamic INSERT into table '{ mv_tabname }' failed: { sy-subrc }| ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD write_to_component.
    ASSIGN COMPONENT iv_component_name OF STRUCTURE cg_structure TO FIELD-SYMBOL(<lg_comp>).
    ASSERT <lg_comp> IS ASSIGNED.
    <lg_comp> = iv_text.
*    WRITE iv_text TO <lg_comp>.
  ENDMETHOD.

  METHOD validate_mapping.
    DATA(lo_mapping_descr) = CAST cl_abap_structdescr(
                               cl_abap_typedescr=>describe_by_data( is_mapping )
                             ).

    LOOP AT lo_mapping_descr->components ASSIGNING FIELD-SYMBOL(<ls_component>).
      ASSIGN COMPONENT <ls_component>-name OF STRUCTURE is_mapping TO FIELD-SYMBOL(<lg_target>).
      ASSERT <lg_target> IS ASSIGNED.

      IF <lg_target> IS NOT INITIAL
          AND NOT line_exists( io_descr->components[ name = <lg_target> ] ).

        RAISE EXCEPTION TYPE zcx_alog_illegal_argument
          EXPORTING
            iv_reason = |Column '{ <lg_target> }' is missing in table | &&
                        |{ io_descr->get_relative_name( ) }.|
            iv_value  = CONV string( is_mapping ) ##NO_TEXT.
      ENDIF.

      UNASSIGN <lg_target>.
    ENDLOOP.
  ENDMETHOD.

  METHOD validate_key.
    LOOP AT it_key ASSIGNING FIELD-SYMBOL(<lv_comp>).
      IF NOT line_exists( io_descr->components[ name = <lv_comp> ] ).
        RAISE EXCEPTION TYPE zcx_alog_illegal_argument
          EXPORTING
            iv_reason = |Column '{ <lv_comp> }' of key is missing in table | &&
                        |{ io_descr->get_relative_name( ) }.| ##NO_TEXT.
      ENDIF.
    ENDLOOP.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_alog_illegal_argument
        EXPORTING
          iv_reason = 'Key cannot be empty.' ##NO_TEXT.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
