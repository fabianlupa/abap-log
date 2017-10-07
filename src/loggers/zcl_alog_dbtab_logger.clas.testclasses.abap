*CLASS ltcl_test DEFINITION DEFERRED.
*
*CLASS ltcl_logger DEFINITION
*  FOR TESTING
*  INHERITING FROM zcl_alog_dbtab_logger
*  FINAL
*  CREATE PUBLIC.
*
*  PUBLIC SECTION.
*    METHODS:
*      constructor.
*  PROTECTED SECTION.
*    METHODS:
*      get_next_key REDEFINITION.
*  PRIVATE SECTION.
*    CLASS-DATA:
*     gv_last_key TYPE i VALUE 0.
*ENDCLASS.
*
*CLASS ltcl_test DEFINITION
*  FOR TESTING
*  RISK LEVEL CRITICAL
*  DURATION SHORT
*  FINAL.
*
*  PUBLIC SECTION.
*    CONSTANTS:
*      gc_tabname TYPE tabname VALUE 'TEST',
*      BEGIN OF gc_column_names,
*        text      TYPE fieldname VALUE 'TEXT',
*        type      TYPE fieldname VALUE 'TYPE',
*        username  TYPE fieldname VALUE 'USER',
*        timestamp TYPE fieldname VALUE 'TIMESTAMP',
*        index     TYPE fieldname VALUE 'COUNTER',
*      END OF gc_column_names.
*    METHODS:
*      test_entry FOR TESTING.
*  PROTECTED SECTION.
*  PRIVATE SECTION.
*    CLASS-METHODS:
*      class_setup,
*      class_teardown.
*    METHODS:
*      setup,
*      teardown.
*    CLASS-DATA:
*      go_tab_descr TYPE REF TO cl_abap_tabledescr.
*    DATA:
*      mo_logger TYPE REF TO zcl_alog_dbtab_logger.
*ENDCLASS.
*
*CLASS ltcl_logger IMPLEMENTATION.
*  METHOD constructor.
*    super->constructor(
*      iv_tabname = ltcl_test=>gc_tabname
*      is_mapping = VALUE #( text_target = 'TEXT' )
*      it_key_components = VALUE #( ( ltcl_test=>gc_column_names-index ) )
*    ).
*  ENDMETHOD.
*
*  METHOD get_next_key.
*    ADD 1 TO gv_last_key.
*    rv_key = gv_last_key.
*  ENDMETHOD.
*ENDCLASS.
*
*CLASS ltcl_test IMPLEMENTATION.
*  METHOD setup.
*    mo_logger = NEW ltcl_logger( ).
*  ENDMETHOD.
*
*  METHOD teardown.
*    DATA: lr_tab TYPE REF TO data.
*    FIELD-SYMBOLS: <lt_pointer> TYPE STANDARD TABLE.
*
*    FREE mo_logger.
*
*    CREATE DATA lr_tab TYPE HANDLE go_tab_descr.
*    ASSERT lr_tab IS BOUND.
*    ASSIGN lr_tab->* TO <lt_pointer>.
*    ASSERT <lt_pointer> IS ASSIGNED.
*
*    " Delete table content
*    SELECT * INTO TABLE <lt_pointer> FROM (gc_tabname).
*    IF sy-subrc = 0.
*      DELETE (gc_tabname) FROM TABLE <lt_pointer>.
*    ENDIF.
*  ENDMETHOD.
*
*  METHOD class_setup.
*    DATA: lt_definition TYPE STANDARD TABLE OF dd03p.
*
*    DATA(lo_descr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( gc_tabname ) ).
*    go_tab_descr = cl_abap_tabledescr=>get( lo_descr ).
*
*    lt_definition = VALUE #(
*      ( tabname = gc_tabname fieldname = gc_column_names-text )
*      ( tabname = gc_tabname fieldname = gc_column_names-type )
*      ( tabname = gc_tabname fieldname = gc_column_names-username )
*      ( tabname = gc_tabname fieldname = gc_column_names-timestamp )
*    ).
*
**    CALL FUNCTION 'DDIF_TABL_PUT'
**      EXPORTING
**        name      = gc_tabname
***       dd02v_wa  = ' '    " Table Header
***       dd09l_wa  = ' '    " Technical Settings of the Table
**      TABLES
**        dd03p_tab =     " Table Fields
***       dd05m_tab =     " Foreign Key Fields of the Table
***       dd08v_tab =     " Foreign Keys of the Table
***       dd35v_tab =     " Header of the Search Help Assignments of the Table
***       dd36m_tab =     " Allocations of the Search Help Assignments of the Table
***      EXCEPTIONS
***       tabl_not_found    = 1
***       name_inconsistent = 2
***       tabl_inconsistent = 3
***       put_failure       = 4
***       put_refused       = 5
***       others    = 6
**      .
**    ASSERT sy-subrc = 0.
*  ENDMETHOD.
*
*  METHOD class_teardown.
*    CALL FUNCTION 'RS_DD_DELETE_OBJ'
*      EXPORTING
*        no_ask               = abap_true
*        objname              = gc_tabname
*        objtype              = 'T'
*      EXCEPTIONS
*        not_executed         = 1
*        object_not_found     = 2
*        object_not_specified = 3
*        permission_failure   = 4
*        dialog_needed        = 5
*        OTHERS               = 6.
*    ASSERT sy-subrc = 0.
*  ENDMETHOD.
*
*  METHOD test_entry.
*    TYPES: BEGIN OF lty_test,
*             text        TYPE zalog_s_logentry-text,
*             type        TYPE REF TO zcl_alog_entry_type,
*             description TYPE zalog_s_logentry-type_descr,
*           END OF lty_test,
*           lty_test_tab TYPE STANDARD TABLE OF lty_test WITH DEFAULT KEY.
*    DATA: lt_test TYPE lty_test_tab,
*          lr_tab  TYPE REF TO data,
*          lt_cond TYPE stringtab.
*    FIELD-SYMBOLS: <lt_tab>     TYPE STANDARD TABLE,
*                   <lg_row>     TYPE data,
*                   <lg_column>  TYPE data,
*                   <lg_column2> TYPE data.
*
*    DO 400 TIMES.
*      APPEND VALUE #( LET type = ztcl_alog_test_utl=>get_random_log_type( ) IN
*                      text = ztcl_alog_test_utl=>get_random_log_text( )
*                      type = type
*                      description = type->mv_description
*                    ) TO lt_test.
*    ENDDO.
*
*    LOOP AT lt_test ASSIGNING FIELD-SYMBOL(<ls_test>).
*      mo_logger->entry( iv_text = <ls_test>-text io_type = <ls_test>-type ).
*      FREE <ls_test>-type.
*    ENDLOOP.
*
*    COMMIT WORK.
*
*    CREATE DATA lr_tab TYPE HANDLE go_tab_descr.
*    ASSERT lr_tab IS BOUND.
*    ASSIGN lr_tab->* TO <lt_tab>.
*    ASSERT <lt_tab> IS ASSIGNED.
*
*    APPEND LINES OF VALUE stringtab(
**      ( |{ gc_column_names-counter } = @sy-index| )
*    ) TO lt_cond.
*
*    LOOP AT lt_test ASSIGNING FIELD-SYMBOL(<ls_test2>).
*      SELECT * INTO TABLE @<lt_tab>
*        FROM (gc_tabname)
*        WHERE (lt_cond).
*
*      cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( <lt_tab> ) ).
*
*      LOOP AT <lt_tab> ASSIGNING <lg_row>.
*        EXIT.
*      ENDLOOP.
*      ASSERT <lg_row> IS ASSIGNED.
*
*      DO 4 TIMES.
*        DATA(lv_column_name) = SWITCH #( sy-index WHEN 1 THEN gc_column_names-text
*                                                  WHEN 2 THEN gc_column_names-timestamp
*                                                  WHEN 3 THEN gc_column_names-type
*                                                  WHEN 4 THEN gc_column_names-username ).
*
*        ASSIGN COMPONENT lv_column_name OF STRUCTURE <lg_row> TO <lg_column>.
*        ASSERT <lg_column> IS ASSIGNED.
*
*        ASSIGN COMPONENT lv_column_name OF STRUCTURE <ls_test2> TO <lg_column2>.
*        ASSERT <lg_column> IS ASSIGNED.
*
*        cl_abap_unit_assert=>assert_equals( exp = <lg_column2> act = <lg_column> ).
*
*        UNASSIGN: <lg_column>, <lg_column2>.
*      ENDDO.
*
*      CLEAR: <lt_tab>, <lg_row>.
*    ENDLOOP.
*  ENDMETHOD.
*ENDCLASS.
