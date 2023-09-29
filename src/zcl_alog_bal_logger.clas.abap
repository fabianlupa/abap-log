"! Application log logger
"! <p>
"! Logger that uses BC-SRV-BAL for logging (transaction SLG1, SLG0, ...).
"! </p>
CLASS zcl_alog_bal_logger DEFINITION
  PUBLIC
  INHERITING FROM zcl_alog_msg_logger_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! @parameter iv_object | BAL object
    "! @parameter iv_subobject | BAL suboject
    "! @parameter iv_ext_id | External id for the logs
    "! @parameter iv_date_delete | Application log: expiry date
    "! @raising zcx_alog_illegal_argument | Object or subobject do not exist
    METHODS constructor IMPORTING iv_object      TYPE balobj_d OPTIONAL
                                  iv_subobject   TYPE balsubobj OPTIONAL
                                  iv_ext_id      TYPE balnrext OPTIONAL
                                  iv_date_delete TYPE aldate_del OPTIONAL
                        RAISING   zcx_alog_illegal_argument.
    METHODS exception REDEFINITION.
    "! Save log to db
    "! <p>
    "! Only possible if object / subobject were specified correctly in the constructor.
    "! </p>
    "! @parameter iv_in_update_task | Use update task for saving
    "! @parameter iv_in_secondary_conn | Use secondary database connection
    "! @raising zcx_alog_bal_error | BAL error
    METHODS save IMPORTING iv_in_update_task    TYPE abap_bool DEFAULT abap_false
                           iv_in_secondary_conn TYPE abap_bool DEFAULT abap_true
                 RAISING   zcx_alog_bal_error.
    "! Show log entries
    "! @parameter is_profile | Display profile
    "! @parameter iv_amodal | Amodal dialog
    "! @raising zcx_alog_bal_error | BAL error
    METHODS show_log_entries IMPORTING is_profile TYPE bal_s_prof OPTIONAL
                                       iv_amodal  TYPE abap_bool OPTIONAL
                             RAISING   zcx_alog_bal_error.
    "! Get docking container instance
    "! @parameter ro_container | Docking container
    "! @raising zcx_alog_bal_error | BAL error
    METHODS get_docking_container RETURNING VALUE(ro_container) TYPE REF TO cl_gui_docking_container
                                  RAISING   zcx_alog_bal_error.
    "! Prepare context for the next entry
    "! @parameter is_context | Context
    METHODS prepare_context IMPORTING is_context TYPE bal_s_cont.
    "! Get the log handle of this logger instance
    "! @parameter rv_handle | Log handle
    METHODS get_log_handle RETURNING VALUE(rv_handle) TYPE balloghndl.

    "! Prepare table for the next entry
    "! @parameter it_table | Table
    "! @parameter iv_table_struc_type_name | Structure name of table
    "! @parameter iv_callback_program | Set callback program for handling message details
    "! @parameter iv_callback_form |  Set callback form for handling message details
    "! @parameter iv_callback_type |  Set callback type for handling message details. '' = Form call, 'F' = Function module call
    METHODS prepare_table IMPORTING it_table                 TYPE ANY TABLE
                                    iv_table_struc_type_name TYPE string
                                    iv_callback_program      TYPE baluep OPTIONAL
                                    iv_callback_form         TYPE baluef OPTIONAL
                                    iv_callback_type         TYPE baluet DEFAULT ''.

    "! Callback method for ALV display out of callback of program/form
    "! @parameter it_params       | Parameters from callback form
    "! @parameter iv_start_column | Start column of ALV popup
    "! @parameter iv_end_column   | End column of ALV popup
    "! @parameter iv_start_line   | Start line of ALV popup
    "! @parameter iv_end_line     | End line of ALV popup
    METHODS log_callback_alv IMPORTING it_params       TYPE rsrv_t_appl_log_params
                                       iv_start_column TYPE i DEFAULT 1
                                       iv_end_column   TYPE i DEFAULT 80
                                       iv_start_line   TYPE i DEFAULT 1
                                       iv_end_line     TYPE i DEFAULT 10.

    "! Get message details abap description
    "! @parameter it_table_struc_type_names | Generate table description for this structure
    "! @parameter eo_struc_def              | Structure type description
    "! @parameter eo_table_def              | Table type description
    "! @raising   cx_sy_struct_creation     | Struct creation error
    "! @raising   cx_sy_table_creation      | Table creation error
    CLASS-METHODS get_msg_details_abap_descr IMPORTING it_table_struc_type_names TYPE string_table
                                             EXPORTING eo_struc_def              TYPE REF TO cl_abap_structdescr
                                                       eo_table_def              TYPE REF TO cl_abap_tabledescr
                                             RAISING   cx_sy_struct_creation
                                                       cx_sy_table_creation.

    "! Show alv from message number of message details
    "! @parameter iv_msgnr           | Message number
    "! @parameter it_message_details | Message details
    "! @parameter iv_start_column    | Start column of ALV popup
    "! @parameter iv_end_column      | End column of ALV popup
    "! @parameter iv_start_line      | Start line of ALV popup
    "! @parameter iv_end_line        | End line of ALV popup
    CLASS-METHODS show_message_alv IMPORTING iv_msgnr           TYPE balmnr
                                             it_message_details TYPE ANY TABLE
                                             iv_start_column    TYPE i DEFAULT 1
                                             iv_end_column      TYPE i DEFAULT 80
                                             iv_start_line      TYPE i DEFAULT 1
                                             iv_end_line        TYPE i DEFAULT 10.

  PROTECTED SECTION.
    METHODS:
      entry_internal REDEFINITION,
      entry_msg_internal REDEFINITION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_msg_details,
        log_handle            TYPE balloghndl,
        msgnr                 TYPE balmnr,
        table_struc_type_name TYPE string,
        data_table            TYPE REF TO data,
      END OF ts_msg_details,
      tt_msg_details TYPE STANDARD TABLE OF ts_msg_details WITH DEFAULT KEY.

    DATA mv_log_handle                TYPE balloghndl.
    DATA ms_context                   TYPE bal_s_cont.
    DATA mr_table                     TYPE REF TO data.
    DATA mv_table_struc_type_name     TYPE string.
    DATA mv_log_number                TYPE balognr.
    DATA mv_msg_count                 TYPE balmnr.
    DATA mt_msg_details_ref           TYPE tt_msg_details.
    DATA mt_table_struc_types         TYPE string_table.
    DATA mv_add_table_to_next_message TYPE abap_bool.
    DATA mv_callback_program          TYPE baluep.
    DATA mv_callback_form             TYPE baluef.
    DATA mv_callback_type             TYPE baluet.

    CONSTANTS cv_name_msg_ident TYPE balpar VALUE 'MSGNR'.

    METHODS get_message_details_table RETURNING VALUE(rt_msg_details) TYPE REF TO data.

    CLASS-METHODS get_table_from_msgnr IMPORTING iv_msgnr           TYPE balmnr
                                                 it_message_details TYPE ANY TABLE
                                       RETURNING VALUE(rr_table)    TYPE REF TO data.
ENDCLASS.



CLASS zcl_alog_bal_logger IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    DATA(ls_log) = VALUE bal_s_log(
      extnumber  = iv_ext_id
      object     = iv_object
      subobject  = iv_subobject
      aldate_del = iv_date_delete
    ).

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_log
      IMPORTING
        e_log_handle            = mv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      DATA(lx_ex) = NEW zcx_alog_bal_error( ).
      RAISE EXCEPTION TYPE zcx_alog_illegal_argument
        EXPORTING
          ix_previous = lx_ex
          iv_reason   = lx_ex->get_text( ).
    ENDIF.


  ENDMETHOD.

  METHOD entry_internal.
    DATA ls_params TYPE bal_s_parm.

    mv_msg_count += 1.

    IF mv_add_table_to_next_message = abap_true.
      IF mv_callback_form IS INITIAL.
        ls_params-callback-userexitf = 'ZALOG_MSG_CALLBACK'.
        ls_params-callback-userexitt = 'F'.
      ELSE.
        ls_params-callback-userexitf = mv_callback_form.
        ls_params-callback-userexitp = mv_callback_program.
        ls_params-callback-userexitt = mv_callback_type.
      ENDIF.

      APPEND VALUE #( parname  = |TS_{ lines( mt_table_struc_types ) }|
                      parvalue = mv_table_struc_type_name ) TO ls_params-t_par.

      APPEND VALUE #( parname  = cv_name_msg_ident
                      parvalue = mv_msg_count ) TO ls_params-t_par.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
      EXPORTING
        i_log_handle     = mv_log_handle
        i_msgty          = io_type->mv_message_type
        i_probclass      = io_type->mv_bal_probclass
        i_text           = CONV char200( iv_text )
        i_s_context      = ms_context
        i_s_params       = ls_params
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      DATA(lx_bal) = NEW zcx_alog_bal_error( ).
      CLEAR ms_context.
      CLEAR mr_table.
      CLEAR mv_table_struc_type_name.
      mv_add_table_to_next_message = abap_false.
      RAISE EXCEPTION NEW zcx_alog_logging_failed( ix_previous = lx_bal
                                                   iv_reason   = lx_bal->get_text( ) ).
    ENDIF.

    IF mv_add_table_to_next_message = abap_true.

      DATA ls_msg_details_ref TYPE ts_msg_details.

      CREATE DATA ls_msg_details_ref-data_table TYPE STANDARD TABLE OF (mv_table_struc_type_name).
      ASSIGN ls_msg_details_ref-data_table->* TO FIELD-SYMBOL(<lt_msg_details_table>).

      ls_msg_details_ref-log_handle      = mv_log_handle.
      ls_msg_details_ref-msgnr           = mv_msg_count.
      ls_msg_details_ref-table_struc_type_name = mv_table_struc_type_name.

      ASSIGN mr_table->* TO FIELD-SYMBOL(<lt_table>).
      <lt_msg_details_table> = <lt_table>.

      APPEND ls_msg_details_ref TO mt_msg_details_ref.

      CLEAR mr_table.
      CLEAR mv_table_struc_type_name.
      mv_add_table_to_next_message = abap_false.

    ENDIF.

    CLEAR ms_context.
  ENDMETHOD.


  METHOD entry_msg_internal.
    DATA(ls_msg) = VALUE bal_s_msg(
      msgty     = iv_msgty
      msgid     = iv_msgid
      msgno     = iv_msgno
      msgv1     = iv_msgv1
      msgv2     = iv_msgv2
      msgv3     = iv_msgv3
      msgv4     = iv_msgv4
      context   = ms_context
      probclass = zcl_alog_entry_type=>from_msgty( iv_msgty )->mv_bal_probclass
    ).
    CLEAR ms_context.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = mv_log_handle
        i_s_msg          = ls_msg
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      DATA(lx_bal) = NEW zcx_alog_bal_error( ).
      RAISE EXCEPTION TYPE zcx_alog_logging_failed
        EXPORTING
          ix_previous = lx_bal
          iv_reason   = lx_bal->get_text( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_docking_container.
    ro_container = NEW cl_gui_docking_container( side = cl_gui_docking_container=>dock_at_bottom ).

    CALL FUNCTION 'BAL_CNTL_CREATE'
      EXPORTING
        i_container          = ro_container
        i_t_log_handle       = VALUE bal_t_logh( ( mv_log_handle ) )
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        OTHERS               = 3.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_alog_bal_error.
    ENDIF.
  ENDMETHOD.


  METHOD get_log_handle.
    rv_handle = mv_log_handle.
  ENDMETHOD.


  METHOD prepare_context.
    ms_context = is_context.
  ENDMETHOD.


  METHOD save.
    DATA: lt_new_lognumbers   TYPE bal_t_lgnm.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_in_update_task = iv_in_update_task
        i_t_log_handle   = VALUE bal_t_logh( ( mv_log_handle ) )
        i_2th_connection = iv_in_secondary_conn
      IMPORTING
        e_new_lognumbers = lt_new_lognumbers
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_alog_bal_error.
    ENDIF.

* find out the lognumber of this saved log
    DATA(ls_new_lognumber) = VALUE #( lt_new_lognumbers[ log_handle = mv_log_handle ] OPTIONAL ).

    " also store message details on data base
    mv_log_number = ls_new_lognumber-lognumber.

    FIELD-SYMBOLS <lt_msg_details> TYPE ANY TABLE.
    DATA(lr_msg_details) = get_message_details_table( ).
    ASSIGN lr_msg_details->* TO <lt_msg_details>.
    IF <lt_msg_details> IS NOT INITIAL.
      EXPORT mt_msg_details = <lt_msg_details> TO DATABASE bal_indx(al) ID mv_log_number.
      CLEAR <lt_msg_details>.
    ENDIF.
  ENDMETHOD.


  METHOD show_log_entries.
    DATA: ls_profile TYPE bal_s_prof.

    IF sy-batch IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF is_profile IS INITIAL.
      CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
        IMPORTING
          e_s_display_profile = ls_profile.
    ELSE.
      ls_profile = is_profile.
    ENDIF.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile  = ls_profile
        i_t_log_handle       = VALUE bal_t_logh( ( mv_log_handle ) )
        i_amodal             = iv_amodal
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_alog_bal_error.
    ENDIF.
  ENDMETHOD.


  METHOD exception.
    inform_attached_loggers( iv_text = ix_ex->get_text( ) io_type = zcl_alog_entry_type=>go_error ).

    CALL FUNCTION 'BAL_LOG_EXC_ADD'
      EXPORTING
        i_log_handle     = mv_log_handle
        i_msgty          = zcl_alog_entry_type=>go_error->mv_message_type
        i_probclass      = zcl_alog_entry_type=>go_error->mv_bal_probclass
        i_exception      = ix_ex
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      DATA(lx_bal) = NEW zcx_alog_bal_error( ).
      RAISE EXCEPTION TYPE zcx_alog_logging_failed
        EXPORTING
          ix_previous = lx_bal
          iv_reason   = lx_bal->get_text( ).
    ENDIF.
  ENDMETHOD.
  METHOD prepare_table.
    mr_table = REF #( it_table ).
    mv_table_struc_type_name = iv_table_struc_type_name.
    mv_add_table_to_next_message = abap_true.
    mv_callback_form = iv_callback_form.
    mv_callback_program = iv_callback_program.
    mv_callback_type = iv_callback_type.
    APPEND mv_table_struc_type_name TO mt_table_struc_types.
  ENDMETHOD.

  METHOD get_msg_details_abap_descr.
    DATA(lt_components) = VALUE cl_abap_structdescr=>component_table(
        ( name = 'LOG_HANDLE' type = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_name( 'BALLOGHNDL' ) ) )
        ( name = 'MSGNR' type = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_name( 'BALMNR' ) ) )
        ( name = 'TABLE_STRUC_TYPE_NAME' type = CAST cl_abap_elemdescr( cl_abap_elemdescr=>get_string( ) ) ) ).

    LOOP AT it_table_struc_type_names REFERENCE INTO DATA(lr_table_struc_types).
      DATA(lv_table_struc_type_name) = lr_table_struc_types->*.
      APPEND VALUE #( name = lv_table_struc_type_name
                      type = cl_abap_tabledescr=>get(
                                 p_line_type = CAST cl_abap_structdescr( cl_abap_tabledescr=>describe_by_name(
                                                                             lv_table_struc_type_name ) ) ) ) TO lt_components.
    ENDLOOP.

    eo_struc_def = cl_abap_structdescr=>get( lt_components ).

    eo_table_def = cl_abap_tabledescr=>get( p_line_type = eo_struc_def ).
  ENDMETHOD.

  METHOD log_callback_alv.
    DATA(lv_msgnr) = CONV balmnr( it_params[ param = 'MSGNR'  ]-value ).

    FIELD-SYMBOLS <lt_msg_details> TYPE ANY TABLE.

    DATA(lr_msg_details) = get_message_details_table( ).
    ASSIGN lr_msg_details->* TO <lt_msg_details>.

    show_message_alv( iv_msgnr           = lv_msgnr
                      it_message_details = <lt_msg_details>
                      iv_start_column    = iv_start_column
                      iv_end_column      = iv_end_column
                      iv_start_line      = iv_start_line
                      iv_end_line        = iv_end_line ).
  ENDMETHOD.

  METHOD get_message_details_table.
    FIELD-SYMBOLS <lt_msg_details> TYPE ANY TABLE.

    get_msg_details_abap_descr( EXPORTING it_table_struc_type_names = mt_table_struc_types
                                IMPORTING eo_struc_def        = DATA(lo_struc_def)
                                          eo_table_def        = DATA(lo_table_def) ).

    CREATE DATA rt_msg_details TYPE HANDLE lo_table_def.
    ASSIGN rt_msg_details->* TO <lt_msg_details>.

    LOOP AT mt_msg_details_ref REFERENCE INTO DATA(lr_msg_details_ref).
      DATA ls_msg_details TYPE REF TO data.
      CREATE DATA ls_msg_details TYPE HANDLE lo_struc_def.
      ASSIGN ls_msg_details->* TO FIELD-SYMBOL(<ls_msg_details>).

      ASSIGN COMPONENT 'LOG_HANDLE' OF STRUCTURE <ls_msg_details> TO FIELD-SYMBOL(<lv_log_handle>).
      <lv_log_handle> = lr_msg_details_ref->*-log_handle.
      ASSIGN COMPONENT 'MSGNR' OF STRUCTURE <ls_msg_details> TO FIELD-SYMBOL(<lv_msgnr>).
      <lv_msgnr> = lr_msg_details_ref->*-msgnr.
      ASSIGN COMPONENT 'TABLE_STRUC_TYPE_NAME' OF STRUCTURE <ls_msg_details> TO FIELD-SYMBOL(<lv_table_struc_type_name>).
      <lv_table_struc_type_name> = lr_msg_details_ref->*-table_struc_type_name.

      ASSIGN lr_msg_details_ref->data_table->* TO FIELD-SYMBOL(<lt_data_table_ref>).

      ASSIGN COMPONENT lr_msg_details_ref->*-table_struc_type_name OF STRUCTURE <ls_msg_details> TO FIELD-SYMBOL(<lt_data_table>).

      <lt_data_table> = <lt_data_table_ref>.

      INSERT <ls_msg_details> INTO TABLE <lt_msg_details>.

    ENDLOOP.
  ENDMETHOD.

  METHOD get_table_from_msgnr.
    DATA lv_where       TYPE string.
    FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.
    CONCATENATE '''' iv_msgnr '''' INTO lv_where.
    CONCATENATE ' MSGNR =' lv_where INTO lv_where SEPARATED BY space.
    LOOP AT it_message_details ASSIGNING FIELD-SYMBOL(<ls_msg_details>) WHERE (lv_where).
      ASSIGN COMPONENT 'TABLE_STRUC_TYPE_NAME' OF STRUCTURE <ls_msg_details> TO FIELD-SYMBOL(<lv_table_type_name>).
      ASSIGN COMPONENT <lv_table_type_name> OF STRUCTURE <ls_msg_details> TO <lt_table>.

      CREATE DATA rr_table TYPE TABLE OF (<lv_table_type_name>).
      ASSIGN rr_table->* TO FIELD-SYMBOL(<lt_return_table>).
      <lt_return_table> = <lt_table>.
      EXIT.
    ENDLOOP.
  ENDMETHOD.

  METHOD show_message_alv.
    FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.

    DATA(lr_table) = get_table_from_msgnr( iv_msgnr           = iv_msgnr
                                           it_message_details = it_message_details  ).

    ASSIGN lr_table->* TO <lt_table>.

    DATA lo_salv TYPE REF TO cl_salv_table.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_salv
                                CHANGING  t_table      = <lt_table> ).
      CATCH cx_salv_msg.
        RAISE EXCEPTION TYPE zcx_alog_bal_error.
    ENDTRY.

    lo_salv->set_screen_popup( start_column = iv_start_column
                               end_column   = iv_end_column
                               start_line   = iv_start_line
                               end_line     = iv_end_line ).

    lo_salv->display( ).
  ENDMETHOD.

ENDCLASS.
