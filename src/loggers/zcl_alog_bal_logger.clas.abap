"! Application log logger
"! <p>
"! Logger that uses BC-SRV-BAL for logging (transaction SLG1, SLG0, ...).
"! </p>
CLASS zcl_alog_bal_logger DEFINITION
  PUBLIC
  INHERITING FROM zcl_alog_msg_logger_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      "! @parameter iv_object | BAL object
      "! @parameter iv_subobject | BAL suboject
      "! @parameter iv_ext_id | External id for the logs
      "! @raising zcx_alog_illegal_argument | Object or subobject do not exist
      constructor IMPORTING iv_object    TYPE balobj_d OPTIONAL
                            iv_subobject TYPE balsubobj OPTIONAL
                            iv_ext_id    TYPE balnrext OPTIONAL
                  RAISING   zcx_alog_illegal_argument,
      exception REDEFINITION,
      "! Save log to db
      "! <p>
      "! Only possible if object / subobject were specified correctly in the constructor.
      "! </p>
      "! @parameter iv_in_update_task | Use update task for saving
      "! @parameter iv_in_secondary_conn | Use secondary database connection
      "! @raising zcx_alog_bal_error | BAL error
      save IMPORTING iv_in_update_task    TYPE abap_bool DEFAULT abap_false
                     iv_in_secondary_conn TYPE abap_bool DEFAULT abap_true
           RAISING   zcx_alog_bal_error,
      "! Show log entries
      "! @parameter is_profile | Display profile
      "! @parameter iv_amodal | Amodal dialog
      "! @raising zcx_alog_bal_error | BAL error
      show_log_entries IMPORTING is_profile TYPE bal_s_prof OPTIONAL
                                 iv_amodal  TYPE abap_bool OPTIONAL
                       RAISING   zcx_alog_bal_error,
      "! Get docking container instance
      "! @parameter ro_container | Docking container
      "! @raising zcx_alog_bal_error | BAL error
      get_docking_container RETURNING VALUE(ro_container) TYPE REF TO cl_gui_docking_container
                            RAISING   zcx_alog_bal_error,
      "! Prepare context for the next entry
      "! @parameter is_context | Context
      prepare_context IMPORTING is_context TYPE bal_s_cont,
      "! Get the log handle of this logger instance
      "! @parameter rv_handle | Log handle
      get_log_handle RETURNING VALUE(rv_handle) TYPE balloghndl.
  PROTECTED SECTION.
    METHODS:
      entry_internal REDEFINITION,
      entry_msg_internal REDEFINITION.
  PRIVATE SECTION.
    DATA:
      mv_log_handle TYPE balloghndl,
      ms_context    TYPE bal_s_cont.
ENDCLASS.



CLASS zcl_alog_bal_logger IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    DATA(ls_log) = VALUE bal_s_log(
      extnumber = iv_ext_id
      object    = iv_object
      subobject = iv_subobject
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
    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
      EXPORTING
        i_log_handle     = mv_log_handle
        i_msgty          = io_type->mv_message_type
        i_probclass      = io_type->mv_bal_probclass
        i_text           = CONV char200( iv_text )
        i_s_context      = ms_context
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      DATA(lx_bal) = NEW zcx_alog_bal_error( ).
      CLEAR ms_context.
      RAISE EXCEPTION TYPE zcx_alog_logging_failed
        EXPORTING
          ix_previous = lx_bal
          iv_reason   = lx_bal->get_text( ).
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
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_in_update_task = iv_in_update_task
        i_t_log_handle   = VALUE bal_t_logh( ( mv_log_handle ) )
        i_2th_connection = iv_in_secondary_conn
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_alog_bal_error.
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
ENDCLASS.
