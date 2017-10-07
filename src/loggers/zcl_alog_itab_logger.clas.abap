"! Internal table logger
CLASS zcl_alog_itab_logger DEFINITION
  PUBLIC
  INHERITING FROM zcl_alog_msg_logger_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      gty_log_tab TYPE STANDARD TABLE OF zalog_s_logentry WITH DEFAULT KEY.
    METHODS:
      "! Display the current log entries as ALV
      "! @raising cx_salv_msg | SALV message
      display_as_alv RAISING cx_salv_msg,
      "! Display the current log entries als ALV in popup mode
      "! @parameter iv_start_column | Start column
      "! @parameter iv_start_line | Start line
      "! @parameter iv_end_column | End column
      "! @parameter iv_end_line | End line
      "! @raising cx_salv_msg | SALV message
      display_as_alv_popup IMPORTING iv_start_column TYPE i DEFAULT 5
                                     iv_start_line   TYPE i DEFAULT 5
                                     iv_end_column   TYPE i DEFAULT 120
                                     iv_end_line     TYPE i DEFAULT 25
                           RAISING   cx_salv_msg,
      "! Get an ALV instance
      "! @parameter io_container | Custom container
      "! @parameter iv_container_name | Container name
      "! @parameter ro_alv | Created instance
      "! @raising cx_salv_msg | SALV message
      get_alv IMPORTING io_container      TYPE REF TO cl_gui_container OPTIONAL
                        iv_container_name TYPE string OPTIONAL
              RETURNING VALUE(ro_alv)     TYPE REF TO cl_salv_table
              RAISING   cx_salv_msg,
      "! Get a copy of the internal log table
      "! @parameter rt_log | Log table
      get_log_table RETURNING VALUE(rt_log) TYPE gty_log_tab.
  PROTECTED SECTION.
    METHODS:
      entry_internal REDEFINITION.
    DATA:
      mt_log TYPE gty_log_tab.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_alog_itab_logger IMPLEMENTATION.
  METHOD entry_internal.
    GET TIME STAMP FIELD DATA(lv_timestamp).
    APPEND VALUE #( type_icon  = io_type->mv_icon
                    type_descr = io_type->mv_description
                    timestamp  = lv_timestamp
                    username   = cl_abap_syst=>get_user_name( )
                    text       = iv_text ) TO mt_log.
  ENDMETHOD.


  METHOD display_as_alv.
    get_alv( )->display( ).
  ENDMETHOD.


  METHOD display_as_alv_popup.
    DATA(lo_alv) = get_alv( ).
    lo_alv->set_screen_popup( start_column = iv_start_column
                              start_line   = iv_start_line
                              end_column   = iv_end_column
                              end_line     = iv_end_line ).
    lo_alv->display( ).
  ENDMETHOD.


  METHOD get_alv.
    " Because salv uses IS SUPPLIED the call needs to look like this unfortunately
    IF io_container IS BOUND AND iv_container_name IS INITIAL.
      cl_salv_table=>factory(
        EXPORTING
          r_container    = io_container
        IMPORTING
          r_salv_table   = ro_alv
        CHANGING
          t_table        = mt_log
      ).
    ELSEIF io_container IS BOUND AND iv_container_name IS NOT INITIAL.
      cl_salv_table=>factory(
        EXPORTING
          container_name = iv_container_name
          r_container    = io_container
        IMPORTING
          r_salv_table   = ro_alv
        CHANGING
          t_table        = mt_log
      ).
    ELSEIF io_container IS NOT BOUND AND iv_container_name IS NOT INITIAL.
      cl_salv_table=>factory(
        EXPORTING
          container_name = iv_container_name
        IMPORTING
          r_salv_table   = ro_alv
        CHANGING
          t_table        = mt_log
      ).
    ELSEIF io_container IS NOT BOUND AND iv_container_name IS INITIAL.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table   = ro_alv
        CHANGING
          t_table        = mt_log
      ).
    ENDIF.

    IF ro_alv IS BOUND.
      ro_alv->get_columns( )->set_optimize( abap_true ).
      ro_alv->get_functions( )->set_all( ).
      TRY.
          ro_alv->get_columns(:
            )->get_column( 'TIMESTAMP' )->set_visible( abap_false ),
            )->get_column( 'USERNAME' )->set_visible( abap_false ).
        CATCH cx_salv_not_found ##NO_HANDLER.
          " Should only happen if someone changes ZALOG_S_LOGENTRY
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD get_log_table.
    rt_log = mt_log.
  ENDMETHOD.
ENDCLASS.
