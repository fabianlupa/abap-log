"! Progress reporter logger
"! <p>
"! This logger shows all log entries in the SAP GUI progress indicator (status bar), using
"! <em>CL_PROGRESS_INDICATOR</em> internally (which itself is a wrapper class for function module
"! <em>SAPGUI_PROGRESS_INDICATOR</em>.
"! </p>
"! <p>
"! Keep in mind that sending status messages using the progress indicator to the front end may cause
"! severe performance problems. If that is a concern supply <em>IV_OUTPUT_IMMEDIATELY</em> in the
"! constructor with <em>ABAP_FALSE</em>.
"! </p>
CLASS zcl_alog_progress_rep_logger DEFINITION
  PUBLIC
  INHERITING FROM zcl_alog_msg_logger_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! @parameter iv_output_immediately | Output log entries immediately / always
    METHODS constructor
      IMPORTING
        !iv_output_immediately TYPE abap_bool DEFAULT abap_true .
    METHODS set_total_entries
      IMPORTING
        !iv_total_entries TYPE int4 .
    METHODS set_actual_entries
      IMPORTING
        !iv_actual_entries TYPE int4 .
  PROTECTED SECTION.

    METHODS entry_internal
         REDEFINITION .
    METHODS entry_msg_internal
         REDEFINITION .
  PRIVATE SECTION.

    DATA mv_output_immediately TYPE abap_bool .
    DATA mv_total_entries TYPE int4 .
    DATA mv_actual_entries TYPE int4 .
ENDCLASS.



CLASS ZCL_ALOG_PROGRESS_REP_LOGGER IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mv_output_immediately = iv_output_immediately.
* EP Set actual and default maxinum counter
    set_actual_entries( 1 ).
    set_total_entries( 100 ).

  ENDMETHOD.


  METHOD entry_internal.
    cl_progress_indicator=>progress_indicate( i_text               = iv_text
                                              i_output_immediately = mv_output_immediately
* EP use maximun message
*                                              i_processed          = 1
*                                              i_total              = 100 ).
                                              i_processed          = mv_actual_entries
                                              i_total              = mv_total_entries ).
  ENDMETHOD.


  METHOD entry_msg_internal.
    cl_progress_indicator=>progress_indicate( i_msgid              = iv_msgid
                                              i_msgno              = iv_msgno
                                              i_msgv1              = iv_msgv1
                                              i_msgv2              = iv_msgv2
                                              i_msgv3              = iv_msgv3
                                              i_msgv4              = iv_msgv4
                                              i_output_immediately = mv_output_immediately
                                              i_processed          = 1
                                              i_total              = 100 ).
  ENDMETHOD.


  METHOD set_actual_entries.
    mv_actual_entries = iv_actual_entries.
  ENDMETHOD.


  METHOD set_total_entries.
    mv_total_entries = iv_total_entries.
  ENDMETHOD.
ENDCLASS.
