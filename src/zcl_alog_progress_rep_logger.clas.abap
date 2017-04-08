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
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      "! @parameter iv_output_immediately | Output log entries immediately / always
      constructor IMPORTING iv_output_immediately TYPE abap_bool DEFAULT abap_true.
  PROTECTED SECTION.
    METHODS:
      entry_internal REDEFINITION,
      entry_msg_internal REDEFINITION.
  PRIVATE SECTION.
    DATA:
      mv_output_immediately TYPE abap_bool.
ENDCLASS.



CLASS zcl_alog_progress_rep_logger IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_output_immediately = iv_output_immediately.
  ENDMETHOD.

  METHOD entry_internal.
    cl_progress_indicator=>progress_indicate( i_text               = iv_text
                                              i_output_immediately = mv_output_immediately
                                              i_processed          = 1
                                              i_total              = 100 ).
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
ENDCLASS.
