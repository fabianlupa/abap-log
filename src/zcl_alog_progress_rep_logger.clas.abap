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
class ZCL_ALOG_PROGRESS_REP_LOGGER definition
  public
  inheriting from ZCL_ALOG_MSG_LOGGER_BASE
  create public .

public section.

      "! @parameter iv_output_immediately | Output log entries immediately / always
  methods CONSTRUCTOR
    importing
      !IV_OUTPUT_IMMEDIATELY type ABAP_BOOL default ABAP_TRUE .
  methods SET_TOTAL_ENTRIES
    importing
      !IV_TOTAL_ENTRIES type INT4 .
  methods SET_ACTUAL_ENTRIES
    importing
      !IV_ACTUAL_ENTRIES type INT4 .
protected section.

  methods ENTRY_INTERNAL
    redefinition .
  methods ENTRY_MSG_INTERNAL
    redefinition .
private section.

  data MV_OUTPUT_IMMEDIATELY type ABAP_BOOL .
  data MV_TOTAL_ENTRIES type INT4 .
  data MV_ACTUAL_ENTRIES type INT4 .
ENDCLASS.



CLASS ZCL_ALOG_PROGRESS_REP_LOGGER IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mv_output_immediately = iv_output_immediately.
* EP Set actual and default maxinum counter
    SET_actual_ENTRIES( 1 ).
    SET_TOTAL_ENTRIES( 100 ).

  ENDMETHOD.


  METHOD entry_internal.
    cl_progress_indicator=>progress_indicate( i_text               = iv_text
                                              i_output_immediately = mv_output_immediately
* EP use maximun message
*                                              i_processed          = 1
*                                              i_total              = 100 ).
                                              i_processed          = MV_ACTUAL_ENTRIES
                                              i_total              = MV_TOTAL_ENTRIES ).
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


  method SET_ACTUAL_ENTRIES.
    mv_actual_ENTRIES = iv_actual_ENTRIES.
  endmethod.


  method SET_TOTAL_ENTRIES.
    mv_TOTAL_ENTRIES = iv_TOTAL_ENTRIES.
  endmethod.
ENDCLASS.
