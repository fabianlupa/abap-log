"! Logger using MESSAGE statements for batch processing
CLASS zcl_alog_message_logger DEFINITION
  PUBLIC
  INHERITING FROM zcl_alog_msg_logger_base
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS:
      entry_internal REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ALOG_MESSAGE_LOGGER IMPLEMENTATION.


  METHOD entry_internal.
    " Message type must be I or S, otherwise the type will be implicity converted to E and program
    " execution is halted. See for details:
    " http://help-legacy.sap.com/abapdocu_751/en/abenabap_message_list_processing.htm
    MESSAGE |{ io_type->mv_message_type }: { iv_text }| TYPE 'I'.
  ENDMETHOD.
ENDCLASS.
