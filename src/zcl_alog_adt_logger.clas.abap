"! Logger for ADT console applications
"! <p>
"! This logger dynamically uses the <em>OUT</em>-parameter of <em>IF_OO_ADT_CLASSRUN~MAIN</em> for
"! log output. Static usage is intentionally avoided because the interface is only available as of
"! ABAP 7.51.
"! </p>
"! Unfortunately there are some limitations if you expect Java-style console output. These seem to
"! be 'by design':
"! <ul>
"!   <li>Logged output is only transmitted after the <em>main</em>-method has finished
"!       execution</li>
"!   <li>Logged output is not transmitted at all if the application fails with a short dump</li>
"!   <li>Log entries are always separated by an empty line</li>
"! </ul>
CLASS zcl_alog_adt_logger DEFINITION
  PUBLIC
  INHERITING FROM zcl_alog_msg_logger_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      "! @parameter io_out | Reference to <em>IF_OO_ADT_INTRNL_CLASSRUN</em> as found in
      "!                     <em>IF_OO_ADT_CLASSRUN~MAIN</em>-OUT
      "! @raising zcx_alog_argument_null | <em>io_out</em> cannot be null
      "! @raising zcx_alog_illegal_argument | <em>io_out</em> does not implement
      "!                                      <em>IF_OO_ADT_INTRNL_CLASSRUN</em>
      constructor IMPORTING io_out TYPE REF TO object
                  RAISING   zcx_alog_argument_null
                            zcx_alog_illegal_argument.
  PROTECTED SECTION.
    METHODS:
      entry_internal REDEFINITION.
  PRIVATE SECTION.
    METHODS:
      format_entry IMPORTING iv_text             TYPE csequence
                             io_type             TYPE REF TO zcl_alog_entry_type
                   RETURNING VALUE(rv_formatted) TYPE string.
    DATA:
      mi_console_adapter TYPE REF TO lcl_adapter.
ENDCLASS.



CLASS zcl_alog_adt_logger IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mi_console_adapter = NEW lcl_adapter( io_out ).
  ENDMETHOD.

  METHOD entry_internal.
    mi_console_adapter->write_text( format_entry( iv_text = iv_text io_type = io_type ) ).
  ENDMETHOD.

  METHOD format_entry.
    DATA: lv_timestamp TYPE timestampl.

    ASSERT io_type IS BOUND.
    GET TIME STAMP FIELD lv_timestamp.
    DATA(lv_timezone) = cl_abap_tstmp=>get_system_timezone( ).

    rv_formatted = |{ lv_timestamp TIMESTAMP = ISO TIMEZONE = lv_timezone } | &&
                   |{ io_type->mv_description WIDTH = 7 ALIGN = LEFT } | &&
                   |{ iv_text }|.
  ENDMETHOD.
ENDCLASS.
