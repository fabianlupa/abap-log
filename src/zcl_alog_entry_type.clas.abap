"! Log entry type enumeration
CLASS zcl_alog_entry_type DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor,
      "! Get an entry type instance from message type
      "! @parameter iv_msgty | Message type
      "! @parameter ro_entry_type | Entry type instance
      "! @raising zcx_alog_call_error | Unknown message type
      from_msgty IMPORTING iv_msgty             TYPE syst_msgty
                 RETURNING VALUE(ro_entry_type) TYPE REF TO zcl_alog_entry_type
                 RAISING   zcx_alog_call_error.
    CLASS-DATA:
      go_info    TYPE REF TO zcl_alog_entry_type READ-ONLY,
      go_warning TYPE REF TO zcl_alog_entry_type READ-ONLY,
      go_error   TYPE REF TO zcl_alog_entry_type READ-ONLY,
      go_debug   TYPE REF TO zcl_alog_entry_type READ-ONLY.
    DATA:
      mv_message_type TYPE syst_msgty READ-ONLY,
      mv_description  TYPE string READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      constructor IMPORTING iv_message_type TYPE syst_msgty
                            iv_description  TYPE csequence.
ENDCLASS.



CLASS zcl_alog_entry_type IMPLEMENTATION.
  METHOD class_constructor.
    DEFINE init.
      &1 = NEW #( iv_message_type = &2
                  iv_description  = &3 ).
    END-OF-DEFINITION.

    init: go_info    'I' 'INFO',
          go_warning 'W' 'WARNING',
          go_error   'E' 'ERROR',
          go_debug   'I' 'DEBUG'.
  ENDMETHOD.

  METHOD from_msgty.
    CASE iv_msgty.
      WHEN 'S' OR 'I'.
        ro_entry_type = go_info.
      WHEN 'E' OR 'X' OR 'A'.
        ro_entry_type = go_error.
      WHEN 'W'.
        ro_entry_type = go_warning.
      WHEN OTHERS.
        " Unsupported message type
        RAISE EXCEPTION TYPE zcx_alog_call_error
          EXPORTING
            is_textid = zcx_alog_call_error=>gc_msgty_unknown
            iv_msgty  = iv_msgty.
    ENDCASE.
  ENDMETHOD.

  METHOD constructor.
    mv_message_type = iv_message_type.
    mv_description = iv_description.
  ENDMETHOD.
ENDCLASS.
