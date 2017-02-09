"! Log entry type enumeration
CLASS zcl_alog_entry_type DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor.
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

  METHOD constructor.
    mv_message_type = iv_message_type.
    mv_description = iv_description.
  ENDMETHOD.
ENDCLASS.
