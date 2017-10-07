"! Example class for ADT console logging
CLASS zcl_alog_example_adt_classrun DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_alog_example_adt_classrun IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    DATA(li_logger) = NEW zcl_alog_adt_logger( out ).

    li_logger->info( `Hello world.` ) ##NO_TEXT.
    li_logger->warning( `Hello darkness my old friend.` ) ##NO_TEXT.
    li_logger->error( `I've come to talk with you again.` ) ##NO_TEXT.
    li_logger->debug( `BEEP BOOP` ) ##NO_TEXT.
    li_logger->exception( NEW zcx_alog_argument_null( ) ).

    MESSAGE s000(zalog) WITH 'Hello from message class' INTO DATA(gv_dummy) ##NEEDED ##NO_TEXT.
    li_logger->info_msg( ).

    li_logger->warning_msg(
      iv_msgid = 'ZALOG'
      iv_msgno = '001'
      iv_msgv1 = 'Hello from message class'
      iv_msgv2 = 'without where used list support...'
    ) ##NO_TEXT.
  ENDMETHOD.
ENDCLASS.
