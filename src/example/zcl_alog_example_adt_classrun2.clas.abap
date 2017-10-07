"! Example class for ADT console logging 2
CLASS zcl_alog_example_adt_classrun2 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      if_oo_adt_classrun.
    CLASS-METHODS:
      class_constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      do_stuff.
    CLASS-DATA:
      gi_logger TYPE REF TO zif_alog_msg_logger.
ENDCLASS.

CLASS zcl_alog_example_adt_classrun2 IMPLEMENTATION.
  METHOD class_constructor.
    DATA: lo_dummy TYPE REF TO zcl_alog_example_adt_classrun2 ##NEEDED.
    gi_logger = zcl_alog_static_logger=>get_msg_logger_for_any( lo_dummy ).
  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.
    zcl_alog_static_configuration=>set_configuration( NEW zcl_alog_adt_logger( out ) ).

    gi_logger->info( `Hello world.` ) ##NO_TEXT.
    gi_logger->warning( `Hello darkness my old friend.` ) ##NO_TEXT.
    gi_logger->error( `I've come to talk with you again.` ) ##NO_TEXT.
    gi_logger->debug( `BEEP BOOP` ) ##NO_TEXT.
    gi_logger->exception( NEW zcx_alog_argument_null( ) ).

    MESSAGE s000(zalog) WITH 'Hello from message class' INTO DATA(gv_dummy) ##NEEDED ##NO_TEXT.
    gi_logger->info_msg( ).

    gi_logger->warning_msg(
      iv_msgid = 'ZALOG'
      iv_msgno = '001'
      iv_msgv1 = 'Hello from message class'
      iv_msgv2 = 'without where used list support...'
    ) ##NO_TEXT.

    do_stuff( ).
  ENDMETHOD.

  METHOD do_stuff.
    gi_logger->info( 'Doing stuff over here' ) ##NO_TEXT.
  ENDMETHOD.
ENDCLASS.
