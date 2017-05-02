REPORT zalog_example_static_logger.

CLASS lcl_class_with_logging DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor.
    METHODS:
      run.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      gi_logger TYPE REF TO zif_alog_logger.
ENDCLASS.

CLASS lcl_class_with_logging IMPLEMENTATION.
  METHOD class_constructor.
    DATA: lo_dummy TYPE REF TO lcl_class_with_logging ##NEEDED.

    gi_logger = zcl_alog_static_logger=>get_logger_for_any( lo_dummy ).

*    gi_logger = zcl_alog_static_logger=>get_logger( 'LCL_CLASS_WITH_LOGGING' ) ##NO_TEXT.
*    mi_logger = zcl_alog_static_logger=>get_logger_for_any( me ).
  ENDMETHOD.

  METHOD run.
    gi_logger->info( 'Doing stuff' ) ##NO_TEXT.
    gi_logger->warning( 'And logging it' ) ##NO_TEXT.
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.
  zcl_alog_static_configuration=>set_configuration( NEW zcl_alog_itab_logger( ) ).

  zcl_alog_static_logger=>info( `Hello world.` ) ##NO_TEXT.
  zcl_alog_static_logger=>info( `Hello world.` ) ##NO_TEXT.
  zcl_alog_static_logger=>warning( `Hello darkness my old friend.` ) ##NO_TEXT.
  zcl_alog_static_logger=>error( `I've come to talk with you again.` ) ##NO_TEXT.
  zcl_alog_static_logger=>debug( `BEEP BOOP` ) ##NO_TEXT.
  zcl_alog_static_logger=>exception( NEW zcx_alog_argument_null( ) ).

  MESSAGE s000(zalog) WITH 'Hello from message class' INTO DATA(gv_dummy) ##NEEDED ##NO_TEXT.
  zcl_alog_static_logger=>info_msg( ).

  zcl_alog_static_logger=>warning_msg(
    iv_msgid = 'ZALOG'
    iv_msgno = '001'
    iv_msgv1 = 'Hello from message class'
    iv_msgv2 = 'without where used list support...'
  ) ##NO_TEXT.

  NEW lcl_class_with_logging( )->run( ).

  TRY.
      CAST zcl_alog_itab_logger(
        zcl_alog_static_configuration=>get_active_configuration( )->mi_logger
      )->display_as_alv_popup( ).
    CATCH cx_salv_msg INTO DATA(gx_ex).
      MESSAGE gx_ex TYPE 'E'.
  ENDTRY.
