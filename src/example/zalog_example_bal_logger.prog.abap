REPORT zalog_example_bal_logger.

DATA(go_logger) = NEW zcl_alog_bal_logger( ).
go_logger->info( `Information` ) ##NO_TEXT.
go_logger->exception( NEW zcx_alog_already_attached( ) ).
go_logger->warning( `Warning` ) ##NO_TEXT.
go_logger->error( `Error` ) ##NO_TEXT.
go_logger->debug( `Debug` ) ##NO_TEXT.

DATA gt_table TYPE STANDARD TABLE OF bal_s_ex06.

gt_table = VALUE #( ( id = '00000002' txt_id = 'Chris Smith' )
                    ( id = '00000013' txt_id = 'Paula Quick' )
                    ( id = '00000002' txt_id = 'Tim Meyer' )  ) ##NO_TEXT.

" If you use a object for the logger and therefore save the log to the database,
" please do not use the callback parameters.
" The logger itself will handle the display of the data table
go_logger->prepare_table( it_table                 = gt_table
                          iv_table_struc_type_name = 'BAL_S_EX06'
                          iv_callback_form         = 'LOG_CALLBACK'
                          iv_callback_program      = sy-repid ).

go_logger->info( 'Info with attached table!' ) ##NO_TEXT.

MESSAGE e000(zalog) WITH 'Message class error' INTO DATA(gv_dummy) ##NEEDED ##NO_TEXT.
go_logger->entry_msg( ).

go_logger->prepare_context( VALUE #( tabname = 'T000'
                                     value   = VALUE t000( mtext = 'Context' ) ) ) ##NO_TEXT.
go_logger->info( `Info with context` ) ##NO_TEXT.

go_logger->show_log_entries( ).

FORM log_callback
 TABLES
    it_params STRUCTURE spar ##CALLED.
  go_logger->log_callback_alv( it_params[] ).
ENDFORM.
