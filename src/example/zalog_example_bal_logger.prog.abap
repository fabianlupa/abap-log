REPORT zalog_example_bal_logger.

DATA(go_logger) = NEW zcl_alog_bal_logger( ).
go_logger->info( `Information` ) ##NO_TEXT.
go_logger->exception( NEW zcx_alog_already_attached( ) ).
go_logger->warning( `Warning` ) ##NO_TEXT.
go_logger->error( `Error` ) ##NO_TEXT.
go_logger->debug( `Debug` ) ##NO_TEXT.

MESSAGE e000(zalog) WITH 'Message class error' INTO DATA(gv_dummy) ##NEEDED ##NO_TEXT.
go_logger->entry_msg( ).

go_logger->prepare_context( VALUE #( tabname = 'T000'
                                     value   = VALUE t000( mtext = 'Context' ) ) ) ##NO_TEXT.
go_logger->info( `Info with context` ) ##NO_TEXT.

go_logger->show_log_entries( ).
