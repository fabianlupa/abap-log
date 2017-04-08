REPORT zalog_example_bal_logger.

DATA(go_logger) = NEW zcl_alog_bal_logger( ).
go_logger->info( `Information` ).
go_logger->exception( NEW zcx_alog_already_attached( ) ).
go_logger->warning( `Warning` ).
go_logger->error( `Error` ).
go_logger->debug( `Debug` ).

MESSAGE e000(zalog) WITH 'Message class error' INTO DATA(gv_dummy) ##NEEDED.
go_logger->entry_msg( ).

go_logger->prepare_context( VALUE #( tabname = 'T000'
                                     value   = VALUE t000( mtext = 'Context' ) ) ).
go_logger->info( `Info with context` ).

go_logger->show_log_entries( ).
