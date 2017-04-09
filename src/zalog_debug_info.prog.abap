PROGRAM zalog_debug_info.

cl_demo_output=>new(
  )->begin_section( 'abap-log Debug Information'
  )->write_data( name = 'Version' value = zcl_alog_version=>gc_version
  )->display( ) ##NO_TEXT.

LEAVE PROGRAM.
