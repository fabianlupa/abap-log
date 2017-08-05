CLASS ltcl_test DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      test_entry FOR TESTING,
      test_append FOR TESTING.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      gc_logpath      TYPE fileintern VALUE 'TMP',
      gc_filename     TYPE string VALUE 'zalog_file_logger_test.tmp' ##NO_TEXT,
      gc_line_pattern TYPE string VALUE `^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2} ` &
      `([\w\d]+) [\w\d]+: (.*)$` ##NO_TEXT.
    CLASS-METHODS:
      class_setup,
      class_teardown,
      delete_temp_file,
      get_physical_filename RETURNING VALUE(rv_filename) TYPE string.
    METHODS:
      setup,
      teardown.
    CLASS-DATA:
      go_regex TYPE REF TO cl_abap_regex.
    DATA:
      mo_logger TYPE REF TO zcl_alog_file_logger.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD setup.
    delete_temp_file( ).
    mo_logger = zcl_alog_file_logger=>from_logical_path( iv_logpath  = gc_logpath
                                                         iv_filename = gc_filename ).
  ENDMETHOD.

  METHOD teardown.
    delete_temp_file( ).
  ENDMETHOD.

  METHOD class_setup.
    go_regex = NEW #( gc_line_pattern ).
  ENDMETHOD.

  METHOD class_teardown.
    FREE go_regex.
  ENDMETHOD.

  METHOD get_physical_filename.
    CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
      EXPORTING
        logical_path               = gc_logpath
        file_name                  = gc_filename
      IMPORTING
        file_name_with_path        = rv_filename
      EXCEPTIONS
        path_not_found             = 1
        missing_parameter          = 2
        operating_system_not_found = 3
        file_system_not_found      = 4
        OTHERS                     = 5.

    cl_abap_unit_assert=>assert_subrc( ).
  ENDMETHOD.

  METHOD delete_temp_file.
    DATA(lv_filename) = get_physical_filename( ).
    DELETE DATASET lv_filename.
  ENDMETHOD.

  METHOD test_append.
    CONSTANTS: lc_old_entry TYPE string VALUE 'Old entry',
               lc_new_entry TYPE string VALUE 'New entry'.
    DATA: lv_line  TYPE string,
          lt_lines TYPE stringtab.

    mo_logger->info( lc_old_entry ).
    FREE mo_logger.

    mo_logger = zcl_alog_file_logger=>from_logical_path( iv_logpath  = gc_logpath
                                                         iv_filename = gc_filename ).
    mo_logger->info( lc_new_entry ).

    DATA(lv_filename) = get_physical_filename( ).
    OPEN DATASET lv_filename FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    DO.
      READ DATASET lv_filename INTO lv_line.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      APPEND lv_line TO lt_lines.
    ENDDO.

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_lines ) ).

    DATA(lo_matcher1) = go_regex->create_matcher( text = lt_lines[ 1 ] ).
    cl_abap_unit_assert=>assert_true( lo_matcher1->match( ) ).
    cl_abap_unit_assert=>assert_equals( exp = lc_old_entry act = lo_matcher1->get_submatch( 2 ) ).

    DATA(lo_matcher2) = go_regex->create_matcher( text = lt_lines[ 2 ] ).
    cl_abap_unit_assert=>assert_true( lo_matcher2->match( ) ).
    cl_abap_unit_assert=>assert_equals( exp = lc_new_entry act = lo_matcher2->get_submatch( 2 ) ).
  ENDMETHOD.

  METHOD test_entry.
    TYPES: BEGIN OF lty_test,
             text        TYPE zalog_s_logentry-text,
             type        TYPE REF TO zcl_alog_entry_type,
             description TYPE zalog_s_logentry-type_descr,
           END OF lty_test,
           lty_test_tab TYPE STANDARD TABLE OF lty_test WITH DEFAULT KEY.
    DATA: lt_test   TYPE lty_test_tab,
          lt_result TYPE lty_test_tab,
          lv_line   TYPE string.

    DO 400 TIMES.
      APPEND VALUE #( LET type = ztcl_alog_test_utl=>get_random_log_type( ) IN
                      text = ztcl_alog_test_utl=>get_random_log_text( )
                      type = type
                      description = type->mv_description
                    ) TO lt_test.
    ENDDO.

    " Log all entries
    LOOP AT lt_test ASSIGNING FIELD-SYMBOL(<ls_test>).
      mo_logger->entry( iv_text = <ls_test>-text io_type = <ls_test>-type ).
      FREE <ls_test>-type.
    ENDLOOP.

    " Read them again from the created file
    DATA(lv_filename) = get_physical_filename( ).
    OPEN DATASET lv_filename FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    DO.
      READ DATASET lv_filename INTO lv_line.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      DATA(lo_matcher) = go_regex->create_matcher( text = lv_line ).
      DATA(lv_matches) = lo_matcher->match( ).
      cl_abap_unit_assert=>assert_true( lv_matches ).

      APPEND VALUE #(
        text        = lo_matcher->get_submatch( 2 )
        description = lo_matcher->get_submatch( 1 )
      ) TO lt_result.
    ENDDO.

    " Compare
    cl_abap_unit_assert=>assert_equals( exp = lt_test act = lt_result ).
  ENDMETHOD.
ENDCLASS.
