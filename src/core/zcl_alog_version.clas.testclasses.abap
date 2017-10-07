CLASS lcl_test DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PUBLIC SECTION.
    METHODS:
      test_compare FOR TESTING,
      test_of FOR TESTING.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_test IMPLEMENTATION.
  METHOD test_compare.
    DATA(lo_version_100) = zcl_alog_version=>of( '1.0.0' ).
    DATA(lo_version_100s) = zcl_alog_version=>of( '1.0.0-SNAPSHOT' ).
    DATA(lo_version_101) = zcl_alog_version=>of( '1.0.1' ).
    DATA(lo_version_200) = zcl_alog_version=>of( '2.0.0' ).

    cl_abap_unit_assert=>assert_true( boolc(
      zcl_alog_version=>compare( io_version_a = lo_version_100
                                 io_version_b = lo_version_100s ) > 0
    ) ).

    cl_abap_unit_assert=>assert_true( boolc(
      zcl_alog_version=>compare( io_version_a = lo_version_100
                                 io_version_b = lo_version_100 ) = 0
    ) ).

    cl_abap_unit_assert=>assert_true( boolc(
      zcl_alog_version=>compare( io_version_a = lo_version_100
                                 io_version_b = lo_version_101 ) < 0
    ) ).

    cl_abap_unit_assert=>assert_true( boolc(
      zcl_alog_version=>compare( io_version_a = lo_version_100
                                 io_version_b = lo_version_200 ) < 0
    ) ).

    cl_abap_unit_assert=>assert_true( boolc(
      zcl_alog_version=>compare( io_version_a = lo_version_200
                                 io_version_b = lo_version_100 ) > 0
    ) ).

    cl_abap_unit_assert=>assert_true( boolc(
      zcl_alog_version=>compare( io_version_a = lo_version_200
                                 io_version_b = lo_version_100s ) > 0
    ) ).

    cl_abap_unit_assert=>assert_true( boolc(
      zcl_alog_version=>compare( io_version_a = lo_version_100s
                                 io_version_b = lo_version_100 ) < 0
    ) ).

    cl_abap_unit_assert=>assert_true( boolc(
      zcl_alog_version=>compare( io_version_a = lo_version_101
                                 io_version_b = lo_version_100 ) > 0
    ) ).

    cl_abap_unit_assert=>assert_true( boolc(
      zcl_alog_version=>compare( io_version_a = lo_version_101
                                 io_version_b = lo_version_100s ) > 0
    ) ).

    cl_abap_unit_assert=>assert_true( boolc(
      zcl_alog_version=>compare( io_version_a = lo_version_100s
                                 io_version_b = lo_version_100s ) = 0
    ) ).
  ENDMETHOD.

  METHOD test_of.
    cl_abap_unit_assert=>assert_bound( zcl_alog_version=>of( '1.0.0' ) ).
    cl_abap_unit_assert=>assert_bound( zcl_alog_version=>of( '1.0.0-SNAPSHOT' ) ).
    cl_abap_unit_assert=>assert_bound( zcl_alog_version=>of( '127.0.0' ) ).
    cl_abap_unit_assert=>assert_bound( zcl_alog_version=>of( '255.255.255' ) ).

    TRY.
        zcl_alog_version=>of( '1.0.0.0' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_alog_illegal_argument ##NO_HANDLER.
    ENDTRY.

    TRY.
        zcl_alog_version=>of( 'TEST' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_alog_illegal_argument ##NO_HANDLER.
    ENDTRY.

    TRY.
        zcl_alog_version=>of( '1.0' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_alog_illegal_argument ##NO_HANDLER.
    ENDTRY.

    TRY.
        zcl_alog_version=>of( 'SNAPSHOT-1.0.0' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_alog_illegal_argument ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
