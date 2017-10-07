"! Version constants for abap-log
CLASS zcl_alog_version DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CONSTANTS:
      "! Version of abap-log library in major.minor.revision format
      gc_version TYPE string VALUE '0.2.1-SNAPSHOT'.
    CLASS-METHODS:
      class_constructor,
      "! Factory method
      "! @parameter iv_version | Version string
      "! @parameter ro_version | Created instance
      "! @raising zcx_alog_illegal_argument | iv_version does not have the required format
      of IMPORTING iv_version        TYPE string
         RETURNING VALUE(ro_version) TYPE REF TO zcl_alog_version
         RAISING   zcx_alog_illegal_argument,
      "! Compare two version instances
      "! @parameter io_version_a | Version A
      "! @parameter io_version_b | Version B
      "! @parameter rv_result | <ul>
      "!                          <li>Less than 0 = version A is lower than version B</li>
      "!                          <li>Greater than 0 = version A is higher than version B</li>
      "!                          <li>Equal to 0 = versions are identical</li>
      "!                        </ul>
      "! @raising zcx_alog_argument_null | io_version_a and io_version_b cannot be null
      compare IMPORTING io_version_a     TYPE REF TO zcl_alog_version
                        io_version_b     TYPE REF TO zcl_alog_version
              RETURNING VALUE(rv_result) TYPE i
              RAISING   zcx_alog_argument_null.
    METHODS:
      "! Compare this version to another one
      "! @parameter io_version | The version to compare this instance to
      "! @parameter rv_result | See <em>compare</em>
      "! @raising zcx_alog_argument_null | io_version cannot be null
      compare_to IMPORTING io_version       TYPE REF TO zcl_alog_version
                 RETURNING VALUE(rv_result) TYPE i
                 RAISING   zcx_alog_argument_null.
    CLASS-DATA:
      " These are for the currently installed version
      gv_major       TYPE i READ-ONLY,
      gv_minor       TYPE i READ-ONLY,
      gv_revision    TYPE i READ-ONLY,
      gv_is_snapshot TYPE abap_bool READ-ONLY,
      go_current     TYPE REF TO zcl_alog_version READ-ONLY.
    DATA:
      mv_version     TYPE string READ-ONLY,
      mv_major       TYPE i READ-ONLY,
      mv_minor       TYPE i READ-ONLY,
      mv_revision    TYPE i READ-ONLY,
      mv_is_snapshot TYPE abap_bool READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      constructor IMPORTING iv_version TYPE string.
ENDCLASS.



CLASS zcl_alog_version IMPLEMENTATION.
  METHOD class_constructor.
    go_current = NEW #( gc_version ).
    gv_major = go_current->mv_major.
    gv_minor = go_current->mv_minor.
    gv_revision = go_current->mv_revision.
    gv_is_snapshot = go_current->mv_is_snapshot.
  ENDMETHOD.


  METHOD constructor.
    mv_version = iv_version.

    SPLIT mv_version AT '-' INTO TABLE DATA(lt_split1).

    IF lines( lt_split1 ) = 2.
      mv_is_snapshot = abap_true.
    ENDIF.

    SPLIT lt_split1[ 1 ] AT '.' INTO TABLE DATA(lt_split2).

    mv_major = lt_split2[ 1 ].
    mv_minor = lt_split2[ 2 ].
    mv_revision = lt_split2[ 3 ].
  ENDMETHOD.


  METHOD compare.
    DEFINE compare_attr.
      rv_result = io_version_a->&1 - io_version_b->&1.
    END-OF-DEFINITION.

    IF io_version_a IS NOT BOUND OR io_version_b IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_alog_argument_null
        EXPORTING
          iv_variable_name = 'IO_VERSION_A / IO_VERSION_B'.
    ENDIF.

    WHILE rv_result = 0.
      CASE sy-index.
        WHEN 1.
          compare_attr mv_major.
        WHEN 2.
          compare_attr mv_minor.
        WHEN 3.
          compare_attr mv_revision.
        WHEN 4.
          rv_result = COND #( WHEN io_version_a->mv_is_snapshot = io_version_b->mv_is_snapshot
                              THEN 0
                              WHEN io_version_a->mv_is_snapshot = abap_true THEN -1
                              WHEN io_version_b->mv_is_snapshot = abap_true THEN 1 ).
        WHEN OTHERS.
          EXIT.
      ENDCASE.
    ENDWHILE.
  ENDMETHOD.


  METHOD compare_to.
    rv_result = compare( io_version_a = me io_version_b = io_version ).
  ENDMETHOD.


  METHOD of.
    IF NOT matches( val = iv_version regex = `\d+\.\d+\.\d+(-.+)?` ).
      RAISE EXCEPTION TYPE zcx_alog_illegal_argument
        EXPORTING
          iv_reason = 'Version format invalid'
          iv_value  = iv_version ##NO_TEXT.
    ENDIF.

    ro_version = NEW #( iv_version ).
  ENDMETHOD.
ENDCLASS.
