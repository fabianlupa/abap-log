"! File logger
"! <p>
"! Allow logging to a file on the application server. The file handle will be opened and closed
"! for each logged message. The file will be created if it does not exist. If it does exists the new
"! entries will be appended. Logical paths are supported.
"! </p>
CLASS zcl_alog_file_logger DEFINITION
  PUBLIC
  INHERITING FROM zcl_alog_msg_logger_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! Get an instance using a logical path
      "! @parameter iv_logpath | Logical path name
      "! @parameter iv_filename | File name
      "! @parameter ro_logger | Logger instance
      "! @raising zcx_alog_illegal_argument | Logical path cannot be used
      from_logical_path IMPORTING iv_logpath       TYPE pathintern
                                  iv_filename      TYPE string
                        RETURNING VALUE(ro_logger) TYPE REF TO zcl_alog_file_logger
                        RAISING   zcx_alog_illegal_argument.
    METHODS:
      "! @parameter iv_path | Physical file path on application server
      constructor IMPORTING iv_path TYPE csequence.
  PROTECTED SECTION.
    METHODS:
      entry_internal REDEFINITION,
      "! Get a formatted log message
      "! <p>
      "! Override this method in a subclass for custom formatting.
      "! </p>
      "! @parameter iv_text | Message text
      "! @parameter io_type | Message type
      "! @parameter rv_formatted_text | Formatted text
      get_formatted_text IMPORTING iv_text                  TYPE csequence
                                   io_type                  TYPE REF TO zcl_alog_entry_type
                         RETURNING VALUE(rv_formatted_text) TYPE string.
  PRIVATE SECTION.
    DATA:
      mv_path TYPE string.
ENDCLASS.



CLASS zcl_alog_file_logger IMPLEMENTATION.
  METHOD from_logical_path.
    DATA: lv_path TYPE string.

    CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
      EXPORTING
        logical_path               = iv_logpath
        file_name                  = iv_filename
      IMPORTING
        file_name_with_path        = lv_path
      EXCEPTIONS
        path_not_found             = 1
        missing_parameter          = 2
        operating_system_not_found = 3
        file_system_not_found      = 4
        OTHERS                     = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO DATA(lv_msg_text).
      RAISE EXCEPTION TYPE zcx_alog_illegal_argument
        EXPORTING
          iv_reason = lv_msg_text
          iv_value  = |{ iv_logpath } { iv_filename }|.
    ENDIF.

    ro_logger = NEW #( lv_path ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    mv_path = iv_path.
  ENDMETHOD.

  METHOD entry_internal.
    DATA: lv_msg TYPE string.

    TRY.
        OPEN DATASET mv_path FOR APPENDING IN TEXT MODE ENCODING DEFAULT MESSAGE lv_msg.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_alog_logging_failed
            EXPORTING
              iv_reason = lv_msg.
        ENDIF.

        DATA(lv_formatted_text) = get_formatted_text( iv_text = iv_text io_type = io_type ).
        TRANSFER lv_formatted_text TO mv_path.

        CLOSE DATASET mv_path.

      CATCH cx_sy_file_open
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_sy_file_authority
            cx_sy_file_io
            cx_sy_file_open_mode INTO DATA(lx_ex).
        RAISE EXCEPTION TYPE zcx_alog_logging_failed
          EXPORTING
            ix_previous = lx_ex
            iv_reason   = lx_ex->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_formatted_text.
    GET TIME.
    rv_formatted_text = |{ sy-datum DATE = ISO } { sy-uzeit TIME = ISO } | &&
                        |{ io_type->mv_description } { cl_abap_syst=>get_user_name( ) }: | &&
                        |{ iv_text }|.
  ENDMETHOD.
ENDCLASS.
