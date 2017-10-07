# abap-log [![Build Status](https://travis-ci.org/flaiker/abap-log.svg?branch=master)](https://travis-ci.org/flaiker/abap-log) [![ABAP Doc](https://img.shields.io/badge/ABAP%20Doc-latest-blue.svg)](https://flaiker.github.io/abap-log/) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
Logging library for ABAP

## Overview
_abap-log_ is a library to provide various ways of logging messages in ABAP. The goal is to have a common interface for logger objects so that dependency injection can be used and logging-actions can be decoupled from application logic. Inline string log messages as well as message-class-based messages are supported.

### Supported targets
- Internal table
- Application log (BC-SRV-BAL)
- `MESSAGE`-statements for batch processing (job log)
- SAP GUI progress indicator
- Custom database tables
- Log text files on the application server
- Eclipse console (using [IF_OO_ADT_CLASSRUN](https://help.sap.com/viewer/c238d694b825421f940829321ffa326a/7.51.1/en-US/520a4e84024b4a96b3793775bf9e6844.html))

### Examples
Example programs for each logger can be found in the package [ZALOG_EXAMPLE](https://github.com/flaiker/abap-log/tree/master/src/example).

**Console logging in ABAP 7.51**
<img src="https://github.com/flaiker/abap-log/wiki/rendered/console.png">

**Internal table logging**
```abap
REPORT zalog_example.

DATA(go_logger) = NEW zcl_alog_itab_logger( ).

go_logger->info( `Hello world.` ) ##NO_TEXT.
go_logger->warning( `Hello darkness my old friend.` ) ##NO_TEXT.
go_logger->error( `Ive come to talk with you again.` ) ##NO_TEXT.
go_logger->debug( `BEEP BOOP` ) ##NO_TEXT.
go_logger->exception( NEW zcx_alog_argument_null( ) ).

MESSAGE s000(zalog) WITH 'Hello from message class' INTO DATA(gv_dummy) ##NEEDED ##NO_TEXT.
go_logger->info_msg( ).

go_logger->warning_msg(
  iv_msgid = 'ZALOG'
  iv_msgno = '001'
  iv_msgv1 = 'Hello from message class'
  iv_msgv2 = 'without where used list support...'
) ##NO_TEXT.

TRY.
    go_logger->display_as_alv( ).
  CATCH cx_salv_msg INTO DATA(gx_ex).
    MESSAGE gx_ex TYPE 'E'.
ENDTRY.
```

**Java style logging**
```abap
CLASS lcl_class_with_logging DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor.
    METHODS:
      run.
  PRIVATE SECTION.
    CLASS-DATA:
      gi_logger TYPE REF TO zif_alog_logger.
ENDCLASS.

CLASS lcl_class_with_logging IMPLEMENTATION.
  METHOD class_constructor.
    DATA: lo_dummy TYPE REF TO lcl_class_with_logging ##NEEDED.
    gi_logger = zcl_alog_static_logger=>get_logger_for_any( lo_dummy ).
  ENDMETHOD.

  METHOD run.
    gi_logger->info( `Info message` ) ##NO_TEXT.
    gi_logger->warning( `WARNING` ) ##NO_TEXT.
  ENDMETHOD.
ENDCLASS.
```

### API and documentation
Library documentation is done using ABAP Doc and can be found [here](https://flaiker.github.io/abap-log/) as deployed HTML. It can also be viewed in eclipse in the _ABAP Element Info_ view (F2).

**Class diagram**
<br/><img src="https://github.com/flaiker/abap-log/wiki/rendered/API.png">


## Installation
To use this library at least ABAP 740 must be supported by the application server. The reason for this is mostly the new syntax which is heavily used.
- Clone the repository via [abapGit](https://github.com/larshp/abapGit)
- Recommended target package: ZALOG

## Development
Feel free to contribute using pull requests or issues for feature requests and bug reports. This project has been entirely developed in the ABAP Development Tools for eclipse, including ABAP Doc documentation (which is automatically deployed to GitHub pages) and class based exceptions (these cannot be fully edited in SE24 (!)). Using eclipse is therefore highly recommended.

## License
[MIT License Copyright (c) 2017 Fabian Lupa](LICENSE)
