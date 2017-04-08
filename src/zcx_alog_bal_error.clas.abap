"! BAL exception
CLASS zcx_alog_bal_error DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      if_t100_message.
    METHODS:
      "! @parameter iv_msgid | Message id
      "! @parameter iv_msgno | Message number
      "! @parameter iv_msgv1 | Message variable 1
      "! @parameter iv_msgv2 | Message variable 2
      "! @parameter iv_msgv3 | Message variable 3
      "! @parameter iv_msgv4 | Message variable 4
      constructor IMPORTING iv_msgid TYPE syst_msgid DEFAULT sy-msgid
                            iv_msgno TYPE syst_msgno DEFAULT sy-msgno
                            iv_msgv1 TYPE syst_msgv DEFAULT sy-msgv1
                            iv_msgv2 TYPE syst_msgv DEFAULT sy-msgv2
                            iv_msgv3 TYPE syst_msgv DEFAULT sy-msgv3
                            iv_msgv4 TYPE syst_msgv DEFAULT sy-msgv4.
    DATA:
      mv_attr1 TYPE syst_msgv READ-ONLY,
      mv_attr2 TYPE syst_msgv READ-ONLY,
      mv_attr3 TYPE syst_msgv READ-ONLY,
      mv_attr4 TYPE syst_msgv READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_alog_bal_error IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( ).

    mv_attr1 = iv_msgv1.
    mv_attr2 = iv_msgv2.
    mv_attr3 = iv_msgv3.
    mv_attr4 = iv_msgv4.

    CLEAR textid.
    if_t100_message~t100key = VALUE #( msgid = iv_msgid
                                       msgno = iv_msgno
                                       attr1 = 'MV_ATTR1'
                                       attr2 = 'MV_ATTR2'
                                       attr3 = 'MV_ATTR3'
                                       attr4 = 'MV_ATTR4' ).
  ENDMETHOD.
ENDCLASS.
