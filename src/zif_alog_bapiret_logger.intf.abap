interface ZIF_ALOG_BAPIRET_LOGGER
  public .


  interfaces ZIF_ALOG_LOGGER .
  interfaces ZIF_ALOG_MSG_LOGGER .

  methods ENTRY_BAPIRET_TABLE
    importing
      !IT_RETURN type BAPIRET2_T
    raising
      ZCX_ALOG_LOGGING_FAILED .
  methods ENTRY_BAPIRET
    importing
      !IS_RETURN type BAPIRET2
    raising
      ZCX_ALOG_LOGGING_FAILED .
endinterface.
