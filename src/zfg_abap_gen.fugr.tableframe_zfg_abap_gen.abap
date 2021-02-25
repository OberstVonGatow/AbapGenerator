*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFG_ABAP_GEN
*   generation date: 24.02.2021 at 23:07:21
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFG_ABAP_GEN       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
