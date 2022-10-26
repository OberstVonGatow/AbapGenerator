*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZZT_ABAP_GEN....................................*
DATA:  BEGIN OF STATUS_ZZT_ABAP_GEN                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZZT_ABAP_GEN                  .
CONTROLS: TCTRL_ZZT_ABAP_GEN
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZZT_ABAP_GEN                  .
TABLES: ZZT_ABAP_GEN                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
