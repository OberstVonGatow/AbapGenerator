REPORT ztemplate001.
*DATA FOR SELECT-OPTIONS - START
DATA: gv_matnr TYPE matnr.
*DATA FOR SELECT-OPTIONS - END

SELECT-OPTIONS s_matnr FOR gv_matnr.


*INITIALIZATION.

*AT SELECTION-SCREEN.

*AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.
*SET SELECT DATA - START
  DATA(lo_model) = NEW zcl_template001_model( ).
*SET SELECT DATA - END

  lo_model->select_data( ).

  " Build View
  DATA(lo_view) = NEW zcl_template001_view( ).
  lo_view->set_model( io_model = lo_model ).


  lo_view->display( ).
