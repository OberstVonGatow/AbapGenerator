CLASS zcl_template001_view DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS display.

    METHODS: get_model RETURNING VALUE(ro_result) TYPE REF TO zcl_template001_model,
      set_model IMPORTING io_model TYPE REF TO zcl_template001_model.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_model TYPE REF TO zcl_template001_model.
    DATA mo_alv TYPE REF TO cl_gui_alv_grid.


ENDCLASS.



CLASS zcl_template001_view IMPLEMENTATION.

  METHOD get_model.
    ro_result = mo_model.
  ENDMETHOD.

  METHOD set_model.
    mo_model = io_model.
  ENDMETHOD.

  METHOD display.

    DATA(lr_data) = mo_model->get_output_data( ).
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN lr_data->* TO <table>.

    mo_alv = NEW cl_gui_alv_grid(
                      i_parent = cl_gui_container=>default_screen
                      i_appl_events = abap_true ).


* Create Fieldcat
    DATA: o_salv TYPE REF TO cl_salv_table.

    TRY.
        cl_salv_table=>factory( IMPORTING
                                  r_salv_table = o_salv
                                CHANGING
                                  t_table      = <table> ).
      CATCH cx_salv_msg.
        "handle exception
        RETURN.
    ENDTRY.

    DATA(lt_fcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = o_salv->get_columns( )
                                                                       r_aggregations = o_salv->get_aggregations( ) ).

* Set Layout
    DATA(ls_layout) = VALUE lvc_s_layo( zebra      = abap_true
                                        cwidth_opt = 'A'
                                        grid_title = 'TEMPLATE001' ).

* Show ALV
    mo_alv->set_table_for_first_display( EXPORTING
                                          i_bypassing_buffer = abap_false
                                          i_save             = 'A'
                                          is_layout          = ls_layout
                                        CHANGING
                                          it_fieldcatalog    = lt_fcat
                                          it_outtab          = <table> ).

* Force cl_gui_container=>default_screen
    WRITE: space.

  ENDMETHOD.


ENDCLASS.
