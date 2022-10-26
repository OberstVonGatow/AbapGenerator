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
    DATA ms_variant TYPE disvariant.

ENDCLASS.



CLASS zcl_template001_view IMPLEMENTATION.

  METHOD get_model.
    ro_result = mo_model.
  ENDMETHOD.

  METHOD set_model.
    mo_model = io_model.
  ENDMETHOD.

  METHOD display.

    DATA lo_salv TYPE REF TO cl_salv_table.
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.

    DATA(lr_data) = mo_model->get_output_data( ).
    ASSIGN lr_data->* TO <lt_table>.

    mo_alv = NEW cl_gui_alv_grid(
                      i_parent = cl_gui_container=>default_screen
                      i_appl_events = abap_true ).

    "Create Fieldcat
    TRY.
        cl_salv_table=>factory( IMPORTING
                                  r_salv_table = lo_salv
                                CHANGING
                                  t_table      = <lt_table> ).
      CATCH cx_salv_msg.

    ENDTRY.

    DATA(lt_fcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = lo_salv->get_columns( )
                                                                       r_aggregations = lo_salv->get_aggregations( ) ).

    "Set Layout
    DATA(ls_layout) = VALUE lvc_s_layo( zebra      = abap_true
                                        cwidth_opt = 'A' ).

*    mo_alv->set_title( 'Titel'(001) ).

    ms_variant-report = sy-cprog.

    "Show ALV
    mo_alv->set_table_for_first_display( EXPORTING
                                          i_bypassing_buffer = abap_true
                                          is_variant         = ms_variant
                                          i_save             = 'A'
                                          is_layout          = ls_layout
                                        CHANGING
                                          it_fieldcatalog    = lt_fcat
                                          it_outtab          = <lt_table> ).

* Force cl_gui_container=>default_screen
    WRITE: space.

  ENDMETHOD.


ENDCLASS.
