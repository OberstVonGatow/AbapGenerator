CLASS zcl_abap_generator_report DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS Constructor
      IMPORTING
        io_main_generator TYPE REF TO zcl_abap_generator
        iv_name           TYPE zzde_abap_name
        iv_namespace      TYPE zzde_abap_namespace.

    METHODS create_report
      IMPORTING
        iv_package  TYPE devclass
        iv_template TYPE devclass
      RAISING
        zcx_abap_gen_report_create .

    METHODS read_selection
      IMPORTING
        !is_prefix          TYPE zzs_abap_gen_prefix
      RETURNING
        VALUE(rt_selection) TYPE zztt_abap_gen_selection
      RAISING
        zcx_abap_gen_report_read.

    METHODS update_report
      IMPORTING
        is_prefix TYPE zzs_abap_gen_prefix
        iv_class  TYPE seoclskey-clsname
      RAISING
        zcx_abap_gen_report_update
        zcx_abap_gen_report_read.

    METHODS show_report.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_generator TYPE REF TO zcl_abap_generator.

    DATA mt_report_code TYPE string_table .
    DATA mt_selection TYPE zztt_abap_gen_selection.
    DATA mv_new_reportname  TYPE programm.

    METHODS build_new_reportname
      IMPORTING
        iv_namespace         TYPE zzde_abap_namespace
        iv_name              TYPE zzde_abap_name
      RETURNING
        VALUE(rv_reportname) TYPE programm.

    METHODS read_selop_data
      IMPORTING
        !it_report            TYPE string_table
        !is_prefix            TYPE zzs_abap_gen_prefix
      RETURNING
        VALUE(rt_range_types) TYPE zztt_abap_gen_range_types.

    METHODS read_template_report
      IMPORTING
        iv_template               TYPE devclass
      RETURNING
        VALUE(rt_template_source) TYPE string_table.

ENDCLASS.



CLASS zcl_abap_generator_report IMPLEMENTATION.

  METHOD constructor.
    mo_generator = io_main_generator.
    mv_new_reportname = build_new_reportname(
                          iv_namespace = iv_namespace
                          iv_name      = iv_name
                        ).
  ENDMETHOD.

  METHOD build_new_reportname.
    rv_reportname  = iv_namespace && iv_name.
  ENDMETHOD.


  METHOD read_selection.

    READ REPORT mv_new_reportname INTO mt_report_code.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_gen_report_read
        MESSAGE ID zcx_abap_gen_report_read=>msgid
        NUMBER zcx_abap_gen_report_read=>msgno
        WITH mv_new_reportname.
    ENDIF.

    DATA(lt_range_types) = read_selop_data(
                             it_report = mt_report_code
                             is_prefix = is_prefix
                           ).

    DATA lv_nextline_add TYPE string.
    DATA ls_selection TYPE zzs_abap_gen_selection.


    LOOP AT mt_report_code REFERENCE INTO DATA(lr_report_code)
    WHERE table_line IS NOT INITIAL.

      DATA(lv_codeline) = lr_report_code->*.

      IF lv_nextline_add IS NOT INITIAL.
        lv_codeline = |{ lv_nextline_add } { lv_codeline }|.
        CLEAR lv_nextline_add.
      ENDIF.

      lv_codeline = replace( val = lv_codeline sub = ':' with = '' ).
      lv_codeline = replace( val = lv_codeline sub = '.' with = '' ).
      lv_codeline = replace( val = lv_codeline sub = ',' with = '' ).

      SPLIT lv_codeline AT space INTO TABLE DATA(lt_codeline).

      DELETE lt_codeline
        WHERE table_line IS INITIAL
        OR table_line = 'AS'.

      IF lines( lt_codeline ) < 3.
        CONTINUE.
      ENDIF.

      ls_selection-codetype = lt_codeline[ 1 ].

      CASE ls_selection-codetype.
        WHEN zcl_abap_generator=>c_codetype_parameters.

          CASE lt_codeline[ 3 ].
            WHEN 'TYPE'.
              ls_selection-dictype = lt_codeline[ 4 ].
            WHEN 'CHECKBOX'.
              ls_selection-dictype = 'abap_bool'.
            WHEN 'LIKE'.
              "TODO LIKE
              CONTINUE.
            WHEN OTHERS.
              CONTINUE.
          ENDCASE.

          ls_selection-name = lt_codeline[ 2 ].
          ls_selection-name = replace( val = ls_selection-name sub = |{ is_prefix-report_parameter }| with = '' case = abap_false ).

        WHEN zcl_abap_generator=>c_codetype_selops.

          IF lt_codeline[ 3 ] <> 'FOR'.
            CONTINUE.
          ENDIF.

          ls_selection-name = lt_codeline[ 2 ].
          ls_selection-name = replace( val = ls_selection-name sub = |{ is_prefix-report_selopt }| with = '' case = abap_false ).
          READ TABLE lt_range_types REFERENCE INTO DATA(lr_range_types)
          WITH KEY varname = lt_codeline[ 4 ].
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          ls_selection-typename = lr_range_types->typename.
          ls_selection-dictype = lr_range_types->dictype.

        WHEN OTHERS.
          CONTINUE.
      ENDCASE.


      APPEND ls_selection TO rt_selection.

      DATA(lv_codeline_len) = strlen( lr_report_code->* ) - 1.
      IF lr_report_code->*+lv_codeline_len(1) = ','.
        lv_nextline_add = ls_selection-codetype.
      ENDIF.

      CLEAR ls_selection.
    ENDLOOP.

    mt_selection = rt_selection.
  ENDMETHOD.



  METHOD read_selop_data.

    DATA(lv_start_index) = line_index( it_report[ table_line = mo_generator->get_report_sel_start(  ) ] ) + 1.
    DATA(lv_end_index) = line_index( it_report[ table_line = mo_generator->get_report_sel_end( ) ] ) - 1.

    LOOP AT it_report REFERENCE INTO DATA(lr_reportline)
    FROM lv_start_index TO lv_end_index.

      DATA(lv_codeline) = shift_left( lr_reportline->* ).

      lv_codeline = replace( val = lv_codeline sub = ',' with = '' ).
      lv_codeline = replace( val = lv_codeline sub = '.' with = '' ).
      lv_codeline = replace( val = lv_codeline sub = ':' with = '' ).

      SPLIT lv_codeline AT space INTO TABLE DATA(lt_codeline).
      DELETE lt_codeline
          WHERE table_line IS INITIAL
          OR table_line = 'TYPE'
          OR table_line = 'DATA'.

      IF lt_codeline IS INITIAL.
        CONTINUE.
      ENDIF.

      rt_range_types = VALUE #( BASE rt_range_types (
                                    varname = lt_codeline[ 1 ]
                                    dictype = |{ lt_codeline[ 2 ] CASE = UPPER }|
                                    typename = |{ is_prefix-rt_type }{ lt_codeline[ 2 ] CASE = UPPER }|
                                    ) ).

    ENDLOOP.

  ENDMETHOD.

  METHOD create_report.

    SELECT SINGLE FROM reposrc
    FIELDS progname
      WHERE progname = @mv_new_reportname
*      AND r3state = 'A'
      INTO @DATA(lv_progname).
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = mv_new_reportname
        object_class        = 'ABAP'
        mode                = 'I'
*       global_lock         = space
        devclass            = iv_package
*       korrnum             = space
*       use_korrnum_immediatedly = space
*       author              = space
        master_language     = sy-langu
*       genflag             = space
*       program             = space
*       object_class_supports_ma = space
*       extend              = space
        suppress_dialog     = abap_false
*       mod_langu           = space
*       activation_call     = space
*      IMPORTING
*       devclass            =
*       korrnum             =
*       ordernum            =
*       new_corr_entry      =
*       author              =
*       transport_key       =
*       new_extend          =
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_gen_report_create
        MESSAGE ID zcx_abap_gen_report_create=>msgid
        NUMBER zcx_abap_gen_report_create=>msgno
        WITH mv_new_reportname.
    ENDIF.

    DATA lt_source TYPE string_table.
    lt_source = read_template_report( iv_template = iv_template ).
    IF lt_source IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_source REFERENCE INTO DATA(lr_source).
      mo_generator->replace_template_string( ir_string = lr_source iv_lower = abap_true ).
    ENDLOOP.

    CALL FUNCTION 'PRETTY_PRINTER'
      EXPORTING
        inctoo             = space
      TABLES
        ntext              = lt_source
        otext              = lt_source
      EXCEPTIONS
        enqueue_table_full = 1
        include_enqueued   = 2
        include_readerror  = 3
        include_writeerror = 4
        OTHERS             = 5.


    INSERT REPORT mv_new_reportname
      FROM lt_source
      STATE 'A'
      PROGRAM TYPE '1'. "Executable program
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_gen_report_create
        MESSAGE ID zcx_abap_gen_report_create=>msgid
        NUMBER zcx_abap_gen_report_create=>msgno
        WITH mv_new_reportname.
    ENDIF.

    " Insert textpool
    DATA lt_textpool TYPE STANDARD TABLE OF textpool.
    lt_textpool = VALUE #( ( id = 'R' entry = mv_new_reportname+1 ) ).
    INSERT TEXTPOOL mv_new_reportname FROM lt_textpool.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_gen_report_create
        MESSAGE ID zcx_abap_gen_report_create=>msgid
        NUMBER zcx_abap_gen_report_create=>msgno
        WITH mv_new_reportname.
    ENDIF.

    COMMIT WORK AND WAIT.

  ENDMETHOD.

  METHOD read_template_report.

    DATA(lv_packagename) = mo_generator->get_template_packagename( iv_template ).

    cl_package=>load_package(
    EXPORTING
      i_package_name             = lv_packagename
*    i_force_reload             =
    IMPORTING
      e_package                  = DATA(lo_package)
    EXCEPTIONS
      object_not_existing        = 1
      unexpected_error           = 2
      intern_err                 = 3
      object_locked_and_modified = 4
      OTHERS                     = 5
  ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lo_package->get_elements(
      EXPORTING
        i_check_existence = abap_true
      IMPORTING
        e_elements        = DATA(lt_elements)
    ).

    LOOP AT lt_elements REFERENCE INTO DATA(lr_element).
      IF lr_element->*->dev_elem_type = 'PROG'.
        READ REPORT lr_element->*->dev_elem_key INTO rt_template_source.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.



  METHOD update_report.

    DATA lv_methodname TYPE seocmpname.
    lv_methodname = mo_generator->get_selection_methodname( ).

    IF mt_report_code IS INITIAL.
      read_selection(
        EXPORTING
          is_prefix    = is_prefix
      ).
    ENDIF.

    CHECK mt_report_code IS NOT INITIAL.

    DATA(lv_report_set_start) = mo_generator->get_report_set_start( ).
    DATA(lv_report_set_end) = mo_generator->get_report_set_end( ).

    DATA(lv_start_index) = line_index( mt_report_code[ table_line = lv_report_set_start ] ).
    IF lv_start_index <> 0.
      DATA(lv_end_index) = line_index( mt_report_code[ table_line = lv_report_set_end ] ) + 1.
      DATA(lt_report_code_end) = VALUE string_table(
           FOR line IN mt_report_code FROM lv_end_index ( line ) ).
      DELETE mt_report_code FROM lv_start_index.
    ENDIF.

    APPEND lv_report_set_start TO mt_report_code.
    APPEND |DATA(lo_model) = NEW { iv_class CASE = LOWER }( ).| TO mt_report_code.
    APPEND |lo_model->{ lv_methodname CASE = LOWER }(| TO mt_report_code.
    LOOP AT mt_selection REFERENCE INTO DATA(lr_selection).
      APPEND INITIAL LINE TO mt_report_code REFERENCE INTO DATA(lr_codeline).
      CASE lr_selection->codetype.
        WHEN zcl_abap_generator=>c_codetype_parameters.
          lr_codeline->* = |i{ is_prefix-class_parameter }{ lr_selection->name } = { is_prefix-report_parameter }{ lr_selection->name }|.
        WHEN zcl_abap_generator=>c_codetype_selops.
          lr_codeline->* = |i{ is_prefix-class_selopt }{ lr_selection->name } = { is_prefix-report_selopt }{ lr_selection->name }[]|.
      ENDCASE.
      lr_codeline->* = to_lower( lr_codeline->* ).
    ENDLOOP.
    APPEND |).| TO mt_report_code.
    APPEND lv_report_set_end TO mt_report_code.

    APPEND LINES OF lt_report_code_end TO mt_report_code.

    INSERT REPORT mv_new_reportname
      FROM mt_report_code
      STATE 'A'
      PROGRAM TYPE '1'. "Executable program
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_gen_report_update
        MESSAGE ID zcx_abap_gen_report_update=>msgid
        NUMBER zcx_abap_gen_report_update=>msgno
        WITH mv_new_reportname.
    ENDIF.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD show_report.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation   = 'SHOW'
        object_name = mv_new_reportname
        object_type = 'PROG'.

  ENDMETHOD.

ENDCLASS.
