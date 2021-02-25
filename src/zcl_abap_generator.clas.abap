CLASS zcl_abap_generator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_codetype_selops TYPE string VALUE 'SELECT-OPTIONS'.
    CONSTANTS c_codetype_parameters TYPE string VALUE 'PARAMETERS'.

    METHODS Constructor.

    METHODS set_main_attributes
      IMPORTING
        iv_name      TYPE zzde_abap_name
        iv_namespace TYPE zzde_abap_namespace
        iv_local     TYPE zzde_abap_local
      RAISING
        zcx_abap_gen_main_att.

    METHODS run_initialization
      IMPORTING
        iv_packagedesc TYPE as4text
        iv_dlvunit     TYPE dlvunit
        iv_parentcl    TYPE packparent OPTIONAL
        iv_template    TYPE devclass
      RAISING
        zcx_abap_gen_package
        zcx_abap_gen_report_create
        zcx_abap_gen_class_read
        zcx_abap_gen_class_create.

    METHODS update_selection
      IMPORTING
        iv_modelsufix    TYPE zzde_abap_modelsuffix
        is_prefix        TYPE zzs_abap_gen_prefix
        iv_dict_rangetab TYPE abap_bool
      RAISING
        zcx_abap_gen_report_read
        zcx_abap_gen_class_update
        zcx_abap_gen_report_update.


    METHODS replace_template_string
      IMPORTING
        !ir_string TYPE REF TO data
        !iv_lower  TYPE abap_bool DEFAULT abap_false.

    METHODS replace_template_structure
      IMPORTING
        !ir_structure TYPE REF TO data.

    METHODS replace_template_table
      IMPORTING
        ir_table TYPE REF TO data.

    METHODS get_template_packagename
      IMPORTING
        iv_template           TYPE devclass
      RETURNING
        VALUE(rv_packagename) TYPE devclass.

    METHODS get_package_rangetypes
      RETURNING
        VALUE(rv_package) TYPE devclass.

    METHODS get_selection_methodname
      RETURNING
        VALUE(rv_methodname) TYPE seocmpname.

    METHODS get_report_sel_start
      RETURNING
        VALUE(rv_result) TYPE string.
    METHODS get_report_sel_end
      RETURNING
        VALUE(rv_result) TYPE string.
    METHODS get_report_set_start
      RETURNING
        VALUE(rv_result) TYPE string.
    METHODS get_report_set_end
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS get_default_namespace
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS get_default_dlvunit
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS get_default_model_suffix
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS get_default_dict_rt_type
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS get_default_prefix
      RETURNING
        VALUE(rs_prefix) TYPE zzs_abap_gen_prefix.
    METHODS show_results.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_classes_generator TYPE REF TO zcl_abap_generator_class.
    DATA mo_report_generator TYPE REF TO zcl_abap_generator_report.
    DATA mo_package_generator TYPE REF TO zcl_abap_generator_package.

    DATA mv_name TYPE zzde_abap_name.
    DATA mv_template TYPE devclass.

    DATA mt_settings TYPE STANDARD TABLE OF zzt_abap_gen.
    DATA mt_technical_data TYPE STANDARD TABLE OF abap_compname.

    METHODS get_settings.
    METHODS set_initial_settings
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS clear_technical_data
      IMPORTING
        iv_name  TYPE abap_compname
        ir_field TYPE REF TO data.
    METHODS init_technical_data.

ENDCLASS.



CLASS zcl_abap_generator IMPLEMENTATION.


  METHOD constructor.
    get_settings( ).
  ENDMETHOD.

  METHOD set_main_attributes.

    IF iv_name IS INITIAL
    OR iv_namespace IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abap_gen_main_att.
    ENDIF.

    mv_name = iv_name.

    mo_package_generator = NEW #(
            iv_name = iv_name
            iv_namespace = iv_namespace
            iv_local = iv_local
        ).

    mo_classes_generator = NEW #(
            io_main_generator =  me
        ).

    mo_report_generator = NEW #(
            io_main_generator = me
            iv_name = iv_name
            iv_namespace = iv_namespace
        ).

  ENDMETHOD.

  METHOD replace_template_string.
    FIELD-SYMBOLS <any> TYPE any.
    ASSIGN ir_string->* TO <any>.
    IF <any> IS ASSIGNED.
      IF iv_lower = abap_true.
        <any> = replace( val = <any> sub = mv_template with = to_lower( mv_name ) occ = 0 case = abap_false ).
      ELSE.
        <any> = replace( val = <any> sub = mv_template with = mv_name occ = 0 case = abap_false ).
      ENDIF.

    ENDIF.
  ENDMETHOD.

  METHOD clear_technical_data.
    IF mt_technical_data IS INITIAL.
      init_technical_data(  ).
    ENDIF.
    IF line_exists( mt_technical_data[ table_line = iv_name ] ).
      FIELD-SYMBOLS <any> TYPE any.
      ASSIGN ir_field->* TO <any>.
      IF <any> IS ASSIGNED.
        CLEAR <any>.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD replace_template_structure.

    FIELD-SYMBOLS <field> TYPE any.
    FIELD-SYMBOLS <structure> TYPE any.
    DATA lo_structdescr TYPE REF TO cl_abap_structdescr.

    ASSIGN ir_structure->* TO <structure>.
    lo_structdescr ?= cl_abap_structdescr=>describe_by_data_ref( p_data_ref = ir_structure ).

    LOOP AT lo_structdescr->components ASSIGNING FIELD-SYMBOL(<component>).
      ASSIGN COMPONENT <component>-name OF STRUCTURE <structure> TO <field>.
      IF <field> IS ASSIGNED.
        clear_technical_data( iv_name = <component>-name ir_field = REF #( <field> ) ).

        IF <component>-type_kind = cl_abap_typedescr=>typekind_char
        OR <component>-type_kind = cl_abap_typedescr=>typekind_clike
        OR <component>-type_kind = cl_abap_typedescr=>typekind_csequence
        OR <component>-type_kind = cl_abap_typedescr=>typekind_string.
          replace_template_string( REF #( <field> ) ).
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD replace_template_table.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    DATA lr_line TYPE REF TO data.

    ASSIGN ir_table->* TO <table>.
    LOOP AT <table> REFERENCE INTO lr_line.
      replace_template_structure( ir_structure = lr_line ).
    ENDLOOP.

  ENDMETHOD.


  METHOD run_initialization.

    mv_template = iv_template.

    DATA(lv_packagename) = mo_package_generator->get_new_packagename( ).

    mo_package_generator->create_package(
      EXPORTING
        iv_packagedesc = iv_packagedesc
        iv_dlvunit     = iv_dlvunit
        iv_parentcl    = iv_parentcl
    ).

    mo_report_generator->create_report(
      EXPORTING
        iv_package   = lv_packagename
        iv_template  = iv_template
    ).

    mo_classes_generator->create_classes(
      EXPORTING
        iv_package  = lv_packagename
        iv_template = iv_template
    ).

  ENDMETHOD.




  METHOD update_selection.

    DATA(lt_selection) = mo_report_generator->read_selection(
                           is_prefix    =  is_prefix
                         ).

    DATA(lv_packagename) = mo_package_generator->get_new_packagename( ).

    DATA(lv_classname) = mo_classes_generator->update_selection(
                              EXPORTING
                                iv_packagename     = lv_packagename
                                iv_modelsufix      = iv_modelsufix
                                is_prefix          = is_prefix
                                it_selection       = lt_selection
                                iv_dict_rangetab   = iv_dict_rangetab
                            ).

    mo_report_generator->update_report(
      EXPORTING
        is_prefix = is_prefix
        iv_class  = lv_classname
    ).

  ENDMETHOD.

  METHOD get_settings.

    SELECT FROM zzt_abap_gen
        FIELDS *
        INTO TABLE @mt_settings.
    IF sy-subrc <> 0.
      set_initial_settings( ).
    ENDIF.
  ENDMETHOD.

  METHOD set_initial_settings.
    " Fill intial values for first run

    mt_settings = VALUE #(
        ( client = sy-mandt object = 'PACKAGE_RANGETYPES'   value = 'ZABAP_GENERATOR_RANGES' )
        ( client = sy-mandt object = 'TEMPLATE_PREFIX'      value = 'ZABAP_GEN_TPL_' )
        ( client = sy-mandt object = 'SELECTION_METHODNAME' value = 'SET_SELSCREEN_DATA' )

        ( client = sy-mandt object = 'REPORT_SEL_START'     value = '*DATA FOR SELECT-OPTIONS - START' )
        ( client = sy-mandt object = 'REPORT_SEL_END'       value = '*DATA FOR SELECT-OPTIONS - END' )
        ( client = sy-mandt object = 'REPORT_SET_START'     value = '*SET SELECT DATA - START' )
        ( client = sy-mandt object = 'REPORT_SET_END'       value = '*SET SELECT DATA - END' )

        ( client = sy-mandt object = 'NAMESPACE'            value = 'Z' )
        ( client = sy-mandt object = 'DLVUNIT'              value = 'HOME' )
        ( client = sy-mandt object = 'CLASS_MODEL_SUFFIX'   value = 'MODEL' )

        ( client = sy-mandt object = 'REPORT_P_PREFIX'      value = 'P_' )
        ( client = sy-mandt object = 'REPORT_S_PREFIX'      value = 'S_' )
        ( client = sy-mandt object = 'ATTRIBUTE_PREFIX'     value = 'M' )
        ( client = sy-mandt object = 'CLASS_P_PREFIX'       value = 'V_' )
        ( client = sy-mandt object = 'CLASS_S_PREFIX'       value = 'RT_' )
        ( client = sy-mandt object = 'CLASS_RT_TYP_PREFIX'  value = 'TYRT_' )
        ( client = sy-mandt object = 'DICT_RT_TYP_PREFIX'   value = 'ZZRT_')
        ( client = sy-mandt object = 'DEFAULT_DICT_RT_TYP'   value = 'X')
         ).

    INSERT zzt_abap_gen FROM TABLE mt_settings.
  ENDMETHOD.

  METHOD get_package_rangetypes.
    rv_package = mt_settings[ object = 'PACKAGE_RANGETYPES' ]-value.
  ENDMETHOD.

  METHOD get_selection_methodname.
    rv_methodname = mt_settings[ object = 'SELECTION_METHODNAME' ]-value.
  ENDMETHOD.

  METHOD get_template_packagename.
    DATA lv_packagename TYPE devclass.
    rv_packagename = |{ mt_settings[ object = 'TEMPLATE_PREFIX' ]-value }{ iv_template }|.
    rv_packagename = to_upper( rv_packagename ).
  ENDMETHOD.

  METHOD get_report_sel_start.
    rv_result = mt_settings[ object = 'REPORT_SEL_START' ]-value.
  ENDMETHOD.
  METHOD get_report_sel_end.
    rv_result = mt_settings[ object = 'REPORT_SEL_END' ]-value.
  ENDMETHOD.
  METHOD get_report_set_start.
    rv_result = mt_settings[ object = 'REPORT_SET_START' ]-value.
  ENDMETHOD.
  METHOD get_report_set_end.
    rv_result = mt_settings[ object = 'REPORT_SET_END' ]-value.
  ENDMETHOD.

  METHOD get_default_namespace.
    rv_result = mt_settings[ object = 'NAMESPACE' ]-value.
  ENDMETHOD.
  METHOD get_default_dlvunit.
    rv_result = mt_settings[ object = 'DLVUNIT' ]-value.
  ENDMETHOD.
  METHOD get_default_model_suffix.
    rv_result = mt_settings[ object = 'CLASS_MODEL_SUFFIX' ]-value.
  ENDMETHOD.
  METHOD get_default_dict_rt_type.
    rv_result = mt_settings[ object = 'DEFAULT_DICT_RT_TYP' ]-value.
  ENDMETHOD.

  METHOD get_default_prefix.
    rs_prefix-report_parameter = mt_settings[ object = 'REPORT_P_PREFIX' ]-value.
    rs_prefix-report_selopt = mt_settings[ object = 'REPORT_S_PREFIX' ]-value.
    rs_prefix-attribute = mt_settings[ object = 'ATTRIBUTE_PREFIX' ]-value.
    rs_prefix-class_parameter = mt_settings[ object = 'CLASS_P_PREFIX' ]-value.
    rs_prefix-class_selopt = mt_settings[ object = 'CLASS_S_PREFIX' ]-value.
    rs_prefix-class_rt_type = mt_settings[ object = 'CLASS_RT_TYP_PREFIX' ]-value.
    rs_prefix-dict_rt_type = mt_settings[ object = 'DICT_RT_TYP_PREFIX' ]-value.
  ENDMETHOD.

  METHOD init_technical_data.
    mt_technical_data = VALUE #(
      ( 'AUTHOR' )
      ( 'CREATEDON' )
      ( 'CHANGEDBY' )
      ( 'CHANGEDON' )
      ( 'CHGDANYBY' )
    ).
  ENDMETHOD.

  METHOD show_results.
    mo_report_generator->show_report( ).
  ENDMETHOD.





ENDCLASS.
