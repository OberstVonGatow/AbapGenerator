CLASS zcl_abap_generator_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        io_main_generator TYPE REF TO zcl_abap_generator.

    METHODS create_classes
      IMPORTING
        iv_package  TYPE devclass
        iv_template TYPE devclass
      RAISING
        zcx_abap_gen_class_read
        zcx_abap_gen_class_create.

    METHODS update_selection
      IMPORTING
        iv_packagename            TYPE packname
        iv_modelsufix             TYPE zzde_abap_modelsuffix
        is_prefix                 TYPE zzs_abap_gen_prefix
        it_selection              TYPE zztt_abap_gen_selection
        iv_dict_rangetab          TYPE abap_bool
      RETURNING
        VALUE(rv_model_classname) TYPE seoclsname
      RAISING
        zcx_abap_gen_class_update
        zcx_abap_gen_class_read.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_generator TYPE REF TO zcl_abap_generator.
    DATA: mt_upd_attributes TYPE seoo_attributes_r,
          mt_upd_parameters TYPE seos_parameters_r,
          mt_upd_types      TYPE seoo_types_r.

    METHODS read_classes
      IMPORTING
        !iv_package       TYPE devclass
      RETURNING
        VALUE(rt_clskeys) TYPE seo_clskeys .

    METHODS get_class_source_old
      IMPORTING
        !is_clskey       TYPE seoclskey
      RETURNING
        VALUE(rt_source) TYPE string_table.

    METHODS get_class_source_new
      IMPORTING
        !is_clskey       TYPE seoclskey
      RETURNING
        VALUE(rt_source) TYPE string_table
      RAISING
        cx_sy_dyn_call_error .

    METHODS create_class_rangetypes
      IMPORTING
        !iv_class     TYPE seoclskey-clsname
        !it_selection TYPE zztt_abap_gen_selection
      RAISING
        zcx_abap_gen_class_update.

    METHODS create_dict_rangetypes
      IMPORTING
        !it_selection TYPE zztt_abap_gen_selection
        !is_prefix    TYPE zzs_abap_gen_prefix
      RAISING
        zcx_abap_gen_class_update.

    METHODS read_existing_rangetypes
      IMPORTING
        !iv_packagename   TYPE devclass
      RETURNING
        VALUE(rt_ttyname) TYPE string_table.


    METHODS create_seldata_setmethod
      IMPORTING
        !iv_class     TYPE seoclsname
        !it_selection TYPE zztt_abap_gen_selection
        !is_prefix    TYPE zzs_abap_gen_prefix
      RAISING
        zcx_abap_gen_class_update.

    METHODS create_parameters
      IMPORTING
        !iv_class     TYPE seoclsname
        !iv_method    TYPE seocmpname
        !it_selection TYPE zztt_abap_gen_selection
        !is_prefix    TYPE zzs_abap_gen_prefix
      RAISING
        zcx_abap_gen_class_update.

    METHODS create_attributes
      IMPORTING
        !iv_class     TYPE seoclsname
        !it_selection TYPE zztt_abap_gen_selection
        !is_prefix    TYPE zzs_abap_gen_prefix
      RAISING
        zcx_abap_gen_class_update.

    METHODS generate_class
      IMPORTING
        iv_class TYPE seoclskey-clsname
      RAISING
        zcx_abap_gen_class_update.

    METHODS read_upd_class
      IMPORTING
        iv_classname TYPE seoclsname
      RAISING
        zcx_abap_gen_class_read.
    METHODS delete_attributes
      IMPORTING
        is_prefix TYPE zzs_abap_gen_prefix
      RAISING
        zcx_abap_gen_class_update.
    METHODS delete_parameters
      IMPORTING
        iv_method TYPE vseomethod-cmpname
        is_prefix TYPE zzs_abap_gen_prefix
      RAISING
        zcx_abap_gen_class_update.

ENDCLASS.



CLASS zcl_abap_generator_class IMPLEMENTATION.

  METHOD constructor.
    mo_generator = io_main_generator.
  ENDMETHOD.


  METHOD create_classes.

    DATA(lv_templatepackage) = mo_generator->get_template_packagename( iv_template ).

    DATA lt_clskeys TYPE seo_clskeys.
    lt_clskeys = read_classes( lv_templatepackage ).

    LOOP AT lt_clskeys REFERENCE INTO DATA(lr_clskey).

      DATA(ls_new_clskey) = lr_clskey->*.
      IF find( val = ls_new_clskey-clsname sub = iv_template ) = -1 .
        CONTINUE.
      ENDIF.

      DATA: class                     TYPE vseoclass,
            attributes                TYPE seoo_attributes_r,
            methods                   TYPE seoo_methods_r,
            events                    TYPE seoo_events_r,
            types                     TYPE seoo_types_r,
            parameters                TYPE seos_parameters_r,
            exceps                    TYPE seos_exceptions_r,
            implementings             TYPE seor_implementings_r,
            inheritance               TYPE vseoextend,
            redefinitions             TYPE seor_redefinitions_r,
            impl_details              TYPE seor_redefinitions_r,
            friendships               TYPE seof_friendships_r,
            typepusages               TYPE seot_typepusages_r,
            clsdeferrds               TYPE seot_clsdeferrds_r,
            intdeferrds               TYPE seot_intdeferrds_r,
            explore_inheritance       TYPE seok_cls_typeinfos,
            explore_implementings     TYPE seok_int_typeinfos,
            aliases                   TYPE seoo_aliases_r,
            enhancement_methods       TYPE enhmeth_tabheader,
            enhancement_attributes    TYPE enhclasstabattrib,
            enhancement_events        TYPE enhclasstabevent,
            enhancement_implementings TYPE enhclasstabimplementing,
            enhancement_types         TYPE enhtype_tab
            .

      CALL FUNCTION 'SEO_CLASS_TYPEINFO_GET'
        EXPORTING
          clskey                = lr_clskey->*
          version               = seoc_version_active
          state                 = '1'
          with_descriptions     = abap_true
*         resolve_eventhandler_typeinfo =
*         with_master_language  =
*         with_enhancements     =
*         read_active_enha      =
*         enha_action           =
*         ignore_switches       = 'X'
        IMPORTING
          class                 = class
          attributes            = attributes
          methods               = methods
          events                = events
          types                 = types
          parameters            = parameters
          exceps                = exceps
          implementings         = implementings
          inheritance           = inheritance
          redefinitions         = redefinitions
          impl_details          = impl_details
          friendships           = friendships
          typepusages           = typepusages
          clsdeferrds           = clsdeferrds
          intdeferrds           = intdeferrds
          explore_inheritance   = explore_inheritance
          explore_implementings = explore_implementings
          aliases               = aliases
*         enhancement_methods   = enhancement_methods
*         enhancement_attributes    = enhancement_attributes
*         enhancement_events    = enhancement_events
*         enhancement_implementings = enhancement_implementings
*         enhancement_types     = enhancement_types
        EXCEPTIONS
          not_existing          = 1
          is_interface          = 2
          model_only            = 3
          OTHERS                = 4.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_abap_gen_class_read
          MESSAGE ID zcx_abap_gen_class_read=>msgid
          NUMBER zcx_abap_gen_class_read=>msgno
          WITH lr_clskey->clsname.
      ENDIF.



      TRY.
          DATA(lt_source) = get_class_source_new( lr_clskey->* ).
        CATCH cx_sy_dyn_call_error.
          lt_source = get_class_source_old( lr_clskey->* ).
      ENDTRY.

      IF lt_source IS INITIAL.
        RAISE EXCEPTION TYPE zcx_abap_gen_class_read
          MESSAGE ID zcx_abap_gen_class_read=>msgid
          NUMBER zcx_abap_gen_class_read=>msgno
          WITH lr_clskey->clsname.
      ENDIF.

      DATA method_sources  TYPE seo_method_source_table.
      LOOP AT methods REFERENCE INTO DATA(lr_method).
        APPEND INITIAL LINE TO method_sources REFERENCE INTO DATA(lr_method_source).
        lr_method_source->cpdname = lr_method->cmpname.
        mo_generator->replace_template_string( REF #( lr_method_source->cpdname ) ).
        lr_method_source->redefine = lr_method->redefin.
        DATA(lv_found) = abap_false.
        LOOP AT lt_source REFERENCE INTO DATA(lr_source).
          IF lv_found = abap_false.
            DATA(lv_start_index) = find( val = lr_source->* sub = |METHOD { lr_method->cmpname }| case = abap_false ).
            IF lv_start_index <> -1.
              lv_found = abap_true.
            ENDIF.
            CONTINUE.
          ELSE.
            IF find( val = lr_source->* sub = |ENDMETHOD| case = abap_false ) <> -1.
              EXIT.
            ENDIF.
            mo_generator->replace_template_string( ir_string = REF #( lr_source->* ) iv_lower = abap_true ).
            APPEND lr_source->* TO lr_method_source->source.
          ENDIF.
        ENDLOOP.
        CALL FUNCTION 'PRETTY_PRINTER'
          EXPORTING
            inctoo             = abap_false
*           settings           =
*        IMPORTING
*           indentation_maybe_wrong =
          TABLES
            ntext              = lr_method_source->source
            otext              = lr_method_source->source
          EXCEPTIONS
            enqueue_table_full = 1
            include_enqueued   = 2
            include_readerror  = 3
            include_writeerror = 4
            OTHERS             = 5.
        IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ENDLOOP.



      mo_generator->replace_template_structure( REF #( class ) ).
      mo_generator->replace_template_structure( REF #( inheritance ) ).

      mo_generator->replace_template_table( REF #( redefinitions ) ).
      mo_generator->replace_template_table( REF #( attributes ) ).
      mo_generator->replace_template_table( REF #( methods ) ).
*      mo_generator->replace_template_table( REF #( implementings ) ).
      mo_generator->replace_template_table( REF #( events ) ).
*      mo_generator->replace_template_table( REF #( types ) ).
**         type_source                = ty
      mo_generator->replace_template_table( REF #( parameters ) ).
      mo_generator->replace_template_table( REF #( exceps ) ).
      mo_generator->replace_template_table( REF #( aliases ) ).
**         typepusages                = typepusages
      mo_generator->replace_template_table( REF #( clsdeferrds ) ).
      mo_generator->replace_template_table( REF #( intdeferrds ) ).
      mo_generator->replace_template_table( REF #( friendships ) ).

      class-author = sy-uname.


      CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
        EXPORTING
*         corrnr          =
          devclass        = iv_package
          version         = seoc_version_active
*         genflag         =
          authority_check = seox_true
          overwrite       = seox_true
*         suppress_method_generation =
*         suppress_refactoring_support   =
          method_sources  = method_sources
*         locals_def      =
*         locals_imp      =
*         locals_mac      =
*         suppress_index_update      =
*         typesrc         =
*         suppress_corr   =
*         suppress_dialog =
*         lifecycle_manager          =
*         locals_au       =
*         lock_handle     =
*         suppress_unlock =
*         suppress_commit =
*         generate_method_impls_wo_frame =
*      IMPORTING
*         korrnr          =
*      TABLES
*         class_descriptions         =
*         component_descriptions     =
*         subcomponent_descriptions  =
        CHANGING
          class           = class
          inheritance     = inheritance
          redefinitions   = redefinitions
*         implementings   = implementings
*         impl_details    = impl_details
          attributes      = attributes
          methods         = methods
          events          = events
          types           = types
*         type_source     = type_source
          parameters      = parameters
          exceps          = exceps
          aliases         = aliases
*         typepusages     = typepusages
          clsdeferrds     = clsdeferrds
          intdeferrds     = intdeferrds
          friendships     = friendships
        EXCEPTIONS
          existing        = 1
          is_interface    = 2
          db_error        = 3
          component_error = 4
          no_access       = 5
          other           = 6
          OTHERS          = 7.
      IF sy-subrc = 1.
        CONTINUE.
      ELSEIF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_abap_gen_class_create
          MESSAGE ID zcx_abap_gen_class_create=>msgid
          NUMBER zcx_abap_gen_class_create=>msgno
          WITH class-clsname.
      ENDIF.



    ENDLOOP.
  ENDMETHOD.

  METHOD read_classes.

    cl_package=>load_package(
    EXPORTING
      i_package_name             = iv_package
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
      cl_package=>load_package(
      EXPORTING
        i_package_name             = |${ iv_package }|
*        i_force_reload             =
      IMPORTING
        e_package                  = lo_package
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
    ENDIF.

    lo_package->get_elements(
      EXPORTING
        i_check_existence = abap_true
      IMPORTING
        e_elements        = DATA(lt_elements)
    ).

    LOOP AT lt_elements REFERENCE INTO DATA(lr_element).

      IF lr_element->*->dev_elem_type = 'CLAS'.
        rt_clskeys = VALUE #( BASE rt_clskeys ( clsname = lr_element->*->dev_elem_key ) ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_class_source_new.
    "Copy from ABAP-Git (ZCL_ABAPGIT_OO_SERIALIZER->serialize_abap_new)
    DATA: lo_source   TYPE REF TO object,
          lo_instance TYPE REF TO object.

* do not call the class/methods statically, as it will
* give syntax errors on old versions
    CALL METHOD ('CL_OO_FACTORY')=>('CREATE_INSTANCE')
      RECEIVING
        result = lo_instance.

    CALL METHOD lo_instance->('CREATE_CLIF_SOURCE')
      EXPORTING
        clif_name = is_clskey-clsname
        version   = 'A'
      RECEIVING
        result    = lo_source.

    CALL METHOD lo_source->('GET_SOURCE')
      IMPORTING
        source = rt_source.
  ENDMETHOD.

  METHOD get_class_source_old.
    "Copy from ABAP-Git (ZCL_ABAPGIT_OO_SERIALIZER->serialize_abap_old)
* for old ABAP AS versions
    DATA: lo_source TYPE REF TO cl_oo_source.

    CREATE OBJECT lo_source
      EXPORTING
        clskey             = is_clskey
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lo_source->read( 'A' ).
    rt_source = lo_source->get_old_source( ).

  ENDMETHOD.


  METHOD update_selection.

    DATA(lt_classes) = read_classes( iv_package = iv_packagename ).
    LOOP AT lt_classes REFERENCE INTO DATA(lr_class).
      IF find( val = lr_class->clsname sub = iv_modelsufix ) <> -1 .
        rv_model_classname = lr_class->clsname.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF rv_model_classname IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abap_gen_class_update
        MESSAGE ID zcx_abap_gen_class_update=>msgid
        NUMBER zcx_abap_gen_class_update=>msgno.
    ENDIF.

    read_upd_class( rv_model_classname ).

    IF iv_dict_rangetab = abap_true.
      create_dict_rangetypes(
        EXPORTING
          it_selection = it_selection
          is_prefix    = is_prefix
      ).
    ELSE.
      create_class_rangetypes(
        EXPORTING
          iv_class       = rv_model_classname
          it_selection   = it_selection
      ).
    ENDIF.


    create_seldata_setmethod(
      EXPORTING
        iv_class          = rv_model_classname
        it_selection      = it_selection
        is_prefix         = is_prefix
    ).

    create_attributes(
      EXPORTING
        iv_class          = rv_model_classname
        it_selection      = it_selection
        is_prefix         = is_prefix
    ).

    delete_attributes(
      EXPORTING
        is_prefix         = is_prefix
    ).


    generate_class( iv_class = rv_model_classname ).

  ENDMETHOD.


  METHOD create_class_rangetypes.
    DATA ls_cifkey  TYPE seoclskey .
    ls_cifkey-clsname = iv_class.

    DATA lt_types  TYPE seoo_types_w.
    DATA ls_type LIKE LINE OF lt_types.

    ls_type-clsname = iv_class.
    ls_type-exposure =  2. "Public
    ls_type-version = 1. "Active
    ls_type-state = 1. "Implemented
    ls_type-typtype = 4. "See code

    LOOP AT it_selection REFERENCE INTO DATA(lr_selection)
    WHERE codetype = mo_generator->c_codetype_selops.

      ls_type-cmpname = lr_selection->typename.
      ls_type-typesrc = |{ lr_selection->typename CASE = LOWER } TYPE RANGE OF { lr_selection->dictype CASE = LOWER }|.
      READ TABLE mt_upd_types REFERENCE INTO DATA(lr_upd_types)
      WITH KEY clsname = ls_type-clsname
               cmpname = ls_type-cmpname.
      IF sy-subrc = 0.
        DELETE TABLE mt_upd_types FROM lr_upd_types->*.
        CONTINUE.
      ENDIF.

      APPEND ls_type TO lt_types.
    ENDLOOP.

    IF lt_types IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SEO_TYPE_CREATE'
      EXPORTING
        cifkey            = ls_cifkey
        types             = lt_types
      EXCEPTIONS
        clif_not_existing = 1
        not_specified     = 2
        existing          = 3
        not_all_inserted  = 4
        internal_error    = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_gen_class_update
        MESSAGE ID zcx_abap_gen_class_update=>msgid
        NUMBER zcx_abap_gen_class_update=>msgno
        WITH iv_class.
    ENDIF.

  ENDMETHOD.

  METHOD create_seldata_setmethod.
    DATA ls_cifkey  TYPE seoclskey .
    DATA lt_methods  TYPE seoo_methods_w.
    DATA ls_method TYPE  seoo_method_w.

    ls_cifkey-clsname = iv_class.
    ls_method-clsname = iv_class.
    ls_method-version = '1'.
    ls_method-langu = sy-langu.
    ls_method-exposure = '2'. "Public.
    ls_method-state = '1'. "realisiert.
    ls_method-editorder = '1'.
    ls_method-cmpname = mo_generator->get_selection_methodname( ).
    ls_method-descript = 'Set Selection Screen Data'.

    APPEND ls_method TO lt_methods.

    CALL FUNCTION 'SEO_METHOD_CREATE'
      EXPORTING
        cifkey            = ls_cifkey
        methods           = lt_methods
      EXCEPTIONS
        clif_not_existing = 1
        not_specified     = 2
        existing          = 3
        not_all_inserted  = 4
        internal_error    = 5
        OTHERS            = 6.
    IF sy-subrc <> 0
    AND sy-subrc <> 3.
      RAISE EXCEPTION TYPE zcx_abap_gen_class_update
        MESSAGE ID zcx_abap_gen_class_update=>msgid
        NUMBER zcx_abap_gen_class_update=>msgno
        WITH iv_class.
    ENDIF.


    DATA ls_mtdkey  TYPE seocpdkey.
    ls_mtdkey-clsname = iv_class.
    ls_mtdkey-cpdname = ls_method-cmpname.

    DATA lt_implementation  TYPE seop_source.
    LOOP AT it_selection REFERENCE INTO DATA(lr_selection).

      APPEND INITIAL LINE TO lt_implementation REFERENCE INTO DATA(lr_implementation).

      CASE lr_selection->codetype.
        WHEN zcl_abap_generator=>c_codetype_parameters.
          lr_implementation->* = |{ is_prefix-attribute }{ is_prefix-class_parameter }{ lr_selection->name } = i{ is_prefix-class_parameter }{ lr_selection->name }.|.
        WHEN zcl_abap_generator=>c_codetype_selops.
          lr_implementation->* = |{ is_prefix-attribute  }{ is_prefix-class_selopt }{ lr_selection->name } = i{ is_prefix-class_selopt }{ lr_selection->name }.|.
      ENDCASE.
      lr_implementation->* = to_lower( lr_implementation->* ).
    ENDLOOP.

    CALL FUNCTION 'SEO_METHOD_GENERATE_INCLUDE'
      EXPORTING
*       lifecycle_manager              =
*       suppress_modification_support  =
*       enhancement                    = space
*       extend                         =
*       suppress_index_update          =
*       with_super_call                =
*       without_method_frame           =
*       corrnr                         =
*       generated                      =
*       editor_lock                    = seox_false
*       suppress_mtdkey_check          =
        implementation                 = lt_implementation
*       implementation_expanded        =
*       suppress_corr                  =
*       redefine                       =
        force                          = abap_true
*       version                        = seoc_version_active
        mtdkey                         = ls_mtdkey
      EXCEPTIONS
        not_existing                   = 1
        model_only                     = 2
        include_existing               = 3
        method_imp_not_generated       = 4
        method_imp_not_initialised     = 5
        _internal_class_not_existing   = 6
        _internal_method_overflow      = 7
        cancelled                      = 8
        method_is_abstract_implemented = 9
        method_is_final_implemented    = 10
        internal_error_insert_report   = 11
        OTHERS                         = 12.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_gen_class_update
        MESSAGE ID zcx_abap_gen_class_update=>msgid
        NUMBER zcx_abap_gen_class_update=>msgno
        WITH iv_class.
    ENDIF.

    create_parameters(
      EXPORTING
        iv_class          = iv_class
        iv_method         = ls_method-cmpname
        it_selection      = it_selection
        is_prefix         = is_prefix
    ).

    delete_parameters(
       EXPORTING
        iv_method         = ls_method-cmpname
        is_prefix         = is_prefix ).


  ENDMETHOD.


  METHOD create_attributes.

    DATA lt_attributes  TYPE seoo_attributes_w.
    DATA lv_sconame TYPE seosconame.
    DATA lv_type TYPE rs38l_typ.
    DATA ls_attkey  TYPE seocmpkey.

    LOOP AT it_selection REFERENCE INTO DATA(lr_selection).

      CASE lr_selection->codetype.
        WHEN zcl_abap_generator=>c_codetype_parameters.
          lv_sconame = |{ is_prefix-attribute }{ is_prefix-class_parameter }{ lr_selection->name }|.
          lv_type = lr_selection->dictype.
        WHEN zcl_abap_generator=>c_codetype_selops.
          lv_sconame = |{ is_prefix-attribute }{ is_prefix-class_selopt }{ lr_selection->name }|.
          lv_type = lr_selection->typename.
      ENDCASE.
      lv_sconame = to_lower( lv_sconame ).
      lv_type = to_lower( lv_type ).

      ls_attkey-clsname = iv_class.
      ls_attkey-cmpname = lv_sconame.


      READ TABLE mt_upd_attributes REFERENCE INTO DATA(lr_upd_att)
      WITH KEY clsname = ls_attkey-clsname
               cmpname = ls_attkey-cmpname.
      IF sy-subrc = 0.
        DELETE TABLE mt_upd_attributes FROM lr_upd_att->*.
        CONTINUE.
      ENDIF.

      lt_attributes = VALUE #( BASE lt_attributes (
          clsname = iv_class
          cmpname = lv_sconame
          version = '1'"Active
          exposure = '0' "Private
          state = '1' "realisiert
          attdecltyp = '0' "Instanzattribut
          typtype = '1' " Type
          type = lv_type
       ) ).

    ENDLOOP.

    IF lt_attributes IS INITIAL.
      RETURN.
    ENDIF.

    DATA ls_cifkey TYPE seoclskey.
    ls_cifkey-clsname = iv_class.

    CALL FUNCTION 'SEO_ATTRIBUTE_CREATE'
      EXPORTING
        cifkey            = ls_cifkey
        attributes        = lt_attributes
      EXCEPTIONS
        clif_not_existing = 1
        not_specified     = 2
        existing          = 3
        not_all_inserted  = 4
        internal_error    = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_gen_class_update
        MESSAGE ID zcx_abap_gen_class_update=>msgid
        NUMBER zcx_abap_gen_class_update=>msgno
        WITH iv_class.
    ENDIF.

  ENDMETHOD.

  METHOD create_parameters.

    DATA lt_parameters  TYPE seos_parameters_w.
    DATA ls_cmpkey  TYPE seocmpkey.
    DATA lv_sconame TYPE seosconame.
    DATA lv_type TYPE rs38l_typ.
    DATA ls_parkey  TYPE seos_parameter_key.

    ls_cmpkey-clsname = iv_class.
    ls_cmpkey-cmpname = iv_method.

    LOOP AT it_selection REFERENCE INTO DATA(lr_selection).

      CASE lr_selection->codetype.
        WHEN zcl_abap_generator=>c_codetype_parameters.
          lv_sconame = |i{ is_prefix-class_parameter }{ lr_selection->name }|.
          lv_type = lr_selection->dictype.
        WHEN zcl_abap_generator=>c_codetype_selops.
          lv_sconame = |i{ is_prefix-class_selopt }{ lr_selection->name }|.
          lv_type = lr_selection->typename.
      ENDCASE.
      lv_sconame = to_lower( lv_sconame ).
      lv_type = to_lower( lv_type ).

      ls_parkey-clsname = iv_class.
      ls_parkey-cmpname = iv_method.
      ls_parkey-sconame = lv_sconame.

      READ TABLE mt_upd_parameters REFERENCE INTO DATA(lr_upd_para)
      WITH KEY clsname = ls_parkey-clsname
               cmpname = ls_parkey-cmpname
               sconame = ls_parkey-sconame.
      IF sy-subrc = 0.
        DELETE TABLE mt_upd_parameters FROM lr_upd_para->*.
        CONTINUE.
      ENDIF.

      lt_parameters = VALUE #( BASE lt_parameters (
          cmpname = iv_method
          clsname = iv_class
          version = '1'"Active
          pardecltyp = '0' " Importing
          parpasstyp = '1' " Reference
          sconame = lv_sconame
          typtype = '1' " Type
          type = lv_type
       ) ).

    ENDLOOP.

    CALL FUNCTION 'SEO_PARAMETER_CREATE'
      EXPORTING
        cmpkey                 = ls_cmpkey
        parameters             = lt_parameters
      EXCEPTIONS
        component_not_existing = 1
        not_specified          = 2
        existing               = 3
        not_all_inserted       = 4
        internal_error         = 5
        OTHERS                 = 6.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_gen_class_update
        MESSAGE ID zcx_abap_gen_class_update=>msgid
        NUMBER zcx_abap_gen_class_update=>msgno
        WITH iv_class.
    ENDIF.


  ENDMETHOD.

  METHOD generate_class.

    DATA  clskey  TYPE seoclskey.
    clskey-clsname = iv_class.

    CALL FUNCTION 'SEO_CLASS_GENERATE_SECTIONS'
      EXPORTING
        clskey                        = clskey
        public                        = abap_true
        protected                     = abap_true
        private                       = abap_true
*       suppress_corr                 =
*       return_generated_sections_only =
*       typeinfo                      =
*       line_size                     = 255
*       suppress_index_update         =
*       suppress_comments             =
*  IMPORTING
*       pubsec_source                 =
*       prosec_source                 =
*       prisec_source                 =
      EXCEPTIONS
        not_existing                  = 1
        model_only                    = 2
        public_sec_not_generated      = 3
        protected_sec_not_generated   = 4
        private_sec_not_generated     = 5
        public_sec_not_initialised    = 6
        protected_sec_not_initialised = 7
        private_sec_not_initialised   = 8
        _internal_class_not_existing  = 9
        OTHERS                        = 10.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_gen_class_update
        MESSAGE ID zcx_abap_gen_class_update=>msgid
        NUMBER zcx_abap_gen_class_update=>msgno
        WITH iv_class.
    ENDIF.

  ENDMETHOD.

  METHOD create_dict_rangetypes.
    DATA lv_rangestructure TYPE tabname.

    DATA(lv_package_rangetypes) = mo_generator->get_package_rangetypes(  ).
    DATA(lt_rangetables) = read_existing_rangetypes( lv_package_rangetypes ).

    LOOP AT it_selection REFERENCE INTO DATA(lr_sel)
    WHERE codetype = zcl_abap_generator=>c_codetype_selops.

      IF line_exists( lt_rangetables[ table_line = lr_sel->typename ] ).
        CONTINUE.
      ENDIF.

      lv_rangestructure = |{ is_prefix-rt_type }S_{ lr_sel->dictype }|.

      DATA  lt_fields_tab  TYPE STANDARD TABLE OF dd03p.
      lt_fields_tab = VALUE #(
                  ( tabname = lv_rangestructure fieldname = 'SIGN' position = 0001 rollname = 'DDSIGN' comptype = 'E'  )
                  ( tabname = lv_rangestructure fieldname = 'OPTION' position = 0002 rollname = 'DDOPTION' comptype = 'E'  )
                  ( tabname = lv_rangestructure fieldname = 'LOW' position = 0003 rollname = lr_sel->dictype comptype = 'E'  )
                  ( tabname = lv_rangestructure fieldname = 'HIGH' position = 0004 rollname = lr_sel->dictype comptype = 'E'  )
               ).

      CALL FUNCTION 'RS_CORR_INSERT'
        EXPORTING
          object              = |TABL{ lv_rangestructure }|
          object_class        = 'DICT'
          mode                = 'I'
*         global_lock         = space
          devclass            = lv_package_rangetypes
*         korrnum             = space
*         use_korrnum_immediatedly = space
*         author              = space
          master_language     = sy-langu
*         genflag             = space
*         program             = space
*         object_class_supports_ma = space
*         extend              = space
          suppress_dialog     = abap_false
*         mod_langu           = space
*         activation_call     = space
*      IMPORTING
*         devclass            =
*         korrnum             =
*         ordernum            =
*         new_corr_entry      =
*         author              =
*         transport_key       =
*         new_extend          =
        EXCEPTIONS
          cancelled           = 1
          permission_failure  = 2
          unknown_objectclass = 3
          OTHERS              = 4.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_abap_gen_class_update.
      ENDIF.

      DATA ls_structure TYPE dd02v.
      ls_structure-tabname = lv_rangestructure.
      ls_structure-tabclass = 'INTTAB'. "Structure
      ls_structure-ddtext = |Range Structure { lr_sel->dictype }|.
      ls_structure-ddlanguage = sy-langu.

      CALL FUNCTION 'DDIF_TABL_PUT'
        EXPORTING
          name              = CONV ddobjname( lv_rangestructure )
          dd02v_wa          = ls_structure
*         dd09l_wa          = ' '
        TABLES
          dd03p_tab         = lt_fields_tab
*         dd05m_tab         =
*         dd08v_tab         =
*         dd35v_tab         =
*         dd36m_tab         =
        EXCEPTIONS
          tabl_not_found    = 1
          name_inconsistent = 2
          tabl_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_abap_gen_class_update.
      ENDIF.


      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_tadir_pgmid    = 'R3TR'
          wi_tadir_object   = 'TABL'
          wi_tadir_obj_name = CONV sobj_name( lv_rangestructure )
          wi_set_genflag    = abap_true
          wi_test_modus     = abap_false
          wi_tadir_devclass = lv_package_rangetypes
        EXCEPTIONS
          OTHERS            = 1.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_abap_gen_class_update.
      ENDIF.

      DATA lv_rc TYPE syst_subrc.
      CALL FUNCTION 'DDIF_TABL_ACTIVATE'
        EXPORTING
          name        = CONV ddobjname( lv_rangestructure )
*         auth_chk    = 'X'
*         prid        = -1
*         excommit    = 'X'
        IMPORTING
          rc          = lv_rc
        EXCEPTIONS
          not_found   = 1
          put_failure = 2
          OTHERS      = 3.
      IF sy-subrc <> 0 OR lv_rc = 8.
        RAISE EXCEPTION TYPE zcx_abap_gen_class_update.
      ENDIF.


      CALL FUNCTION 'RS_CORR_INSERT'
        EXPORTING
          object              = |TTYP{ lr_sel->typename }|
          object_class        = 'DICT'
          mode                = 'I'
*         global_lock         = space
          devclass            = lv_package_rangetypes
*         korrnum             = space
*         use_korrnum_immediatedly = space
*         author              = space
          master_language     = sy-langu
*         genflag             = space
*         program             = space
*         object_class_supports_ma = space
*         extend              = space
          suppress_dialog     = abap_false
*         mod_langu           = space
*         activation_call     = space
*      IMPORTING
*         devclass            =
*         korrnum             =
*         ordernum            =
*         new_corr_entry      =
*         author              =
*         transport_key       =
*         new_extend          =
        EXCEPTIONS
          cancelled           = 1
          permission_failure  = 2
          unknown_objectclass = 3
          OTHERS              = 4.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_abap_gen_class_update.
      ENDIF.

      DATA ls_dd40v  TYPE dd40v.

      ls_dd40v-typename = lr_sel->typename.
      ls_dd40v-ttypkind = 'R'.
      ls_dd40v-range_ctyp = lr_sel->dictype.
      ls_dd40v-ddlanguage = sy-langu.
      ls_dd40v-ddtext = |Rangetable { lr_sel->dictype }|.
      ls_dd40v-rowtype = lv_rangestructure.

      CALL FUNCTION 'DDIF_TTYP_PUT'
        EXPORTING
          name              = CONV ddobjname( lr_sel->typename )
          dd40v_wa          = ls_dd40v
*        TABLES
*         dd42v_tab         = lt_dd42v
*         dd43v_tab         = lt_dd43v
        EXCEPTIONS
          ttyp_not_found    = 1
          name_inconsistent = 2
          ttyp_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.

      CALL FUNCTION 'DDIF_TTYP_ACTIVATE'
        EXPORTING
          name        = CONV ddobjname( lr_sel->typename )
*         prid        = -1
        IMPORTING
          rc          = lv_rc
        EXCEPTIONS
          not_found   = 1
          put_failure = 2
          OTHERS      = 3.
      IF sy-subrc <> 0 OR lv_rc = 8.
        RAISE EXCEPTION TYPE zcx_abap_gen_class_update.
      ENDIF.


    ENDLOOP.

  ENDMETHOD.


  METHOD read_existing_rangetypes.


    cl_package=>load_package(
    EXPORTING
      i_package_name             = iv_packagename
      i_force_reload             = abap_true
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
      cl_package=>load_package(
      EXPORTING
        i_package_name             = |${ iv_packagename }|
*        i_force_reload             =
      IMPORTING
        e_package                  = lo_package
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
    ENDIF.

    lo_package->get_elements(
      EXPORTING
        i_check_existence = abap_true
      IMPORTING
        e_elements        = DATA(lt_elements)
    ).

    LOOP AT lt_elements REFERENCE INTO DATA(lr_element).

      IF lr_element->*->dev_elem_type = 'TTYP'.
        rt_ttyname = VALUE #( BASE rt_ttyname ( CONV #( lr_element->*->dev_elem_key ) ) ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.



  METHOD read_upd_class.

    DATA ls_clskey  TYPE seoclskey.
    ls_clskey-clsname = iv_classname.

    CALL FUNCTION 'SEO_CLASS_TYPEINFO_GET'
      EXPORTING
        clskey            = ls_clskey
        version           = seoc_version_active
        state             = '1'
        with_descriptions = abap_false
*       resolve_eventhandler_typeinfo =
*       with_master_language  =
*       with_enhancements =
*       read_active_enha  =
*       enha_action       =
*       ignore_switches   = 'X'
      IMPORTING
*       class             = class
        attributes        = mt_upd_attributes
*       methods           = methods
*       events            = events
        types             = mt_upd_types
        parameters        = mt_upd_parameters
*       exceps            = exceps
*       implementings     = implementings
*       inheritance       = inheritance
*       redefinitions     = redefinitions
*       impl_details      = impl_details
*       friendships       = friendships
*       typepusages       = typepusages
*       clsdeferrds       = clsdeferrds
*       intdeferrds       = intdeferrds
*       explore_inheritance   = explore_inheritance
*       explore_implementings = explore_implementings
*       aliases           = aliases
*       enhancement_methods   = enhancement_methods
*       enhancement_attributes    = enhancement_attributes
*       enhancement_events    = enhancement_events
*       enhancement_implementings = enhancement_implementings
*       enhancement_types = enhancement_types
      EXCEPTIONS
        not_existing      = 1
        is_interface      = 2
        model_only        = 3
        OTHERS            = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_gen_class_read
        MESSAGE ID zcx_abap_gen_class_read=>msgid
        NUMBER zcx_abap_gen_class_read=>msgno
        WITH iv_classname.
    ENDIF.

  ENDMETHOD.

  METHOD delete_attributes.
    DATA lt_del_attkeys  TYPE seoo_attribute_keys.

    DATA(lv_prefix_para) = |{ is_prefix-attribute }{ is_prefix-class_parameter }|.
    DATA(lv_prefix_selopt) = |{ is_prefix-attribute }{ is_prefix-class_selopt }|.

    LOOP AT mt_upd_attributes REFERENCE INTO DATA(lr_attr).
      IF find( val = lr_attr->cmpname sub = lv_prefix_para case = abap_false ) <> -1
      OR find( val = lr_attr->cmpname sub = lv_prefix_selopt case = abap_false ) <> -1 .
        lt_del_attkeys = VALUE #( BASE lt_del_attkeys ( clsname = lr_attr->clsname cmpname = lr_attr->cmpname ) ).
        DATA(lv_class) = lr_attr->clsname.
      ENDIF.
    ENDLOOP.

    IF lt_del_attkeys IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SEO_ATTRIBUTE_DELETE'
      EXPORTING
*       cifkey            =
        attkeys           = lt_del_attkeys
      EXCEPTIONS
        clif_not_existing = 1
        not_specified     = 2
        not_existing      = 3
        OTHERS            = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_gen_class_update
        MESSAGE ID zcx_abap_gen_class_update=>msgid
        NUMBER zcx_abap_gen_class_update=>msgno
        WITH lv_class.
    ENDIF.

  ENDMETHOD.

  METHOD delete_parameters.
    DATA lt_del_parkeys  TYPE seos_parameter_keys.

    DATA(lv_prefix_para) = |i{ is_prefix-class_parameter }|.
    DATA(lv_prefix_selopt) = |i{ is_prefix-class_selopt }|.

    LOOP AT mt_upd_parameters REFERENCE INTO DATA(lr_para).
      IF find( val = lr_para->sconame sub = lv_prefix_para case = abap_false ) <> -1
      OR find( val = lr_para->sconame sub = lv_prefix_selopt case = abap_false ) <> -1 .
        lt_del_parkeys = VALUE #( BASE lt_del_parkeys ( clsname = lr_para->clsname cmpname = lr_para->cmpname sconame = lr_para->sconame ) ).
        DATA(lv_class) = lr_para->clsname.
      ENDIF.
    ENDLOOP.

    IF lt_del_parkeys IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SEO_PARAMETER_DELETE'
      EXPORTING
*       cmpkey                 =
        parkeys                = lt_del_parkeys
      EXCEPTIONS
        component_not_existing = 1
        not_specified          = 2
        not_existing           = 3
        OTHERS                 = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_gen_class_update
        MESSAGE ID zcx_abap_gen_class_update=>msgid
        NUMBER zcx_abap_gen_class_update=>msgno
        WITH lv_class.
    ENDIF.


  ENDMETHOD.

ENDCLASS.
