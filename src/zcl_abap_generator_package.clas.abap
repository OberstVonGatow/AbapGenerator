CLASS zcl_abap_generator_package DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS Constructor
      IMPORTING
        iv_name      TYPE zzde_abap_name
        iv_namespace TYPE zzde_abap_namespace
        iv_local     TYPE zzde_abap_local.

    METHODS create_package
      IMPORTING
        iv_packagedesc TYPE as4text
        iv_dlvunit     TYPE dlvunit
        iv_parentcl    TYPE packparent OPTIONAL
      RAISING
        zcx_abap_gen_package .

    METHODS: get_new_packagename RETURNING VALUE(r_result) TYPE packname.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_new_packagename  TYPE packname.

    METHODS build_new_packagename
      IMPORTING
        iv_namespace          TYPE zzde_abap_namespace
        iv_name               TYPE zzde_abap_name
        iv_local              TYPE zzde_abap_local
      RETURNING
        VALUE(rv_packagename) TYPE packname .


ENDCLASS.



CLASS zcl_abap_generator_package IMPLEMENTATION.

  METHOD constructor.

    mv_new_packagename = build_new_packagename(
            iv_namespace = iv_namespace
            iv_name      = iv_name
            iv_local     = iv_local
        ).

  ENDMETHOD.

  METHOD build_new_packagename.

    rv_packagename  =  iv_namespace && iv_name .
    IF iv_local = abap_true.
      rv_packagename = |${ rv_packagename }|.
    ENDIF.

  ENDMETHOD.

  METHOD create_package.
    DATA lo_package  TYPE REF TO if_package.
    DATA ls_package_data  TYPE scompkdtln.

    ls_package_data-devclass = mv_new_packagename.
    ls_package_data-ctext = iv_packagedesc.
    ls_package_data-parentcl = iv_parentcl.
*    ls_package_data-component
*   ls_package_data-pdevclass = 'ZKED'.
    ls_package_data-dlvunit = iv_dlvunit.
    ls_package_data-as4user = sy-uname.


    cl_package_factory=>create_new_package(
      EXPORTING
        i_reuse_deleted_object       = 'X'
        i_suppress_dialog            = abap_false
      IMPORTING
        e_package                    = lo_package
      CHANGING
        c_package_data               = ls_package_data
      EXCEPTIONS
        object_already_existing      = 1
        object_just_created          = 2
        not_authorized               = 3
        wrong_name_prefix            = 4
        undefined_name               = 5
        reserved_local_name          = 6
        invalid_package_name         = 7
        short_text_missing           = 8
        software_component_invalid   = 9
        layer_invalid                = 10
        author_not_existing          = 11
        component_not_existing       = 12
        component_missing            = 13
        prefix_in_use                = 14
        unexpected_error             = 15
        intern_err                   = 16
        no_access                    = 17
        invalid_translation_depth    = 18
        wrong_mainpack_value         = 19
        superpackage_invalid         = 20
        error_in_cts_checks          = 21
        OTHERS                       = 22
    ).
    IF sy-subrc = '1'.
      RETURN.
    ELSEIF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_gen_package
        MESSAGE ID zcx_abap_gen_package=>msgid
        NUMBER zcx_abap_gen_package=>msgno
        WITH mv_new_packagename.
    ENDIF.

    DATA ls_save_sign  TYPE paksavsign.
    ls_save_sign-pack = ls_save_sign-permis = ls_save_sign-elems = ls_save_sign-interf = abap_true.

    lo_package->save_generic(
      EXPORTING
        i_save_sign           = ls_save_sign
*        i_transport_request   =
*        i_suppress_dialog     = ' '
*      IMPORTING
*        e_transport_request   =
      EXCEPTIONS
        cancelled_in_corr     = 1
        permission_failure    = 2
        object_not_changeable = 3
        object_invalid        = 4
        OTHERS                = 5
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_gen_package
        MESSAGE ID zcx_abap_gen_package=>msgid
        NUMBER zcx_abap_gen_package=>msgno
        WITH mv_new_packagename.
    ENDIF.
  ENDMETHOD.


  METHOD get_new_packagename.
    r_result = me->mv_new_packagename.
  ENDMETHOD.

ENDCLASS.
