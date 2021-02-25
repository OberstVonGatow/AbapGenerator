*&---------------------------------------------------------------------*
*& Report zabap_generator
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap_generator.


PARAMETERS: p_init TYPE abap_bool RADIOBUTTON GROUP mode USER-COMMAND chg_mode DEFAULT 'X',
            p_upd  TYPE abap_bool RADIOBUTTON GROUP mode.

PARAMETERS p_local AS CHECKBOX USER-COMMAND local.

PARAMETERS p_name TYPE zzde_abap_name.
PARAMETERS p_nspace TYPE zzde_abap_namespace.

SELECTION-SCREEN BEGIN OF BLOCK init WITH FRAME TITLE TEXT-001.
  PARAMETERS p_pdesc TYPE as4text MODIF ID ini.
  PARAMETERS p_dlvuni TYPE dlvunit MODIF ID ini.
  PARAMETERS p_parent TYPE packparent MODIF ID ini.
  PARAMETERS p_temp TYPE devclass MODIF ID ini DEFAULT 'TEMPLATE001'.
SELECTION-SCREEN END OF BLOCK init.

SELECTION-SCREEN BEGIN OF BLOCK upd WITH FRAME TITLE TEXT-002.
  PARAMETERS p_msuf TYPE zzde_abap_modelsuffix MODIF ID upd.
  PARAMETERS p_rppref TYPE zzde_abap_report_paraprefix MODIF ID upd.
  PARAMETERS p_rspref TYPE zzde_abap_report_selopprefix MODIF ID upd.
  PARAMETERS p_capref TYPE zzde_abap_attribute_prefix MODIF ID upd.
  PARAMETERS p_cppref TYPE zzde_abap_class_paraprefix MODIF ID upd.
  PARAMETERS p_cspref TYPE zzde_abap_class_selopprefix MODIF ID upd.

  PARAMETERS p_rtdict AS CHECKBOX MODIF ID upd USER-COMMAND rtdict DEFAULT 'X'.
  PARAMETERS p_typref TYPE zzde_abap_typerangetab_prefix MODIF ID upd.

SELECTION-SCREEN END OF BLOCK upd.

INITIALIZATION.

  DATA(lo_generator) = NEW zcl_abap_generator( ).
  p_dlvuni = lo_generator->get_default_dlvunit( ).
  p_msuf = lo_generator->get_default_model_suffix( ).
  p_nspace = lo_generator->get_default_namespace( ).

  DATA ls_prefix TYPE zzs_abap_gen_prefix.
  ls_prefix = lo_generator->get_default_prefix( ).
  p_rppref = ls_prefix-report_parameter.
  p_rspref = ls_prefix-report_selopt.
  p_capref = ls_prefix-attribute.
  p_cppref = ls_prefix-class_parameter.
  p_cspref = ls_prefix-class_selopt.

  p_rtdict = lo_generator->get_default_dict_rt_type( ).


AT SELECTION-SCREEN.
  IF p_rtdict = abap_true.
    p_typref = ls_prefix-dict_rt_type.
  ELSE.
    p_typref = ls_prefix-class_rt_type.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.

  IF p_local = abap_true.
    p_dlvuni = 'LOCAL'.
    p_parent = '$TMP'.
  ELSE.
    p_dlvuni = lo_generator->get_default_dlvunit( ).
    CLEAR p_parent.
  ENDIF.

  LOOP AT SCREEN.

    IF screen-name = 'P_DLVUNI'
    OR screen-name = 'P_PARENT'.
      IF p_local = abap_true
      OR p_upd = abap_true.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.

    CASE screen-group1.
      WHEN 'INI'.
        IF p_init = abap_true.
          screen-input = 1.
        ELSE.
          screen-input = 0.
        ENDIF.
        MODIFY SCREEN.
      WHEN 'UPD'.
        IF p_upd = abap_true.
          screen-input = 1.
        ELSE.
          screen-input = 0.
        ENDIF.
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.

START-OF-SELECTION.
  DATA lcx_exception TYPE REF TO zcx_abap_gen.

  TRY.
      lo_generator->set_main_attributes(
                iv_name               = p_name
                iv_namespace          = p_nspace
                iv_local              = p_local
              ).
    CATCH zcx_abap_gen INTO lcx_exception.
      MESSAGE lcx_exception TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

  IF p_init = abap_true.
    TRY.
        lo_generator->run_initialization(
          EXPORTING
            iv_packagedesc = p_pdesc
            iv_dlvunit     = p_dlvuni
            iv_parentcl    = p_parent
            iv_template    = p_temp
        ).
        MESSAGE s009(zmc_abap_generator).

        lo_generator->show_results(  ).

      CATCH zcx_abap_gen INTO lcx_exception.
        MESSAGE lcx_exception TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

  ELSEIF p_upd = abap_true.

    ls_prefix-report_parameter = p_rppref.
    ls_prefix-report_selopt = p_rspref.
    ls_prefix-attribute = p_capref.
    ls_prefix-class_parameter = p_cppref.
    ls_prefix-class_selopt = p_cspref.
    ls_prefix-rt_type = p_typref.

    TRY.
        lo_generator->update_selection(
          EXPORTING
            iv_modelsufix       = p_msuf
            is_prefix           = ls_prefix
            iv_dict_rangetab    = p_rtdict
        ).
        MESSAGE s010(zmc_abap_generator).
        lo_generator->show_results(  ).

      CATCH zcx_abap_gen INTO lcx_exception.
        MESSAGE lcx_exception TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
  ENDIF.
