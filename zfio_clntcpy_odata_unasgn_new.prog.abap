*&---------------------------------------------------------------------*
*& Report ZFIO_CLNTCPY_ODATA_UNASGN_NEW
*&---------------------------------------------------------------------*
*&
*& I834429 - 11272017
*& I834429 - 08272019
*&  Removed download to excel and switched to ALV grid
*&---------------------------------------------------------------------*
REPORT zfio_clntcpy_odata_unasgn_new.

DATA: gv_system_client_edit TYPE t000-cccoractiv,
      ls_mgdeam             TYPE /iwfnd/cl_mgw_inst_man_dba=>ty_gs_mgdeam,
      ls_alias              TYPE /iwfnd/s_mgw_reg_alias,
      lx_destin_finder      TYPE REF TO /iwfnd/cx_destin_finder,
      lx_cof                TYPE REF TO /iwfnd/cx_cof,
      lo_inst_man_dba       TYPE REF TO /iwfnd/cl_mgw_inst_man_dba,
      msg_text(255)         TYPE c.

DATA: g_s_log    TYPE bal_s_log,
      log_handle TYPE balloghndl,
      g_dummy    TYPE c.


DATA: ld_filename TYPE string,
      ld_path     TYPE string,
      ld_fullpath TYPE string,
      ld_result   TYPE i,
      gd_file     TYPE c.

DATA: p_dir TYPE string.

CONSTANTS: gc_no_client_change  TYPE t000-cccoractiv VALUE '2'.

*----------------------------------------------------------------------*
*** SELECTION SCREEN
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE TEXT-005.
PARAMETERS: rb_b0 RADIOBUTTON GROUP rb DEFAULT 'X',
            rb_b1 RADIOBUTTON GROUP rb,
            rb_b2 RADIOBUTTON GROUP rb,
            rb_b3 RADIOBUTTON GROUP rb.

SELECTION-SCREEN END OF BLOCK sel.

SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE  TEXT-001.

TABLES /iwfnd/i_med_srh.
SELECT-OPTIONS s_serv FOR /iwfnd/i_med_srh-srv_identifier NO INTERVALS MODIF ID p0.

PARAMETERS: p_alias LIKE /iwfnd/c_dfsyal-system_alias MATCHCODE OBJECT /iwfnd/sh_sap_sys_alias MODIF ID p0.

SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE  TEXT-002.

PARAMETERS: s_alias LIKE /iwfnd/c_dfsyal-system_alias MATCHCODE OBJECT /iwfnd/sh_sap_sys_alias MODIF ID p1,
            p_treq1 TYPE e071-trkorr MATCHCODE OBJECT irm_transport MODIF ID p1,
            c_chk   AS CHECKBOX DEFAULT ' ' MODIF ID p1.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE  TEXT-003.

PARAMETERS: s_alias1 LIKE /iwfnd/c_dfsyal-system_alias MATCHCODE OBJECT /iwfnd/sh_sap_sys_alias MODIF ID p2,
            s_alias2 LIKE /iwfnd/c_dfsyal-system_alias MATCHCODE OBJECT /iwfnd/sh_sap_sys_alias MODIF ID p2,
            p_treq2  TYPE e071-trkorr MATCHCODE OBJECT irm_transport MODIF ID p2,
            c_rep    AS CHECKBOX DEFAULT ' ' MODIF ID p2.

SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE  TEXT-004.

PARAMETERS: c_srvlst AS CHECKBOX DEFAULT ' ' MODIF ID p3.

SELECTION-SCREEN END OF BLOCK b3.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF rb_b0 EQ 'X' AND screen-group1 = 'P1'.
      screen-active = '0'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF rb_b0 EQ 'X' AND screen-group1 = 'P2'.
      screen-active = '0'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF rb_b0 EQ 'X' AND screen-group1 = 'P3'.
      screen-active = '0'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF rb_b1 EQ 'X' AND screen-group1 = 'P0'.
      screen-active = '0'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF rb_b1 EQ 'X' AND screen-group1 = 'P2'.
      screen-active = '0'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF rb_b1 EQ 'X' AND screen-group1 = 'P3'.
      screen-active = '0'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF rb_b2 EQ 'X' AND screen-group1 = 'P0'.
      screen-active = '0'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF rb_b2 EQ 'X' AND screen-group1 = 'P1'.
      screen-active = '0'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF rb_b2 EQ 'X' AND screen-group1 = 'P3'.
      screen-active = '0'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF rb_b3 EQ 'X' AND screen-group1 = 'P0'.
      screen-active = '0'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF rb_b3 EQ 'X' AND screen-group1 = 'P1'.
      screen-active = '0'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF rb_b3 EQ 'X' AND screen-group1 = 'P2'.
      screen-active = '0'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.

    CLEAR: s_serv, p_alias, s_alias, p_treq1, c_chk, s_alias1, s_alias2, p_treq2, c_rep, c_srvlst.
  ENDLOOP.

START-OF-SELECTION.

*----------------------------------------------------------------------*
*** MAIN PROGRAM
*----------------------------------------------------------------------*
  PERFORM verify_client_edit.

  IF c_srvlst = abap_true.
    PERFORM download_list.
    EXIT.
  ENDIF.

  PERFORM init_log.

  IF c_chk = abap_true.
    CLEAR msg_text.

    SELECT service_id FROM /iwfnd/c_mgdeam WHERE system_alias = @s_alias INTO @s_serv-low.
      s_serv-sign = 'I'.
      s_serv-option = 'EQ'.
      APPEND s_serv.
    ENDSELECT.

    IF s_serv IS NOT INITIAL.
      PERFORM unassign_system_alias USING s_alias.
    ELSE.
      CONCATENATE 'No alias assignments found for system alias:' s_alias INTO msg_text SEPARATED BY space.
      PERFORM msg_add USING 'E'.
    ENDIF.

  ELSE.

    IF p_alias = '*'.
      SELECT system_alias FROM /iwfnd/c_dfsyal INTO @p_alias.
        PERFORM unassign_system_alias USING p_alias.
      ENDSELECT.
    ELSE.
      IF p_alias IS NOT INITIAL.
        PERFORM unassign_system_alias USING p_alias.
      ENDIF.
    ENDIF.

  ENDIF.

  IF c_rep = abap_true.
    CLEAR msg_text.

    SELECT service_id FROM /iwfnd/c_mgdeam WHERE system_alias = @s_alias1 INTO @s_serv-low.
      s_serv-sign = 'I'.
      s_serv-option = 'EQ'.
      APPEND s_serv.
    ENDSELECT.

    IF s_serv IS NOT INITIAL.
      PERFORM replace_system_alias USING s_alias1 s_alias2.
    ELSE.
      CONCATENATE 'No alias assignments found for system alias:' s_alias1 INTO msg_text SEPARATED BY space.
      PERFORM msg_add USING 'E'.
    ENDIF.

  ENDIF.

  PERFORM save_log.

  PERFORM display_log.              "END PROGRAM

*----------------------------------------------------------------------*
***COPIED FROM INCLUDE /IWFND/R_MGW_REGISTRATION_F01.
***FORM VERIFY_CLIENT_EDIT
*----------------------------------------------------------------------*
FORM verify_client_edit.

  SELECT SINGLE cccoractiv FROM t000
  INTO    gv_system_client_edit    BYPASSING BUFFER
  WHERE mandt = sy-mandt.

  IF gv_system_client_edit = gc_no_client_change.
    MESSAGE i430(tk) WITH sy-mandt.
    EXIT.
  ENDIF.

ENDFORM.            "VERIFY_CLIENT_EDIT
*----------------------------------------------------------------------*
***ADJUSTED FROM INCLUDE /IWFND/R_MGW_REGISTRATION_F01.
***FORM  UNASSIGN_SYSTEM_ALIAS
*----------------------------------------------------------------------*
FORM unassign_system_alias USING VALUE(v_alias) TYPE /iwfnd/c_dfsyal-system_alias.

  DATA: flag         TYPE abap_bool,
        iv_transport TYPE trkorr.

  iv_transport = p_treq1.

  LOOP AT s_serv.

    CLEAR msg_text.

    ls_mgdeam-service_id = s_serv-low.
    ls_mgdeam-system_alias = v_alias.
    flag = abap_true.

    TRY.
        lo_inst_man_dba = /iwfnd/cl_mgw_inst_man_dba=>get_inst_man_dba( ).
        lo_inst_man_dba->delete_mgdeam( is_mgdeam = ls_mgdeam
                                        iv_transport = iv_transport ).

      CATCH /iwfnd/cx_destin_finder INTO lx_destin_finder.
        msg_text = lx_destin_finder->if_message~get_text( ).
        flag = abap_false.
        PERFORM msg_add USING 'E'.


      CATCH /iwfnd/cx_cof INTO lx_cof.
        msg_text = lx_destin_finder->if_message~get_text( ).
        flag = abap_false.
        PERFORM msg_add USING 'E'.

    ENDTRY.

    IF flag = abap_true.
      CONCATENATE 'Alias' v_alias 'deleted for service' s_serv-low INTO msg_text SEPARATED BY space.
      PERFORM msg_add USING 'I'.
    ENDIF.

  ENDLOOP.

ENDFORM.            "UNASSIGN_SYSTEM_ALIAS
*----------------------------------------------------------------------*
*** REPLACE SYSTEM ALIAS
***FORM REPLACE_SYSTEM_ALIAS
*----------------------------------------------------------------------*
FORM replace_system_alias USING v_alias1 TYPE /iwfnd/c_dfsyal-system_alias v_alias2 TYPE /iwfnd/c_dfsyal-system_alias.

  DATA: flag         TYPE abap_bool,
        iv_transport TYPE trkorr.

  iv_transport = p_treq2.

  LOOP AT s_serv.

    CLEAR msg_text.

    ls_mgdeam-service_id = s_serv-low.
    ls_mgdeam-system_alias = v_alias1.
    flag = abap_true.

*--Unassign
    TRY.
        lo_inst_man_dba = /iwfnd/cl_mgw_inst_man_dba=>get_inst_man_dba( ).
        lo_inst_man_dba->delete_mgdeam( is_mgdeam = ls_mgdeam
                                        iv_transport = iv_transport ).

      CATCH /iwfnd/cx_destin_finder INTO lx_destin_finder.
        msg_text = lx_destin_finder->if_message~get_text( ).
        flag = abap_false.
        PERFORM msg_add USING 'E'.


      CATCH /iwfnd/cx_cof INTO lx_cof.
        msg_text = lx_destin_finder->if_message~get_text( ).
        flag = abap_false.
        PERFORM msg_add USING 'E'.

    ENDTRY.

    IF flag = abap_true.
      CONCATENATE 'Alias' v_alias1 'deleted for service' s_serv-low INTO msg_text SEPARATED BY space.
      PERFORM msg_add USING 'W'.
    ENDIF.

*--Replace
    ls_mgdeam-system_alias = v_alias2.

    TRY.
        lo_inst_man_dba = /iwfnd/cl_mgw_inst_man_dba=>get_inst_man_dba( ).
        lo_inst_man_dba->create_mgdeam( is_mgdeam = ls_mgdeam
                                        iv_transport = iv_transport ).

      CATCH /iwfnd/cx_destin_finder INTO lx_destin_finder.
        msg_text = lx_destin_finder->if_message~get_text( ).
        flag = abap_false.
        PERFORM msg_add USING 'E'.


      CATCH /iwfnd/cx_cof INTO lx_cof.
        msg_text = lx_destin_finder->if_message~get_text( ).
        flag = abap_false.
        PERFORM msg_add USING 'E'.

    ENDTRY.

    IF flag = abap_true.
      CONCATENATE 'Alias' v_alias2 'replaced for service' s_serv-low INTO msg_text SEPARATED BY space.
      PERFORM msg_add USING 'I'.
    ENDIF.

  ENDLOOP.

ENDFORM.            "REPLACE_SYSTEM_ALIAS
*----------------------------------------------------------------------*
*** DISPLAY LOG FILE
***FORM DISPLAY_LOG
*----------------------------------------------------------------------*
FORM display_log.

  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXCEPTIONS
      OTHERS = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.            "DISPLAY_LOG
*&---------------------------------------------------------------------*
*** ADD MESSAGE
***FORM MSG_ADD
*&---------------------------------------------------------------------*
FORM msg_add USING VALUE(c_msgty) TYPE symsgty.

* Add this message to log file
  CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
    EXPORTING
      i_log_handle  = log_handle
      i_msgty       = c_msgty
      i_text        = msg_text
    EXCEPTIONS
      log_not_found = 0
      OTHERS        = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "MSG_ADD
*----------------------------------------------------------------------*
*** INITIALIZE LOG
***FORM INIT_LOG
*----------------------------------------------------------------------*
FORM init_log.

  g_s_log-extnumber = 'ZFIO_CLIENTCOPY_ODATA_UNASSIGN'.
  g_s_log-object    = '/IWFND/'.
  g_s_log-subobject = 'CLEANUP'.
  g_s_log-aluser    = sy-uname.
  g_s_log-alprog    = ''.

*** CREATE LOG
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log      = g_s_log
    IMPORTING
      e_log_handle = log_handle
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                "INIT_LOG
*----------------------------------------------------------------------*
*** SAVE LOG
***FORM SAVE_LOG
*----------------------------------------------------------------------*
FORM save_log.

  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_save_all       = 'X'
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                "SAVE_LOG

*----------------------------------------------------------------------*
*** DOWNLOAD LIST
***FORM DOWNLOAD LIST
*----------------------------------------------------------------------*
FORM download_list.

  DATA: BEGIN OF it_srvlst OCCURS 0,
          srv_identifier TYPE /iwfnd/med_mdl_srg_identifier,
          is_active      TYPE /iwfnd/med_mdl_active_flag,
          system_alias   TYPE /iwfnd/defi_system_alias,
          value          TYPE /iwfnd/med_mdl_info_value,
          activation     TYPE /iwfnd/med_mdl_info_value,
        END OF it_srvlst.

  DATA: i_repid                        LIKE sy-repid,
        lv_grid_title                  TYPE lvc_title,
        lv_number_of_selected_services TYPE string,
        ls_layout                      TYPE slis_layout_alv.

  CLEAR it_srvlst.
*  it_srvlst-srv_identifier = 'ID'.
*  it_srvlst-is_active = 'S'.
*  it_srvlst-system_alias = 'ALIAS'.
*  it_srvlst-value = 'TECH_NAME'.
*  it_srvlst-activation = 'ACT_METHOD'.
*  APPEND it_srvlst.
*  CLEAR it_srvlst.

  SELECT srv_identifier, is_active, value FROM /iwfnd/i_med_sin INTO CORRESPONDING FIELDS OF @it_srvlst WHERE name EQ 'BEP_SVC_EXT_SERVICE_NAME' AND is_active EQ 'A'.
    SELECT system_alias FROM /iwfnd/c_mgdeam INTO @it_srvlst-system_alias WHERE service_id EQ @it_srvlst-srv_identifier.
    ENDSELECT.
    APPEND it_srvlst.
    CLEAR it_srvlst.
  ENDSELECT.

*Add Activation Method
  LOOP AT it_srvlst.
    SELECT value FROM /iwfnd/i_med_sin INTO @it_srvlst-activation WHERE name EQ 'BEP_SVC_GENERATOR' AND is_active = 'A' AND srv_identifier EQ @it_srvlst-srv_identifier.
    ENDSELECT.
    MODIFY it_srvlst.
  ENDLOOP.

  i_repid = sy-repid.

  DESCRIBE TABLE it_srvlst LINES lv_number_of_selected_services.

  lv_grid_title = lv_number_of_selected_services && 'Active Services'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_grid_title     = lv_grid_title               " Control title
      i_structure_name = 'zfio_srv_name'
      is_layout        = ls_layout               " List layout specifications
    TABLES
      t_outtab         = it_srvlst               " Table with data to be displayed
    EXCEPTIONS
      program_error    = 1                " Program errors
      OTHERS           = 2.


** Display save dialog window
*  CALL METHOD cl_gui_frontend_services=>file_save_dialog
*    EXPORTING
**     window_title      = ' '
*      default_extension = 'XLS'
*      default_file_name = 'ODATACLNT'
*      initial_directory = 'c:\temp\'
*    CHANGING
*      filename          = ld_filename
*      path              = ld_path
*      fullpath          = ld_fullpath
*      user_action       = ld_result.
*
*  CONCATENATE ld_fullpath sy-mandt '_' sy-datum sy-uzeit '.xls' INTO p_dir.

*  CALL FUNCTION 'GUI_DOWNLOAD'
*    EXPORTING
*      filename                = p_dir
*      filetype                = 'ASC'
*      append                  = 'X'
*      write_field_separator   = 'X'
*      confirm_overwrite       = 'X'
** IMPORTING
**     FILELENGTH              =
*    TABLES
*      data_tab                = it_srvlst
**     fieldnames              = t_head
*    EXCEPTIONS
*      file_write_error        = 1
*      no_batch                = 2
*      gui_refuse_filetransfer = 3
*      invalid_type            = 4
*      no_authority            = 5
*      unknown_error           = 6
*      header_not_allowed      = 7
*      separator_not_allowed   = 8
*      filesize_not_allowed    = 9
*      header_too_long         = 10
*      dp_error_create         = 11
*      dp_error_send           = 12
*      dp_error_write          = 13
*      unknown_dp_error        = 14
*      access_denied           = 15
*      dp_out_of_memory        = 16
*      disk_full               = 17
*      dp_timeout              = 18
*      file_not_found          = 19
*      dataprovider_exception  = 20
*      control_flush_error     = 21
*      OTHERS                  = 22.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
