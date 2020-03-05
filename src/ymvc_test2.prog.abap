*&---------------------------------------------------------------------*
*& Report YMVC_TEST2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ymvc_test2.

INTERFACE lif_report.
  TYPES:
    BEGIN OF gty_s_selscreen,
      s_carrid TYPE RANGE OF sflight-carrid,
      s_connid TYPE RANGE OF sflight-connid,
      s_fldate TYPE RANGE OF sflight-fldate,
      p_batch  TYPE abap_bool,
    END OF gty_s_selscreen.
ENDINTERFACE.

CLASS lcl_model DEFINITION.

  PUBLIC SECTION.
    METHODS:
      read_db
        IMPORTING is_selection TYPE lif_report=>gty_s_selscreen,
      get_out_tab
        EXPORTING et_out TYPE STANDARD TABLE.


  PRIVATE SECTION.

    DATA: mt_main_output TYPE TABLE OF sflight,
          ms_selection   TYPE lif_report=>gty_s_selscreen.

ENDCLASS.                    "lcl_model DEFINITION

CLASS lcl_con_selscr_1000 DEFINITION DEFERRED.

CLASS lcl_con_main DEFINITION INHERITING FROM zcl_mvcfw_con_main.

  PUBLIC SECTION.

    CONSTANTS:
      cv_con_alv_out        TYPE char32 VALUE 'LCL_CON_ALV_OUT',
      cv_con_selscreen_1000 TYPE char32 VALUE 'LCL_CON_SELSCR_1000',
      cv_con_list           TYPE char32 VALUE 'LCL_CON_LIST'.
    METHODS:
      constructor
        IMPORTING iv_program TYPE syrepid,
      pai_main
        IMPORTING
          iv_ucomm TYPE syucomm,
      pbo_main REDEFINITION,
      run_program,
      get_selections REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      create_con_dynpro REDEFINITION,
      create_one_controller REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_model     TYPE REF TO lcl_model,
          ms_selscreen TYPE lif_report=>gty_s_selscreen.

ENDCLASS.                    "lcl_con_main DEFINITION

CLASS lcl_con_selscr_1000 DEFINITION INHERITING FROM zcl_mvcfw_con_selscr.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING io_model      TYPE REF TO lcl_model
                  io_con_main   TYPE REF TO lcl_con_main
                  is_selections TYPE lif_report=>gty_s_selscreen,
      run REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      pai_field_change REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_model TYPE REF TO lcl_model,
          mo_main  TYPE REF TO lcl_con_main,
          " only for background run:
          mt_out   TYPE TABLE OF sflight.

ENDCLASS.                    "lcl_con_selscr_1000 DEFINITION

CLASS lcl_con_alv_out DEFINITION INHERITING FROM zcl_mvcfw_con_alv.
  PUBLIC SECTION.
    METHODS:
      refresh REDEFINITION,
      constructor
        IMPORTING
                  io_container TYPE REF TO cl_gui_container
                  io_model     TYPE REF TO lcl_model
                  io_main      TYPE REF TO lcl_con_main
        RAISING   zcx_mvcfw_error.

  PROTECTED SECTION.
    METHODS:
      prepare_layout REDEFINITION,
      on_double_click REDEFINITION,
      on_toolbar REDEFINITION,
      on_ucomm REDEFINITION,
      prepare_exclude REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_model TYPE REF TO lcl_model,
          mo_main  TYPE REF TO lcl_con_main.

ENDCLASS.                    "lcl_con_alv_out DEFINITION

CLASS lcl_con_list DEFINITION INHERITING FROM zcl_mvcfw_con_cfw.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          io_model TYPE REF TO lcl_model
          io_main  TYPE REF TO lcl_con_main,
      refresh REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: mo_model TYPE REF TO lcl_model,
          mo_main  TYPE REF TO lcl_con_main,
          mt_out   TYPE TABLE OF sflight.

ENDCLASS.


TABLES sflight.

SELECT-OPTIONS: s_carrid            FOR sflight-carrid,
                s_connid            FOR sflight-connid,
                s_fldate            FOR sflight-fldate.

PARAMETERS p_batch AS CHECKBOX.

DATA: gv_okcode     TYPE syucomm,
      go_sel_screen TYPE REF TO lcl_con_selscr_1000,
      go_main       TYPE REF TO lcl_con_main.

*--------------------------------------------------------------------*
* report events
*--------------------------------------------------------------------*

INITIALIZATION.
  " start point: create main and sel.screen controllers
  go_main ?= lcl_con_main=>get_instance( ).
  go_sel_screen ?= go_main->get_con_dynpro( ).

AT SELECTION-SCREEN.
  " call pai of sel.screen controller
  go_sel_screen->pai( ).

START-OF-SELECTION.
  " call run routine of sel.screen controller
  go_sel_screen->run( ).

CLASS lcl_model IMPLEMENTATION.

  METHOD read_db.

    ms_selection = is_selection.
    SELECT * FROM sflight INTO TABLE mt_main_output
      WHERE carrid IN ms_selection-s_carrid AND
            connid IN ms_selection-s_connid AND
            fldate IN ms_selection-s_fldate.

  ENDMETHOD.                    "read_db

  METHOD get_out_tab.

    et_out = mt_main_output.

  ENDMETHOD.                    "get_out_tab

ENDCLASS.                    "lcl_model IMPLEMENTATION

CLASS lcl_con_main IMPLEMENTATION.
  METHOD constructor.

    " the report name is needed in the framework
    super->constructor( |{ sy-repid }| ).
    TRY.
        CREATE OBJECT mo_model.
        " set parameters of the controller for the GUI interface
        set_gui_interface(
          EXPORTING
             iv_gui_status = 'MAIN'     " IV_GUI_STATUS
             iv_titlebar   = 'MAIN'
        ).
      CATCH cx_static_check.
        LEAVE PROGRAM.
    ENDTRY.
  ENDMETHOD.                    "constructor

  METHOD pbo_main.
    " pbo_main is for screen 0001, NOT for the selection screen
    TRY.
        get_con( cv_con_alv_out )->refresh( ).
        rv_res = super->pbo_main( ).
      CATCH cx_static_check.
        " error handling here
    ENDTRY.
  ENDMETHOD.

  METHOD pai_main.
    CASE iv_ucomm.
      WHEN 'EXIT' OR 'BACK' OR 'CANC'.
        SET SCREEN 0.
        LEAVE SCREEN.
    ENDCASE.

  ENDMETHOD.                    "pai_main

  METHOD create_con_dynpro.
    CASE sy-dynnr.
      WHEN '1000'.
        CREATE OBJECT ro_res
          TYPE lcl_con_selscr_1000
          EXPORTING
            io_model      = mo_model
            io_con_main   = me
            is_selections = ms_selscreen. " propagate the selection structure
        " store the controller object
    ENDCASE.

  ENDMETHOD.                    "get_con_dynpro

  METHOD create_one_controller.
    " create each output controller on request
    CASE iv_type.
      WHEN cv_con_alv_out.
        CREATE OBJECT ro_res
          TYPE (iv_type)
          EXPORTING
            io_container = NEW cl_gui_docking_container(
                                  extension = 9999
                                )
            io_main      = me
            io_model     = mo_model.
      WHEN cv_con_list.
        " special case list: no container needed
        CREATE OBJECT ro_res
          TYPE (iv_type)
          EXPORTING
            io_main      = me
            io_model     = mo_model.
    ENDCASE.
  ENDMETHOD.                    "create_one_controller

  METHOD get_selections.
    " the only access to the global fields in the selection screen
    ms_selscreen-s_carrid   = s_carrid[].
    ms_selscreen-s_connid   = s_connid[].
    ms_selscreen-s_fldate   = s_fldate[].
    ms_selscreen-p_batch    = p_batch.
    IF es_data IS SUPPLIED.
      es_data = ms_selscreen.
    ENDIF.
  ENDMETHOD.

  METHOD run_program.

    TRY.
        mo_model->read_db( ms_selscreen ).
        " decide how to present the data
        IF sy-batch = abap_true OR
           ms_selscreen-p_batch = abap_true.
          " a list is used
          get_con( cv_con_list )->refresh( ).
        ELSE.
          " an interactive grid is used
          CALL SCREEN 1.
        ENDIF.
      CATCH cx_static_check.
        " error handling here
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_con_selscr_1000 IMPLEMENTATION.

  METHOD constructor.
    DATA: lt_fname                  TYPE lvc_t_fnam.

    " 1. call the general constructor with the structure name
    " 2. set the model object
    super->constructor( is_selections = is_selections
                        io_con_main = io_con_main ).
    mo_model = io_model.
    " concrete main controller from generic via casting
    mo_main ?= mo_con_main.

  ENDMETHOD.                    "constructor

  METHOD pai_field_change.

*    try.
*        mo_model->perform_field_check( iv_fieldname = iv_fieldname
*                                       iv_source = iv_source ).
*      catch zcx_mvcfw_error into data(lo_err).
*        message lo_err type 'S' display like 'E'.
*    endtry.
  ENDMETHOD.                    "pai_field_change

  METHOD run.
    " pass the control to the main controller
    mo_main->run_program( ).
  ENDMETHOD.                    "pbo

ENDCLASS.                    "create_con_dynpro

CLASS lcl_con_alv_out IMPLEMENTATION.
  METHOD constructor.
    super->constructor(
      io_container = io_container
      iv_structure_name = 'sflight' ).

    mo_model = io_model.
    mo_main = io_main..
  ENDMETHOD.                    "constructor

*
  METHOD prepare_exclude.
*    Example
*    append:
*      cl_gui_alv_grid=>mc_fc_graph             to et_tab.

  ENDMETHOD.                    "prepare_exclude

  METHOD prepare_layout.
    rs_res-cwidth_opt = abap_true.
    rs_res-grid_title = 'Flights table'.
  ENDMETHOD.                    "prepare_layout

  METHOD on_double_click.

    FIELD-SYMBOLS: <ls_list> TYPE sflight,
                   <lt_tab>  TYPE STANDARD TABLE.

    TRY.
        ASSIGN mr_table->* TO <lt_tab>.
        READ TABLE <lt_tab> ASSIGNING <ls_list> INDEX es_row_no-row_id.
        IF sy-subrc = 0 .
          " include double click functions
        ENDIF.
      CATCH zcx_mvcfw_error INTO DATA(lo_err).
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.                    "on_double_click

  METHOD refresh.

    FIELD-SYMBOLS: <lt_table>       TYPE STANDARD TABLE.
    DATA: ls_layout                 TYPE lvc_s_layo.

    ASSIGN mr_table->* TO <lt_table>.

    mo_model->get_out_tab( IMPORTING et_out = <lt_table> ).
    refresh_table( ).

  ENDMETHOD.                    "refresh
*
  METHOD on_toolbar.
    DATA: ls_toolbar  TYPE stb_button.

    "    create user buttons
    ls_toolbar-function = 'CLOSE'.
    ls_toolbar-icon = icon_close.
    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.                    "on_toolbar

  METHOD on_ucomm.

    DATA: lv_line_index             TYPE i.

    TRY.
        CASE e_ucomm.
          WHEN 'CLOSE'.
            hide( ).
        ENDCASE.
      CATCH zcx_mvcfw_error INTO DATA(lo_err).
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.                    "on_ucomm_equi

ENDCLASS.                    "lcl_con_alv_out IMPLEMENTATION

CLASS lcl_con_list IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mo_model = io_model.
    mo_main = io_main.
  ENDMETHOD.

*--------------------------------------------------------------------*
* Redefinition: create a abap list (used for batch run)
*--------------------------------------------------------------------*
  METHOD refresh.
    DATA lo_salv TYPE REF TO cl_salv_table.

    mo_model->get_out_tab( IMPORTING et_out = mt_out ).
    cl_salv_table=>factory(
      EXPORTING
         list_display   = if_salv_c_bool_sap=>true    " ALV Displayed in List Mode
       IMPORTING
         r_salv_table   = lo_salv    " Basis Class Simple ALV Tables
      CHANGING
        t_table        = mt_out
    ).
    lo_salv->get_columns( )->set_optimize( ).
    lo_salv->display( ).

  ENDMETHOD.

ENDCLASS.

MODULE pbo_0001 OUTPUT.
  go_main->pbo_main( ).
ENDMODULE.

MODULE pai_0001 INPUT.
  go_main->pai_main( iv_ucomm = gv_okcode ).
ENDMODULE.
