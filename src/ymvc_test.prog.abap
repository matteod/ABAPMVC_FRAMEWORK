*&---------------------------------------------------------------------*
*& Report YMVC_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ymvc_test.

*----------------------------------------------------------------------*
*       CLASS lcl_model DEFINITION
*----------------------------------------------------------------------*
* The model class can also be public
*----------------------------------------------------------------------*
CLASS lcl_model DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor,
      get_log
        RETURNING VALUE(ro_res) TYPE REF TO if_reca_message_list,
      set_carrid
        IMPORTING iv_in TYPE scarr-carrid,
      get_scarr
        RETURNING VALUE(rs_res) TYPE scarr,
      get_spfli
        RETURNING VALUE(rs_res) TYPE spfli,
      get_spfli_tab
        EXPORTING et_tab TYPE STANDARD TABLE,
      get_sflight_tab
        EXPORTING et_tab TYPE STANDARD TABLE,
      read_scarr
        RAISING
          zcx_mvcfw_error,
      read_sflights
        IMPORTING
          is_line TYPE spfli
        RAISING
          zcx_mvcfw_error.

  PRIVATE SECTION.
    DATA: ms_scarr   TYPE scarr,
          ms_spfli   TYPE spfli,
          mt_spfli   TYPE STANDARD TABLE OF spfli
                                         WITH DEFAULT KEY,
          mt_sflight TYPE STANDARD TABLE OF sflight
                                         WITH DEFAULT KEY,
          mo_log     TYPE REF TO if_reca_message_list.

ENDCLASS.                    "lcl_model DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_con_main DEFINITION
*----------------------------------------------------------------------*
* The main controller handles the overall program flow
*----------------------------------------------------------------------*
CLASS lcl_con_main DEFINITION INHERITING FROM zcl_mvcfw_con_main.

  PUBLIC SECTION.
    METHODS:
      " used by PBO of the subdynpro to get the adequate controller
      " this is a singleton, but 'create private' is not possible
      " because of the inheritance mechanism
      " the constructor will be called by zcl_mvcfw_con_main=>get_instance
      " the correct program name is passed automatically
      constructor
        IMPORTING
          iv_program TYPE syrepid,
      " called by dynpro 0001, also for exit commands!
      pai_main
        IMPORTING
          iv_ucomm TYPE syucomm,
      " method to show table spfli using an alv controller in a docking
      " at the bottom
      show_schedules
        RAISING zcx_mvcfw_error,
      " show flights for a schedule in a docking at right
      show_sflights
        RAISING zcx_mvcfw_error.


  PROTECTED SECTION.
    METHODS:
      create_con_dynpro REDEFINITION,
      create_one_controller REDEFINITION.
  PRIVATE SECTION.
    " constants all controller types
    CONSTANTS: cv_spfli    TYPE char32 VALUE 'LCL_CON_ALV_SPFLI',
               cv_sflight  TYPE char32 VALUE 'LCL_CON_ALV_SFLIGHT',
               cv_dyn_main TYPE char32 VALUE 'LCL_CON_DYNPRO_0100'.

    " the model will be created in constructor
    DATA: mo_model                  TYPE REF TO lcl_model.

ENDCLASS.                    "lcl_con_main DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_con_dynpro_0100 DEFINITION
*----------------------------------------------------------------------*
* Sub-dynpro controller
*----------------------------------------------------------------------*
CLASS lcl_con_dynpro_0100 DEFINITION INHERITING FROM zcl_mvcfw_con_subdyn.

  PUBLIC SECTION.
    METHODS:
      " add parameer io_model in addition to the strucure name
      constructor
        IMPORTING iv_struc_name TYPE c DEFAULT 'SCARR'
                  io_model      TYPE REF TO lcl_model,
      " used for filling the dynpro fields from model data
      pbo REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      " react to single field changes
      pai_field_change REDEFINITION,
      " process the user command
      pai_user_command REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_model TYPE REF TO lcl_model,
          mo_main  TYPE REF TO lcl_con_main.

ENDCLASS.                    "lcl_con_dynpro_0100 DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_con_alv_spfli DEFINITION
*----------------------------------------------------------------------*
* Controller class for the ALV at bottom
*----------------------------------------------------------------------*
CLASS lcl_con_alv_spfli DEFINITION INHERITING FROM zcl_mvcfw_con_alv.

  PUBLIC SECTION.
    METHODS:
      " refresh gets the data from the model and refreshes the display
      refresh REDEFINITION,
      constructor
        IMPORTING
                  io_container TYPE REF TO cl_gui_container
                  io_model     TYPE REF TO lcl_model
        RAISING   cx_static_check.

  PROTECTED SECTION.
    METHODS:
      prepare_layout REDEFINITION,
      on_double_click REDEFINITION,
      on_toolbar REDEFINITION,
      on_ucomm REDEFINITION.


  PRIVATE SECTION.
    DATA: mo_model TYPE REF TO lcl_model,
          mo_main  TYPE REF TO lcl_con_main.

ENDCLASS.                    "lcl_con_alv_spfli DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_con_alv_sflight DEFINITION
*----------------------------------------------------------------------*
* Controller class for the ALV at right
*----------------------------------------------------------------------*
CLASS lcl_con_alv_sflight DEFINITION INHERITING FROM zcl_mvcfw_con_alv.

  PUBLIC SECTION.
    METHODS:
      refresh REDEFINITION,
      constructor
        IMPORTING
                  io_container TYPE REF TO cl_gui_container
                  io_model     TYPE REF TO lcl_model
        RAISING   cx_static_check.

  PROTECTED SECTION.
    METHODS:
      prepare_layout REDEFINITION,
      on_double_click REDEFINITION,
      on_toolbar REDEFINITION,
      on_ucomm REDEFINITION.


  PRIVATE SECTION.
    DATA: mo_model TYPE REF TO lcl_model,
          mo_main  TYPE REF TO lcl_con_main.

ENDCLASS.                    "lcl_con_alv_spfli DEFINITION

" use tables for all structures on dynpros
TABLES scarr.
" these variables are mandatory
DATA: gv_subdyn  TYPE sydynnr,
      gv_okcode  TYPE syucomm,
      go_main    TYPE REF TO lcl_con_main,
      go_dyn_sub TYPE REF TO zcl_mvcfw_con_subdyn.

START-OF-SELECTION.
  " This report can be run directly.
  " as an  alternative, create a dialogue transaction for the screen
  CALL SCREEN 1.

*----------------------------------------------------------------------*
*       CLASS lcl_Model IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_model IMPLEMENTATION.

  METHOD constructor.
    mo_log = cf_reca_message_list=>create( ).
  ENDMETHOD.                    "constructor

  METHOD get_log.
    ro_res = mo_log.
  ENDMETHOD.                    "get_log

  METHOD set_carrid.
    ms_scarr-carrid = iv_in.
  ENDMETHOD.                    "set_carrid

  METHOD get_scarr.
    rs_res = ms_scarr.
  ENDMETHOD.                    "get_spfli

  METHOD get_spfli.
    rs_res = ms_spfli.
  ENDMETHOD.                    "get_spfli

  METHOD get_spfli_tab.
    et_tab = mt_spfli.
  ENDMETHOD.                    "get_spfli_tab

  METHOD get_sflight_tab.
    et_tab = mt_sflight.
  ENDMETHOD.                    "get_sflight_tab

  METHOD read_scarr.
    CLEAR mt_spfli.
    SELECT SINGLE * FROM scarr
      INTO ms_scarr
      WHERE carrid = ms_scarr-carrid.
    IF sy-subrc <> 0.
      MESSAGE e000(0k) WITH 'no data found' INTO sy-msgli.
      RAISE EXCEPTION TYPE zcx_mvcfw_error
        EXPORTING
          textid        = VALUE scx_t100key( msgid = sy-msgid msgno = sy-msgno attr1 = 'MV_MESSAGE_V1' )
          mv_message_v1 = sy-msgv1.
    ENDIF.
    SELECT * FROM spfli
      INTO TABLE mt_spfli
      WHERE carrid = ms_scarr-carrid.
  ENDMETHOD.                    "read_spfli

  METHOD read_sflights.
    CLEAR mt_sflight.
    SELECT * FROM sflight
      INTO TABLE mt_sflight
      WHERE carrid = ms_scarr-carrid AND
            connid = is_line-connid.
    ms_spfli = is_line.
  ENDMETHOD.                    "read_sflights
ENDCLASS.                    "lcl_Model IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS lcl_con_main IMPLEMENTATION
*----------------------------------------------------------------------*
* Main controller
*----------------------------------------------------------------------*
CLASS lcl_con_main IMPLEMENTATION.

  METHOD constructor.
    " 1. pass the actual program name to the super-constructor
    " 2. create the model
    " 3. set the initial subdynpro
    DATA: lv_repid                  TYPE syrepid.

    lv_repid = sy-repid.
    super->constructor( lv_repid ).
    CREATE OBJECT mo_model.
    set_subdynpro(
      EXPORTING
         iv_dynnr      = '0100'    " IV_DYNNR
         iv_gui_status = 'MAIN'     " IV_GUI_STATUS
         iv_titlebar   = 'MAIN'
    ).

  ENDMETHOD.                    "constructor

  METHOD pai_main.
    " process dynpro-independant functions
    CASE iv_ucomm.
      WHEN 'EXIT' OR 'BACK' OR 'CANC'.
        LEAVE PROGRAM.
    ENDCASE.
  ENDMETHOD.                    "pai_main

  METHOD create_con_dynpro.
    " a new controller is needed
    CASE sy-dynnr.
      WHEN '0100'.
        CREATE OBJECT ro_res
          TYPE lcl_con_dynpro_0100
          EXPORTING
            io_model = mo_model.
    ENDCASE.

  ENDMETHOD.                    "get_con_dynpro

  METHOD create_one_controller.
    " this is called by GET_CON, if it is called for the first time for a type
    " in further calls, GET_CON returns the object created at the first call
    " to obtain this, ZCL_mvcfw_CON_MAIN has a table with all active controllers
    " here you decide, where a screen control is to be displayed.
    " 1. Determine which control is to be created (iv_type)
    " 2. Create the container for the control
    " 3. Create the control giving the container and the model
    DATA: lo_docking                TYPE REF TO cl_gui_docking_container.

    CASE iv_type.
      WHEN cv_spfli.
        CREATE OBJECT lo_docking
          EXPORTING
            side      = cl_gui_docking_container=>dock_at_bottom   " Side to Which Control is Docked
            extension = 100.    " Control Extension
        CREATE OBJECT ro_res
          TYPE (iv_type)
          EXPORTING
            io_container = lo_docking
            io_model     = mo_model.
      WHEN cv_sflight.
        CREATE OBJECT lo_docking
          EXPORTING
            side      = cl_gui_docking_container=>dock_at_right   " Side to Which Control is Docked
            extension = 500.    " Control Extension
        CREATE OBJECT ro_res
          TYPE (iv_type)
          EXPORTING
            io_container = lo_docking
            io_model     = mo_model.
    ENDCASE.

  ENDMETHOD.                    "create_one_controller

  METHOD show_schedules.
    " show the schedules. Calling GET_CON the first time will create the control
    get_con( cv_spfli  )->refresh( ).
    " maybe the container is not visible. This will set it visible
    get_con( cv_spfli  )->unhide( ).

  ENDMETHOD.                    "show_schedules

  METHOD show_sflights.

    get_con( cv_sflight  )->refresh( ).
    get_con( cv_sflight  )->unhide( ).

  ENDMETHOD.                    "show_sflights

ENDCLASS.                    "lcl_con_main IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_con_dynpro_0100 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_con_dynpro_0100 IMPLEMENTATION.

  METHOD constructor.
    " 1. call the general constructor with the structure name
    " 2. set the model object
    super->constructor( iv_struc_name ).
    mo_model = io_model.
    mo_main ?= lcl_con_main=>get_instance( ).
  ENDMETHOD.                    "constructor

  METHOD pai_field_change.
    " --> in: iv_fieldname
    " --> in: iv_source (generic)
    CASE iv_fieldname.
      WHEN 'CARRID'.
        " pass the data to the model
        mo_model->set_carrid( iv_source ).
    ENDCASE.

  ENDMETHOD.                    "pai_field_change

  METHOD pbo.
    " get the dynpro fields from the model
    e_data = mo_model->get_scarr( ).

  ENDMETHOD.                    "pbo

  METHOD pai_user_command.
    " process dynpro dependant functions
    TRY.
        CASE iv_ucomm.
          WHEN 'SHOW'.
            mo_model->read_scarr( ).
            mo_main->show_schedules( ).
        ENDCASE.
      CATCH zcx_mvcfw_error INTO DATA(lo_err).
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.                    "pai_user_command
ENDCLASS.                    "lcl_con_dynpro_0100 IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_con_alv_spfli IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_con_alv_spfli IMPLEMENTATION.

  METHOD constructor.
    " The constructor MUST always contain:
    " 1. call of super constructor
    " set model
    " set main controller
    super->constructor(
      io_container = io_container
      iv_structure_name = 'SPFLI' ).

    " it is recommended to keep the model and the main controller in variables
    mo_model = io_model.
    mo_main ?= lcl_con_main=>get_instance( ).

  ENDMETHOD.                    "constructor


  METHOD prepare_layout.
    DATA: ls_scarr                  TYPE scarr.

    ls_scarr = mo_model->get_scarr( ).
    " rs_res is of type lvc_s_layo. You can set all its values herer
    rs_res-cwidth_opt = abap_true.
    rs_res-grid_title = 'Schedules for' && | | && ls_scarr-carrid && | | &&
                        'Double click for details'.
  ENDMETHOD.                    "prepare_layout

  METHOD on_double_click.
    " this is a standard double click call from cl_gui_alv_grid

    FIELD-SYMBOLS: <lt_table>       TYPE STANDARD TABLE.
    DATA ls_line TYPE spfli.

    TRY.
        ASSIGN mr_table->* TO <lt_table>.

        READ TABLE <lt_table> INTO ls_line INDEX e_row-index.
        IF sy-subrc = 0.
          mo_model->read_sflights( ls_line ).
          mo_main->show_sflights( ).
        ENDIF.
      CATCH zcx_mvcfw_error INTO DATA(lo_err).
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.                    "on_double_click

  METHOD refresh.
    " redefined. Here tha table is filled with data from the model

    FIELD-SYMBOLS: <lt_table>       TYPE STANDARD TABLE.
    DATA: ls_layout                 TYPE lvc_s_layo.

    ASSIGN mr_table->* TO <lt_table>.

    mo_model->get_spfli_tab( IMPORTING et_tab = <lt_table> ).
    " refresh the grid title, therefore set new layout values
    ls_layout = prepare_layout( ).

    mo_alv->set_frontend_layout( ls_layout ).
    " note that the first call of this method will call "set_table_for_first_display"
    refresh_table( ).
  ENDMETHOD.                    "refresh

  METHOD on_toolbar.
    " we provide a "close" button to be able to hide the container
    DATA: ls_toolbar  TYPE stb_button.

    CLEAR ls_toolbar.
    ls_toolbar-butn_type = '3'.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'CLOSE' TO ls_toolbar-function.
    MOVE 'Close displayr' TO ls_toolbar-quickinfo.
    MOVE icon_close TO ls_toolbar-icon.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.


  ENDMETHOD.                    "on_toolbar

  METHOD on_ucomm.
    CASE e_ucomm.
      WHEN 'CLOSE'.
        " hide the container
        hide( ).
    ENDCASE.
  ENDMETHOD.                    "on_ucomm
ENDCLASS.                    "lcl_con_alv_spfli IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_con_alv_sflight IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_con_alv_sflight IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
      io_container = io_container
      iv_structure_name = 'SFLIGHT' ).

    mo_model = io_model.
    mo_main ?= lcl_con_main=>get_instance( ).
  ENDMETHOD.                    "constructor

  METHOD prepare_layout.
    DATA: ls_spfli                  TYPE spfli.

    ls_spfli = mo_model->get_spfli( ).
    " rs_res is of type lvc_s_layo. You can set all its values herer
    rs_res-cwidth_opt = abap_true.
    rs_res-grid_title = 'Flights for' && | | && ls_spfli-carrid && | | &&
                        ls_spfli-connid.
  ENDMETHOD.                    "prepare_layout

  METHOD on_double_click.

  ENDMETHOD.                    "on_double_click

  METHOD refresh.

    FIELD-SYMBOLS: <lt_table>       TYPE STANDARD TABLE.
    DATA: ls_layout                 TYPE lvc_s_layo.

    ASSIGN mr_table->* TO <lt_table>.

    mo_model->get_sflight_tab( IMPORTING et_tab = <lt_table> ).
    " refresh the grid title, therefore set new layout values
    ls_layout = prepare_layout( ).

    mo_alv->set_frontend_layout( ls_layout ).

    refresh_table( ).

  ENDMETHOD.                    "refresh

  METHOD on_toolbar.
    DATA: ls_toolbar  TYPE stb_button.

    CLEAR ls_toolbar.
    ls_toolbar-butn_type = '3'.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'CLOSE' TO ls_toolbar-function.
    MOVE 'Close displayr' TO ls_toolbar-quickinfo.
    MOVE icon_close TO ls_toolbar-icon.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.


  ENDMETHOD.                    "on_toolbar

  METHOD on_ucomm.
    CASE e_ucomm.
      WHEN 'CLOSE'.
        hide( ).
    ENDCASE.
  ENDMETHOD.                    "on_ucomm
ENDCLASS.                    "lcl_con_alv_sflight IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  PBO_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0001 OUTPUT.

  " get the main controller and call the pbo method
  go_main ?= lcl_con_main=>get_instance( ).
  gv_subdyn = go_main->pbo_main( ).

ENDMODULE.                 " PBO_0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_sub OUTPUT.

  " get the subscreen controller and call the pbo method
  go_dyn_sub ?= go_main->get_con_dynpro( ).
  go_dyn_sub->pbo( IMPORTING e_data = scarr ).

ENDMODULE.                 " PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0001 INPUT.
  " go_main is set in the pbo module!
  go_main->pai_main( gv_okcode ).
  CLEAR gv_okcode.

ENDMODULE.                 " PAI_0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_sub INPUT.
  " get the subdynpro controller and call the pai method
  go_dyn_sub ?= go_main->get_con_dynpro( ).
  go_dyn_sub->pai( EXPORTING iv_ucomm = gv_okcode
                   CHANGING c_data = scarr ).

ENDMODULE.                 " PAI_0100  INPUT
