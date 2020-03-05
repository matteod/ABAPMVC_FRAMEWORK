CLASS zcl_mvcfw_con_main DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

*"* public components of class ZCL_MVCFW_CON_MAIN
*"* do not include other source files here!!!
    METHODS constructor
      IMPORTING
        VALUE(iv_program) TYPE syrepid .
    METHODS controller_exists
      IMPORTING
        !iv_type      TYPE c
      RETURNING
        VALUE(rv_res) TYPE abap_bool .
    METHODS get_con
      IMPORTING
        !iv_type      TYPE c
      RETURNING
        VALUE(ro_res) TYPE REF TO zcl_mvcfw_con_cfw
      RAISING
        zcx_mvcfw_error .
    METHODS get_selections
      EXPORTING
        !es_data TYPE any .
    METHODS kill_con
      IMPORTING
        !iv_type TYPE c .
    METHODS get_con_dynpro
      IMPORTING
        !iv_screen    TYPE sydynnr OPTIONAL
      RETURNING
        VALUE(ro_res) TYPE REF TO zcl_mvcfw_con_dynpro .
    METHODS pai_exit
      IMPORTING
        !iv_ucomm TYPE syucomm .
    METHODS pbo_main
      RETURNING
        VALUE(rv_res) TYPE sydynnr .
    METHODS refresh
      IMPORTING
        !iv_type  TYPE c OPTIONAL
        !iv_group TYPE char4 OPTIONAL
      RAISING
        cx_static_check .
    METHODS set_subdynpro
      IMPORTING
        !iv_dynnr      TYPE sydynnr OPTIONAL
        !iv_gui_status TYPE gui_status OPTIONAL
        !iv_titlebar   TYPE gui_title OPTIONAL
          PREFERRED PARAMETER iv_dynnr .
    CLASS-METHODS get_instance
      IMPORTING
        VALUE(iv_type) TYPE char32 OPTIONAL
        !iv_program    TYPE syrepid OPTIONAL
      RETURNING
        VALUE(ro_res)  TYPE REF TO zcl_mvcfw_con_main .
  PROTECTED SECTION.

    TYPES:
*"* protected components of class ZCL_MVCFW_CON_MAIN
*"* do not include other source files here!!!
      BEGIN OF gty_s_controller,
        type  TYPE char32,
        dynnr TYPE sydynnr,
        group TYPE char4,
        o_con TYPE REF TO object,
      END OF gty_s_controller .
    TYPES:
      gty_t_controllers TYPE SORTED TABLE OF gty_s_controller
          WITH UNIQUE KEY type dynnr .

    DATA mv_subscreen TYPE sydynnr .
    DATA mv_gui_status TYPE gui_status .
    DATA mv_titlebar_text TYPE text132 .
    DATA mt_controllers TYPE gty_t_controllers .
    DATA mv_titlebar TYPE gui_title .
    DATA mv_program TYPE syrepid .

    METHODS set_gui_interface
      IMPORTING
        !iv_gui_status TYPE gui_status OPTIONAL
        !iv_titlebar   TYPE gui_title OPTIONAL .
    METHODS create_con_dynpro
      IMPORTING
        !iv_screen    TYPE sydynnr OPTIONAL
      RETURNING
        VALUE(ro_res) TYPE REF TO zcl_mvcfw_con_dynpro .
    METHODS create_one_controller
      IMPORTING
        !iv_type      TYPE c
      RETURNING
        VALUE(ro_res) TYPE REF TO zcl_mvcfw_con_cfw
      RAISING
        zcx_mvcfw_error .
  PRIVATE SECTION.

*"* private components of class ZCL_MVCFW_CON_MAIN
*"* do not include other source files here!!!
    CLASS-DATA so_me TYPE REF TO zcl_mvcfw_con_main .

    METHODS put_dynpro_controller
      IMPORTING
        !io_in    TYPE REF TO object
        !iv_dynnr TYPE sydynnr OPTIONAL .
    METHODS get_container_type
      IMPORTING
        !io_in        TYPE REF TO cl_gui_container
        !iv_type      TYPE char32
      RETURNING
        VALUE(ro_res) TYPE REF TO cl_gui_container .
ENDCLASS.



CLASS ZCL_MVCFW_CON_MAIN IMPLEMENTATION.


  METHOD constructor.
    mv_program = iv_program.
  ENDMETHOD.


  METHOD controller_exists.

    READ TABLE mt_controllers
      WITH KEY type = iv_type
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      rv_res = abap_false.
    ELSE.
      rv_res = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD create_con_dynpro.
    MESSAGE 'CREATE_CON_DYNPRO must be redefined' TYPE 'A'.
  ENDMETHOD.


  METHOD create_one_controller.

    " to be redefined
* the type must be inheriting from zcl_mvcfw_con_cfw
* create object ro_res type (iv_type)
*    exporting iv_title = iv_title
*    (pass other parameters)

  ENDMETHOD.


  METHOD get_con.

    DATA ls_controller TYPE gty_s_controller.

    READ TABLE mt_controllers INTO ls_controller
      WITH KEY type = iv_type.
    IF sy-subrc <> 0.
      ls_controller-o_con = create_one_controller( iv_type ).
      ls_controller-type = iv_type.
      INSERT ls_controller INTO TABLE mt_controllers.
    ENDIF.
    ro_res ?= ls_controller-o_con.

  ENDMETHOD.


  METHOD get_container_type.


    IF cl_abap_typedescr=>describe_by_object_ref( io_in )->get_relative_name( ) <> iv_type.
      IF io_in->parent IS BOUND.
        ro_res = get_container_type( io_in = io_in->parent
                                     iv_type = iv_type ).
      ENDIF.
    ELSE.
      ro_res = io_in.
    ENDIF.

  ENDMETHOD.


  METHOD get_con_dynpro.
    FIELD-SYMBOLS: <ls_controller>    TYPE gty_s_controller.
    DATA: lv_screen                   TYPE sydynnr.

    IF iv_screen IS SUPPLIED.
      lv_screen = iv_screen.
    ELSE.
      lv_screen   = sy-dynnr.
    ENDIF.

    " to be redefined
    " return the singleton object for the subdynpro controller according to
    " the actual subscreen number
    READ TABLE mt_controllers ASSIGNING <ls_controller>
      WITH KEY type = space dynnr = lv_screen.
    IF sy-subrc = 0.
      ro_res ?= <ls_controller>-o_con.
    ELSE.
      ro_res = create_con_dynpro( lv_screen ).
      put_dynpro_controller( io_in = ro_res iv_dynnr = lv_screen ).
    ENDIF.

    " in the caller, if ro_res remains not bound, do the instantiation
    " case sy-dynnr.
    "   when '0101'.
    "     ro_res = lcl_con_subdyn_0101=>get_instance( ).
    "     super->put_dynpro_controller( ro_res )

  ENDMETHOD.


  METHOD get_instance.
    DATA lv_type TYPE string.

    IF so_me IS NOT BOUND.
      lv_type = '\PROGRAM='.
      IF iv_program IS NOT SUPPLIED.
        lv_type = lv_type && sy-cprog.
      ELSE.
        lv_type = lv_type && iv_program.
      ENDIF.
      lv_type = lv_type && | \\CLASS=|.
      IF iv_type IS NOT SUPPLIED.
        lv_type = lv_type && 'LCL_CON_MAIN'.
      ELSE.
        lv_type = lv_type && iv_type.
      ENDIF.
      CREATE OBJECT so_me
        TYPE (lv_type)
        EXPORTING
          iv_program = sy-repid.
    ENDIF.
    ro_res = so_me.
  ENDMETHOD.


  METHOD get_selections.
    " to be redefined
    " the only access to the global fields in the selection screen
*    ms_selscreen-s_carrid   = s_carrid[].
*    ms_selscreen-s_connid   = s_connid[].
*    ms_selscreen-s_fldate   = s_fldate[].
*    ms_selscreen-p_batch    = p_batch.
*    if es_data is supplied.
*      es_data = ms_selscreen.
*    endif.
  ENDMETHOD.


  METHOD kill_con.

    DATA ls_controller TYPE gty_s_controller.
    DATA lo_con TYPE REF TO zcl_mvcfw_con_cfw.
    DATA lo_cont TYPE REF TO cl_gui_container.

    READ TABLE mt_controllers INTO ls_controller
      WITH KEY type = iv_type.
    IF sy-subrc = 0.
      DELETE mt_controllers INDEX sy-tabix.
      lo_con ?= ls_controller-o_con.
      lo_cont = lo_con->get_container( ).
      FREE lo_con.
      FREE lo_cont.
    ENDIF.

  ENDMETHOD.


  METHOD pai_exit.

    " should be redefined for further logic
    LEAVE PROGRAM.

  ENDMETHOD.


  METHOD pbo_main.

    DATA:
      lt_exclude_ucomm              TYPE ui_functions.



    rv_res = mv_subscreen.

    SET PF-STATUS mv_gui_status OF PROGRAM mv_program EXCLUDING lt_exclude_ucomm.
    SET TITLEBAR mv_titlebar OF PROGRAM mv_program WITH mv_titlebar_text.


  ENDMETHOD.


  METHOD put_dynpro_controller.
    DATA: ls_con                      TYPE gty_s_controller.

    IF iv_dynnr IS SUPPLIED.
      ls_con-dynnr = sy-dynnr.
    ELSE.
      ls_con-dynnr = sy-dynnr.
    ENDIF.
    ls_con-o_con = io_in.
    INSERT ls_con INTO TABLE mt_controllers.
  ENDMETHOD.


  METHOD refresh.

    DATA: ls_con TYPE gty_s_controller,
          lo_con TYPE REF TO zcl_mvcfw_con_cfw.

    LOOP AT mt_controllers INTO ls_con.
      IF iv_type IS SUPPLIED.
        CHECK ls_con-type = iv_type.
      ENDIF.
      IF iv_group IS SUPPLIED.
        CHECK ls_con-type = iv_type.
      ENDIF.
      lo_con ?= ls_con-o_con.
      lo_con->refresh( ).
    ENDLOOP.


  ENDMETHOD.


  METHOD set_gui_interface.

    IF iv_gui_status IS SUPPLIED.
      mv_gui_status = iv_gui_status.
    ENDIF.
    IF iv_titlebar IS SUPPLIED.
      mv_titlebar = iv_titlebar.
    ENDIF.
  ENDMETHOD.


  METHOD set_subdynpro.

    IF iv_dynnr IS SUPPLIED.
      mv_subscreen = iv_dynnr.
    ENDIF.
    IF iv_gui_status IS SUPPLIED.
      set_gui_interface(
        iv_gui_status = iv_gui_status
        ).
    ENDIF.
    IF iv_titlebar IS SUPPLIED.
      set_gui_interface(
        iv_titlebar = iv_titlebar ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
