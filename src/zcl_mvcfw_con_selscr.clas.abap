CLASS zcl_mvcfw_con_selscr DEFINITION
  PUBLIC
  INHERITING FROM zcl_mvcfw_con_dynpro
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

*"* public components of class ZCL_MVCFW_CON_SELSCR
*"* do not include other source files here!!!
    METHODS run
        ABSTRACT
      RAISING
        cx_static_check .
    METHODS constructor
      IMPORTING
        !iv_struc_name TYPE tabname OPTIONAL
        !is_selections TYPE any OPTIONAL
        !io_con_main   TYPE REF TO zcl_mvcfw_con_main OPTIONAL
          PREFERRED PARAMETER iv_struc_name .

    METHODS pai
        REDEFINITION .
  PROTECTED SECTION.
*"* protected components of class ZCL_MVCFW_CON_SELSCR
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_MVCFW_CON_SELSCR
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_MVCFW_CON_SELSCR IMPLEMENTATION.


  METHOD constructor.

    super->constructor(
        iv_struc_name       = iv_struc_name
        io_con_main         = io_con_main
         ).

    IF iv_struc_name IS INITIAL.
      IF is_selections IS SUPPLIED.
        DATA(lo_structdescr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( is_selections ) ).
        LOOP AT lo_structdescr->components INTO DATA(ls_struc_line).
          APPEND INITIAL LINE TO mt_field_list ASSIGNING FIELD-SYMBOL(<ls_field_list>).
          <ls_field_list>-fieldname = ls_struc_line-name.
        ENDLOOP.

        CREATE DATA mr_screen_data LIKE is_selections.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD pai.
    FIELD-SYMBOLS <ls_screen_local> TYPE any.
    DATA: lr_screen_local TYPE REF TO data.

    ASSIGN mr_screen_data->* TO FIELD-SYMBOL(<ls_screen>).


    IF mo_con_main IS BOUND.
      CREATE DATA lr_screen_local LIKE <ls_screen>.
      ASSIGN lr_screen_local->* TO <ls_screen_local>.

      mo_con_main->get_selections(
        IMPORTING es_data = <ls_screen_local> ).

      super->pai(
        EXPORTING iv_ucomm = iv_ucomm
        CHANGING c_data = <ls_screen_local> ).
    ELSE.
      super->pai(
        EXPORTING iv_ucomm = iv_ucomm
        CHANGING c_data = c_data ).

    ENDIF.


  ENDMETHOD.
ENDCLASS.
