CLASS zcl_mvcfw_con_dynpro DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS init_screen .
*"* public components of class ZCL_MVCFW_CON_DYNPRO
*"* do not include other source files here!!!
    METHODS constructor
      IMPORTING
        !iv_struc_name TYPE tabname OPTIONAL
        !io_con_main   TYPE REF TO zcl_mvcfw_con_main OPTIONAL
          PREFERRED PARAMETER iv_struc_name .
    METHODS pai
      IMPORTING
        !iv_ucomm TYPE syucomm OPTIONAL
      CHANGING
        !c_data   TYPE any OPTIONAL .
    METHODS pai_exit
      IMPORTING
        !iv_ucomm TYPE syucomm OPTIONAL .
  PROTECTED SECTION.

    DATA mo_con_main TYPE REF TO zcl_mvcfw_con_main .
*"* protected components of class ZCL_MVCFW_CON_DYNPRO
*"* do not include other source files here!!!
    DATA mr_screen_data TYPE REF TO data .
    DATA mv_screen_struc TYPE tabname .
    DATA mt_field_list TYPE ddfields .
    DATA mv_stop_field_processing TYPE abap_bool .

    METHODS pai_field_change
      IMPORTING
        !iv_fieldname TYPE dfies-fieldname
        !iv_source    TYPE any
        !iv_destin    TYPE any
        !i_data       TYPE any
      RAISING
        cx_static_check .
    METHODS pai_before_field_change
      IMPORTING
        !iv_ucomm TYPE syucomm
      CHANGING
        !c_data   TYPE any
      RAISING
        cx_static_check .
    METHODS pai_user_command
      IMPORTING
        !iv_ucomm TYPE syucomm
      RAISING
        cx_static_check .
    METHODS show_error .
  PRIVATE SECTION.

*"* private components of class ZCL_MVCFW_CON_DYNPRO
*"* do not include other source files here!!!
    METHODS pai_field_changes
      IMPORTING
        !i_data TYPE any
      RAISING
        cx_static_check .
ENDCLASS.



CLASS ZCL_MVCFW_CON_DYNPRO IMPLEMENTATION.


  METHOD constructor.

    IF iv_struc_name IS NOT INITIAL.
      mv_screen_struc = iv_struc_name.
      CREATE DATA mr_screen_data TYPE (mv_screen_struc).
*   get default field list
      CALL FUNCTION 'DDIF_NAMETAB_GET'
        EXPORTING
          tabname   = iv_struc_name
          all_types = abap_true
        TABLES
*         X031L_TAB =
          dfies_tab = mt_field_list
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
    ENDIF.

    mo_con_main = io_con_main.

  ENDMETHOD.


  METHOD init_screen.
    IF mr_screen_data IS BOUND.
      ASSIGN mr_screen_data->* TO FIELD-SYMBOL(<ls_screen>).
      CLEAR <ls_screen>.
    ENDIF.
  ENDMETHOD.


  METHOD pai.


    TRY.

        IF c_data IS SUPPLIED.
          pai_before_field_change( EXPORTING iv_ucomm = iv_ucomm
                                   CHANGING c_data = c_data ).
          pai_field_changes( c_data ).
        ENDIF.

        pai_user_command( iv_ucomm ).

      CATCH cx_static_check.
        show_error( ).
    ENDTRY.

  ENDMETHOD.


  METHOD pai_before_field_change.

    " this has to be redefined.

  ENDMETHOD.


  METHOD pai_exit.


    TRY.
        pai_user_command( iv_ucomm ).

      CATCH cx_static_check.
        show_error( ).
    ENDTRY.

  ENDMETHOD.


  METHOD pai_field_change.

    " this has to be redefined

  ENDMETHOD.


  METHOD pai_field_changes.


    DATA ls_field TYPE dfies.

    FIELD-SYMBOLS <ls_screen> TYPE any.
    FIELD-SYMBOLS <lv_source> TYPE any.
    FIELD-SYMBOLS <lv_destin> TYPE any.
    DATA ls_comp TYPE x030l.

    CHECK mr_screen_data IS BOUND.

    ASSIGN mr_screen_data->* TO <ls_screen>.
    mv_stop_field_processing = abap_false.
    " find changed fields
    LOOP AT mt_field_list INTO ls_field.
      ASSIGN COMPONENT ls_field-fieldname OF STRUCTURE i_data TO <lv_source>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      ASSIGN COMPONENT ls_field-fieldname OF STRUCTURE <ls_screen> TO <lv_destin>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      IF <lv_source> <> <lv_destin>.
        pai_field_change( iv_fieldname = ls_field-fieldname
                          iv_source = <lv_source>
                          iv_destin = <lv_destin>
                          i_data    = i_data ).
        <lv_destin> = <lv_source>.

      ENDIF.
      IF mv_stop_field_processing = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD pai_user_command.

    " this has to be redefined

  ENDMETHOD.


  METHOD show_error.

    " this has to be redefined

  ENDMETHOD.
ENDCLASS.
