CLASS zcl_mvcfw_con_alv DEFINITION
  PUBLIC
  INHERITING FROM zcl_mvcfw_con_cfw
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

*"* public components of class ZCL_MVCFW_CON_ALV
*"* do not include other source files here!!!
    METHODS constructor
      IMPORTING
        !io_container      TYPE REF TO cl_gui_container
        !iv_structure_name TYPE tabname OPTIONAL
        !is_structure      TYPE any OPTIONAL
      RAISING
        zcx_mvcfw_error .
    METHODS refresh_table
      EXPORTING
        !ev_first_refresh TYPE abap_bool .
    METHODS init_alv
      RAISING
        zcx_mvcfw_error .
    METHODS get_selected_rows
      EXPORTING
        !et_tab TYPE STANDARD TABLE .
  PROTECTED SECTION.
*"* protected components of class ZCL_MVCFW_CON_ALV
*"* do not include other source files here!!!

    DATA mt_excpt_qinf TYPE lvc_t_qinf .
    DATA mt_sort TYPE lvc_t_sort .
    DATA mo_alv TYPE REF TO cl_gui_alv_grid .
    DATA mr_table TYPE REF TO data .
    DATA mv_structure TYPE tabname .
    DATA mt_fcat TYPE lvc_t_fcat .
    DATA mv_save TYPE char1 .
    DATA mv_first_refresh TYPE abap_bool VALUE abap_true. "#EC NOTEXT .
    DATA ms_layout TYPE lvc_s_layo .
    DATA ms_vari TYPE disvariant .
    DATA mt_exclude TYPE ui_functions .

    METHODS on_data_changed
        FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING
        !er_data_changed
        !e_onf4
        !e_onf4_before
        !e_onf4_after
        !e_ucomm .
    METHODS on_data_changed_late
        FOR EVENT data_changed_finished OF cl_gui_alv_grid
      IMPORTING
        !e_modified
        !et_good_cells .
    METHODS prepare_excpt_qinf .
    METHODS prepare_sort
      EXPORTING
        !et_tab TYPE lvc_t_sort .
    METHODS on_button_click
        FOR EVENT button_click OF cl_gui_alv_grid
      IMPORTING
        !es_col_id
        !es_row_no .
    METHODS on_double_click
        FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING
        !e_row
        !e_column
        !es_row_no .
    METHODS on_hotspot_click
        FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING
        !e_row_id
        !e_column_id
        !es_row_no .
    METHODS on_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING
        !e_object
        !e_interactive .
    METHODS on_ucomm
        FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING
        !e_ucomm .
    METHODS set_structure .
    METHODS prepare_layout
      RETURNING
        VALUE(rs_res) TYPE lvc_s_layo .
    METHODS prepare_fieldcat
      EXPORTING
        !et_tab TYPE lvc_t_fcat .
    METHODS prepare_variant
      RETURNING
        VALUE(rs_res) TYPE disvariant .
    METHODS prepare_exclude
      EXPORTING
        !et_tab TYPE ui_functions .
    METHODS show_table .
    METHODS prepare_print
      RETURNING
        VALUE(rs_res) TYPE lvc_s_prnt .
    METHODS alv_input_error
      IMPORTING
        !io_data_changed TYPE REF TO cl_alv_changed_data_protocol
        !is_modcell      TYPE lvc_s_modi .
  PRIVATE SECTION.
*"* private components of class ZCL_MVCFW_CON_ALV
*"* do not include other source files here!!!

    DATA ms_print TYPE lvc_s_prnt .
ENDCLASS.



CLASS ZCL_MVCFW_CON_ALV IMPLEMENTATION.


  METHOD alv_input_error.
    CALL METHOD io_data_changed->add_protocol_entry
      EXPORTING
        i_msgid     = sy-msgid
        i_msgty     = sy-msgty
        i_msgno     = sy-msgno
        i_msgv1     = sy-msgv1
        i_msgv2     = sy-msgv2
        i_msgv3     = sy-msgv3
        i_msgv4     = sy-msgv4
        i_fieldname = is_modcell-fieldname
        i_row_id    = is_modcell-row_id.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( io_container ).
    mv_structure = iv_structure_name.
    IF is_structure IS SUPPLIED.
      CREATE DATA mr_struc LIKE is_structure.
      CLEAR mv_structure.
    ENDIF.
    init_alv( ).
  ENDMETHOD.


  METHOD get_selected_rows.

    DATA: lt_rows TYPE lvc_t_row,
          ls_row  TYPE lvc_s_row.

    FIELD-SYMBOLS: <ls_line>  TYPE any,
                   <lt_table> TYPE STANDARD TABLE.

    ASSIGN mr_table->* TO <lt_table>.

    mo_alv->get_selected_rows(
      IMPORTING
        et_index_rows     = lt_rows    " Numeric IDs of Selected Rows
    ).

    LOOP AT lt_rows INTO ls_row.
      READ TABLE <lt_table> ASSIGNING <ls_line> INDEX ls_row-index.
      IF sy-subrc = 0.
        APPEND <ls_line> TO et_tab.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD init_alv.
    IF mo_alv IS NOT BOUND.
      CREATE OBJECT mo_alv
        EXPORTING
          i_parent = mo_container.    " Parent Container


      SET HANDLER on_toolbar FOR mo_alv.
      SET HANDLER on_ucomm FOR mo_alv.
      SET HANDLER on_double_click FOR mo_alv.
      SET HANDLER on_hotspot_click FOR mo_alv.
      SET HANDLER on_button_click FOR mo_alv.
      SET HANDLER on_data_changed FOR mo_alv.
      SET HANDLER on_data_changed_late FOR mo_alv.

      IF mv_structure IS NOT INITIAL.
        CREATE DATA mr_table TYPE TABLE OF (mv_structure).
      ELSEIF mr_struc IS NOT INITIAL.
        ASSIGN mr_struc->* TO FIELD-SYMBOL(<ls_struc>).
        CREATE DATA mr_table LIKE TABLE OF <ls_struc>.
      ELSE.
        MESSAGE e000(0k) WITH 'cannot init ALV without given structure' INTO sy-msgli.
        RAISE EXCEPTION TYPE zcx_mvcfw_error
          EXPORTING
            textid        = VALUE scx_t100key( msgid = sy-msgid msgno = sy-msgno attr1 = 'MV_MESSAGE_V1' )
            mv_message_v1 = sy-msgv1
*           previous      =
          .
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD on_button_click.
    " must be redefined
  ENDMETHOD.


  METHOD on_data_changed.
    " must be redefined
  ENDMETHOD.


  METHOD on_data_changed_late.
    " must be redefined
  ENDMETHOD.


  METHOD on_double_click.
    " must be redefined
  ENDMETHOD.


  METHOD on_hotspot_click.
    " must be redefined
  ENDMETHOD.


  METHOD on_toolbar.
  ENDMETHOD.


  METHOD on_ucomm.
    " must be redefined
  ENDMETHOD.


  METHOD prepare_exclude.
    " has to be redefined
  ENDMETHOD.


  METHOD prepare_excpt_qinf.
    " must be redefined
  ENDMETHOD.


  METHOD prepare_fieldcat.

    IF mv_structure IS NOT INITIAL.

      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name = mv_structure    " Structure name (structure, table, view)
        CHANGING
          ct_fieldcat      = mt_fcat.    " Field Catalog with Field Descriptions
    ELSE.
      ASSIGN mr_struc->* TO FIELD-SYMBOL(<ls_struc>).
      DATA(lo_strucd) = CAST cl_abap_structdescr(
        cl_abap_typedescr=>describe_by_data( <ls_struc> ) ).

      LOOP AT lo_strucd->components ASSIGNING FIELD-SYMBOL(<ls_comp>).
        ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE <ls_struc> TO FIELD-SYMBOL(<lv_col>).
        DATA(lo_descr) = CAST cl_abap_elemdescr(
                              cl_abap_typedescr=>describe_by_data( <lv_col> ) ).
        DATA(ls_dfies) = lo_descr->get_ddic_field( ).
        APPEND INITIAL LINE TO et_tab ASSIGNING FIELD-SYMBOL(<ls_fcat>).
        <ls_fcat>-fieldname = <ls_comp>-name.
        <ls_fcat>-rollname = ls_dfies-rollname.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD prepare_layout.
  ENDMETHOD.


  METHOD prepare_print.
    " must be redefined
  ENDMETHOD.


  METHOD prepare_sort.

    " must be redefined, if needed
  ENDMETHOD.


  METHOD prepare_variant.
  ENDMETHOD.


  METHOD refresh_table.
    DATA: ls_stbl                     TYPE lvc_s_stbl.

    IF mv_first_refresh = abap_true.
      show_table( ).
      mv_first_refresh = abap_false.
      ev_first_refresh = abap_true.
    ELSE.
      ls_stbl-row = abap_true.
      ls_stbl-col = abap_true.
      mo_alv->refresh_table_display( is_stable = ls_stbl ).
      ev_first_refresh = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD set_structure.
    " must be redefined
  ENDMETHOD.


  METHOD show_table.
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
    ASSIGN mr_table->* TO <lt_table>.

    CHECK mo_alv IS BOUND.

    get_container( )->set_visible( abap_true ).
    ms_layout = prepare_layout( ).
    ms_vari = prepare_variant( ).
    prepare_fieldcat( IMPORTING et_tab = mt_fcat ).
    prepare_exclude( IMPORTING et_tab = mt_exclude ).
    prepare_sort( IMPORTING et_tab = mt_sort ).
    prepare_excpt_qinf( ).

    " maybe mv_save has been set before by a redefinition
    IF mv_save = space.
      mv_save = 'A'.
    ENDIF.

    mo_alv->set_table_for_first_display(
      EXPORTING
*        i_buffer_active               =     " Pufferung aktiv
*         i_bypassing_buffer            = abap_true    " Puffer ausschalten
*        i_consistency_check           =     " Starte Konsistenzverprobung für Schnittstellefehlererkennung
*        i_structure_name              =     " Strukturname der internen Ausgabetabelle
        is_variant                    = ms_vari    " Anzeigevariante
        i_save                        = mv_save    " Anzeigevariante sichern
*        i_default                     = 'X'    " Defaultanzeigevariante
         is_layout                     = ms_layout    " Layout
         is_print                      = ms_print    " Drucksteuerung
*        it_special_groups             =     " Feldgruppen
         it_toolbar_excluding          = mt_exclude     " excludierte Toolbarstandardfunktionen
*        it_hyperlink                  =     " Hyperlinks
*        it_alv_graphics               =     " Tabelle von der Struktur DTC_S_TC
          it_except_qinfo               = mt_excpt_qinf    " Tabelle für die Exception Quickinfo
*        ir_salv_adapter               =     " Interface ALV Adapter
      CHANGING
        it_outtab                     = <lt_table>   " Ausgabetabelle
        it_fieldcatalog               = mt_fcat    " Feldkatalog
        it_sort                       = mt_sort    " Sortierkriterien

*        it_filter                     =     " Filterkriterien
    ).
  ENDMETHOD.
ENDCLASS.
