class ZCL_MVCFW_CON_CFW definition
  public
  abstract
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_CONTAINER type ref to CL_GUI_CONTAINER optional .
  methods GET_CONTAINER
    returning
      value(RO_RES) type ref to CL_GUI_CONTAINER .
  methods REFRESH
    raising
      ZCX_MVCFW_ERROR .
  methods CHECK_CHANGED_DATA
    raising
      zcx_mvcfw_error .
  methods HIDE .
  methods UNHIDE .
  methods FREE .
protected section.

  data MR_STRUC type ref to DATA .
*"* protected components of class ZCL_MVCFW_CON_CFW
*"* do not include other source files here!!!
  data MO_CONTAINER type ref to CL_GUI_CONTAINER .
private section.
*"* private components of class ZCL_MVCFW_CON_CFW
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_MVCFW_CON_CFW IMPLEMENTATION.


method CHECK_CHANGED_DATA.

    " must be redefined

endmethod.


method CONSTRUCTOR.
  mo_container = io_container.
endmethod.


method FREE.
endmethod.


method GET_CONTAINER.

  ro_res = mo_container.

endmethod.


method HIDE.
  mo_container->set_visible( abap_false ).
endmethod.


method REFRESH.

    " must be redefined

endmethod.


method UNHIDE.
  mo_container->set_visible( abap_true ).
endmethod.
ENDCLASS.
