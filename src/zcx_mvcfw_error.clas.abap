class ZCX_MVCFW_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  data MV_MESSAGE_V1 type SYMSGV .
  data MV_MESSAGE_V2 type SYMSGV .
  data MV_MESSAGE_V3 type SYMSGV .
  data MV_MESSAGE_V4 type SYMSGV .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MV_MESSAGE_V1 type SYMSGV optional
      !MV_MESSAGE_V2 type SYMSGV optional
      !MV_MESSAGE_V3 type SYMSGV optional
      !MV_MESSAGE_V4 type SYMSGV optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_MVCFW_ERROR IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MV_MESSAGE_V1 = MV_MESSAGE_V1 .
me->MV_MESSAGE_V2 = MV_MESSAGE_V2 .
me->MV_MESSAGE_V3 = MV_MESSAGE_V3 .
me->MV_MESSAGE_V4 = MV_MESSAGE_V4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
