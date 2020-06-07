*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
"! <p class="shorttext synchronized" lang="en">Output any variable to console</p>

CLASS lcl_delegate DEFINITION
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_adtcon.

    "! <p class="shorttext synchronized" lang ="en"></p>
    "!
    "! @parameter out | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS wrap
      IMPORTING
        out           TYPE REF TO if_oo_adt_intrnl_classrun
      RETURNING
        VALUE(result) TYPE REF TO zif_adtcon.

    METHODS constructor
      IMPORTING
        out TYPE REF TO if_oo_adt_intrnl_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: out TYPE REF TO if_oo_adt_intrnl_classrun.

ENDCLASS.



CLASS lcl_delegate IMPLEMENTATION.

  METHOD wrap.
    result = NEW zcl_adtcon( out ).
  ENDMETHOD.

  METHOD if_oo_adt_intrnl_classrun~begin_section.
    out->begin_section(
      EXPORTING
        title = title
      RECEIVING
        output = output
    ).
  ENDMETHOD.

  METHOD if_oo_adt_intrnl_classrun~display.
    out->display(
      EXPORTING
        data   = data
        name   = name
      RECEIVING
        output = output
    ).
  ENDMETHOD.

  METHOD if_oo_adt_intrnl_classrun~end_section.
    out->end_section(
      RECEIVING
        output = output
    ).
  ENDMETHOD.

  METHOD if_oo_adt_intrnl_classrun~get.
    out->get(
      EXPORTING
        data   = data
        name   = name
      RECEIVING
        output = output
    ).
  ENDMETHOD.

  METHOD if_oo_adt_intrnl_classrun~line.
    out->line(
      RECEIVING
        output = output
    ).
  ENDMETHOD.

  METHOD if_oo_adt_intrnl_classrun~next_section.
    out->next_section(
      EXPORTING
        title  = title
      RECEIVING
        output = output
    ).
  ENDMETHOD.

  METHOD if_oo_adt_intrnl_classrun~write.
    out->write(
      EXPORTING
        data   = data
        name   = name
      RECEIVING
        output = output
    ).
  ENDMETHOD.

  METHOD if_oo_adt_intrnl_classrun~write_data.
    out->write_data(
      EXPORTING
        value  = value
        name   = name
      RECEIVING
        output = output
    ).
  ENDMETHOD.

  METHOD if_oo_adt_intrnl_classrun~write_text.
    out->write_text(
      EXPORTING
        text = text
      RECEIVING
        output = output
    ).
  ENDMETHOD.

  METHOD constructor.

    me->out = out.

  ENDMETHOD.

ENDCLASS.
