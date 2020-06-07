"! <p class="shorttext synchronized" lang="en">Extended ADT console</p>
INTERFACE zif_adtcon
  PUBLIC .
  INTERFACES if_oo_adt_intrnl_classrun.

  ALIASES write          FOR if_oo_adt_intrnl_classrun~write        .
  ALIASES write_data     FOR if_oo_adt_intrnl_classrun~write_data   .
  ALIASES write_text     FOR if_oo_adt_intrnl_classrun~write_text   .
  ALIASES display        FOR if_oo_adt_intrnl_classrun~display      .
  ALIASES begin_section  FOR if_oo_adt_intrnl_classrun~begin_section.
  ALIASES end_section    FOR if_oo_adt_intrnl_classrun~end_section  .
  ALIASES line           FOR if_oo_adt_intrnl_classrun~line         .
  ALIASES next_section   FOR if_oo_adt_intrnl_classrun~next_section .
  ALIASES get            FOR if_oo_adt_intrnl_classrun~get          .
ENDINTERFACE.
