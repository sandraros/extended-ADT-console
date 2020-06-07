"! <p class="shorttext synchronized" lang="en">Output any variable to console</p>
CLASS zcl_adtcon DEFINITION
  PUBLIC
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
    CONSTANTS: c_column_space TYPE string VALUE `  `.

    TYPES: tt_width TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    METHODS:
      write_data
        IMPORTING
          data          TYPE data
          name          TYPE string OPTIONAL
          name_output   TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(result) TYPE string_table
        RAISING
          lcx_error,
      write_primitive
        IMPORTING
          simple        TYPE simple
          name          TYPE string OPTIONAL
          name_output   TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(result) TYPE string_table,
      write_structure
        IMPORTING
          structure     TYPE any
          name          TYPE string OPTIONAL
          name_output   TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(result) TYPE string_table
        RAISING
          lcx_error,
      write_component_names
        IMPORTING
          rtti          TYPE REF TO cl_abap_structdescr
          widths        TYPE tt_width OPTIONAL
        RETURNING
          VALUE(result) TYPE string,
      write_table
        IMPORTING
          table         TYPE ANY TABLE
          name          TYPE string OPTIONAL
          name_output   TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(result) TYPE string_table
        RAISING
          lcx_error,
      write_structured_line
        IMPORTING
          structure     TYPE any
          widths        TYPE tt_width
        RETURNING
          VALUE(result) TYPE string_table
        RAISING
          lcx_error,
      write_reference
        IMPORTING
          reference     TYPE REF TO data
          name          TYPE string OPTIONAL
          name_output   TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(result) TYPE string_table,
      get_widths_of_component_values
        IMPORTING
          structure     TYPE any OPTIONAL
        RETURNING
          VALUE(widths) TYPE tt_width
        RAISING
          lcx_error,
      get_width_data
        IMPORTING
          data          TYPE data
        RETURNING
          VALUE(result) TYPE i
        RAISING
          lcx_error,
      get_max_widths
        IMPORTING
          widths_1      TYPE tt_width
          widths_2      TYPE tt_width
        RETURNING
          VALUE(widths) TYPE tt_width
        RAISING
          lcx_error,
      get_widths_of_component_names
        IMPORTING
          rtti          TYPE REF TO cl_abap_structdescr
        RETURNING
          VALUE(widths) TYPE tt_width,
      get_longest_string_width
        IMPORTING
          string_table  TYPE string_table
        RETURNING
          VALUE(result) TYPE i.

ENDCLASS.



CLASS zcl_adtcon IMPLEMENTATION.

  METHOD wrap.
*    result = NEW lcl_delegate( out ).
    result = NEW zcl_adtcon( out ).
  ENDMETHOD.

  METHOD if_oo_adt_intrnl_classrun~begin_section.
    out->begin_section(
        title = title
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
    out->end_section( ).
  ENDMETHOD.

  METHOD if_oo_adt_intrnl_classrun~get.
    IF data IS NOT SUPPLIED.
      output = out->get( name = name ).
    ELSE.
      output = out->get( data = data name = name ).
    ENDIF.
  ENDMETHOD.

  METHOD if_oo_adt_intrnl_classrun~line.
    out->line( ).
  ENDMETHOD.

  METHOD if_oo_adt_intrnl_classrun~next_section.
    out->next_section( title = title ).
  ENDMETHOD.

  METHOD if_oo_adt_intrnl_classrun~write.

    TRY.
        DATA(result) = write_data( data = data name = name name_output = abap_true ).
        out->write_text( concat_lines_of( table = result sep = |\n| ) ). " && |\n\n|
      CATCH lcx_error.
        out->write_text( 'Data type not yet supported ...' ).
    ENDTRY.
    output = me.

  ENDMETHOD.

  METHOD if_oo_adt_intrnl_classrun~write_data.

    TRY.
        DATA(result) = write_data( data = value name = name name_output = abap_true ).
        out->write_text( concat_lines_of( table = result sep = |\n| ) ).
      CATCH lcx_error.
        out->write_text( 'Data type not yet supported ...' ).
    ENDTRY.
    output = me.

  ENDMETHOD.

  METHOD if_oo_adt_intrnl_classrun~write_text.

    TRY.
        DATA(result) = write_data( data = text name_output = abap_false ).
        out->write_text( concat_lines_of( table = result sep = |\n| ) ).
      CATCH lcx_error.
        out->write_text( 'Data type not yet supported ...' ).
    ENDTRY.
    output = me.

  ENDMETHOD.

  METHOD constructor.

    me->out = out.

  ENDMETHOD.

  METHOD write_data.

    DATA(a) = cl_abap_typedescr=>describe_by_data( p_data = data ).
    CASE a->kind.
      WHEN a->kind_elem.
        result = write_primitive( simple = data name = name name_output = name_output ).
      WHEN a->kind_struct.
        result = write_structure( structure = data name = name name_output = name_output ).
      WHEN a->kind_table.
        result = write_table( table = data name = name name_output = name_output ).
      WHEN a->kind_ref.
        result = write_reference( reference = data name = name name_output = name_output ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE lcx_error.
    ENDCASE.

  ENDMETHOD.

  METHOD write_primitive.

    DATA: string TYPE string.

    DATA(rtti) = cl_abap_typedescr=>describe_by_data( simple ).

    IF name_output = abap_true.
      CASE rtti->type_kind.
        WHEN rtti->typekind_char OR rtti->typekind_string.
          result = VALUE #( ).
        WHEN OTHERS.
          result = VALUE #( ( |Field{ c_column_space }| ) ).
      ENDCASE.
    ENDIF.

    CASE rtti->type_kind.

      WHEN rtti->typekind_date.
        FIELD-SYMBOLS <date> TYPE d.
        ASSIGN simple TO <date> CASTING.
        result = VALUE #( BASE result ( |{ <date> DATE = ISO }{ c_column_space }| ) ).

      WHEN rtti->typekind_time.
        FIELD-SYMBOLS <time> TYPE t.
        ASSIGN simple TO <time> CASTING.
        result = VALUE #( BASE result ( |{ <time> TIME = ISO }{ c_column_space }| ) ).

      WHEN rtti->typekind_packed.
        FIELD-SYMBOLS <packed> TYPE p.
        ASSIGN simple TO <packed> CASTING.
        " 0.00  -> 0.0
        " 3.140 -> 3.14
        " 3     -> 3
        string = |{ <packed> }{ c_column_space }|.
        IF string CA '.'.
          string = substring_before( val = string sub = '.' )
            && replace( val = substring_from( val = string sub = '.' ) regex = '^([.]0)0+(-? *)$|0+(-? *)$' with = '$1$2$3' ).
        ENDIF.
        result = VALUE #( BASE result ( string ) ).

      WHEN rtti->typekind_float.
        FIELD-SYMBOLS <float> TYPE f.
        ASSIGN simple TO <float> CASTING.
        IF <float> IS INITIAL.
          " 0E?  -> 0
          string = |0{ c_column_space }|.
        ELSE.
          " E+00  -> E0
          " E+02  -> E2
          " E+10  -> E10
          " E-02 -> E-2
          string = replace( val = |{ <float> STYLE = SCIENTIFIC }{ c_column_space }| regex = '(E-?)[+]?0*([^0]*\d +)$' with = '$1$2' ).
          " 1E-2  -> 1.0E-2
          IF substring_before( val = string sub = 'E' ) NA '.'.
            string = insert( val = string sub = '.0' off = find( val = string sub = 'E' ) ).
          ENDIF.
        ENDIF.
        result = VALUE #( BASE result ( string ) ).

      WHEN rtti->typekind_char OR rtti->typekind_string.
        result = VALUE #( BASE result ( |{ simple }| ) ).

      WHEN OTHERS.
        result = VALUE #( BASE result ( |{ simple }{ c_column_space }| ) ).
    ENDCASE.

  ENDMETHOD.

  METHOD write_structured_line.

    DATA: comp_result_line TYPE REF TO string.
    FIELD-SYMBOLS: <widths>    TYPE tt_width,
                   <component> TYPE any.

    DATA(rtti) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( p_data = structure ) ).

    result = VALUE #( ( ) ).
    ASSIGN result[ 1 ] TO FIELD-SYMBOL(<first_line>).

    DO lines( rtti->components ) TIMES.
      ASSIGN COMPONENT sy-index OF STRUCTURE structure TO <component>.
      ASSERT sy-subrc = 0.
      DATA(comp_result) = write_data( data = <component> ).
      CASE lines( comp_result ).
        WHEN 0.
          RAISE EXCEPTION TYPE lcx_error.
        WHEN 1.
          <first_line> = COND #(
                WHEN <first_line> IS INITIAL
                THEN comp_result[ 1 ]
                ELSE |{ <first_line> }{ comp_result[ 1 ] }| ).
        WHEN OTHERS.
          " determine width of longest line of component
          DATA(longest_width) = get_longest_string_width( comp_result ).
          " Add empty lines to RESULT so that to make it easier the transfer of COMP_RESULT to RESULT.
          WHILE lines( result ) < lines( comp_result ).
            APPEND INITIAL LINE TO result.
          ENDWHILE.
          " calculate column where each line of COMP_RESULT must start.
          DATA(start_column) = COND #( WHEN result[ 1 ] IS INITIAL THEN 0 ELSE strlen( result[ 1 ] ) )."+ strlen( c_column_space ) ).
          " Transfer COMP_RESULT to RESULT
          LOOP AT comp_result REFERENCE INTO comp_result_line.
            " determine number of spaces to add so that each line of COMP_RESULT starts at position START_COLUMN in RESULT.
            DATA(spaces_to_add) = COND i( LET i = start_column - strlen( result[ sy-tabix ] ) IN WHEN i < 0 THEN 0 ELSE i ).
            result[ sy-tabix ] = result[ sy-tabix ] && repeat( val = ` ` occ = spaces_to_add ) && comp_result_line->*.
          ENDLOOP.
      ENDCASE.
    ENDDO.

  ENDMETHOD.

  METHOD write_structure.

    FIELD-SYMBOLS: <widths>    TYPE tt_width,
                   <component> TYPE any.

    DATA(rtti) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( p_data = structure ) ).

    DATA(widths) = get_max_widths(
        widths_1 = get_widths_of_component_names( rtti )
        widths_2 = get_widths_of_component_values( structure = structure ) ).

    result = VALUE #(
        ( |Structure{ c_column_space }| )
        " COLUMN HEADERS
        ( write_component_names( rtti = rtti widths = widths ) )
        " VALUES OF STRUCTURE COMPONENTS
        ( LINES OF write_structured_line( structure = structure widths = widths ) ) ).

  ENDMETHOD.

  METHOD write_component_names.

    FIELD-SYMBOLS: <widths>    TYPE tt_width,
                   <component> TYPE any.

    IF widths IS NOT INITIAL.

      LOOP AT widths INTO DATA(width).
        result = |{ result }{ rtti->components[ sy-tabix ]-name WIDTH = width }|.
      ENDLOOP.

    ELSE.

      LOOP AT rtti->components REFERENCE INTO DATA(component).
        result = |{ result }{ component->name }|.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD write_table.

    FIELD-SYMBOLS: <line> TYPE any.

    DATA(rtti) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( p_data = table ) ).

    IF rtti->get_table_line_type( )->kind = cl_abap_typedescr=>kind_struct.

      DATA(rtti_line) = CAST cl_abap_structdescr( rtti->get_table_line_type( ) ).

      " Calculate widths of each column - The whole table must be parsed.
      DATA(widths) = get_widths_of_component_names( rtti = rtti_line ).
      LOOP AT table ASSIGNING <line>.
        widths = get_max_widths( widths_1 = widths widths_2 = get_widths_of_component_values( structure = <line> ) ).
      ENDLOOP.

      " STRUCTURED LINES
      result = VALUE #(
        ( |Table{ c_column_space }| )
        " HEADER LINE
        ( write_component_names( rtti = rtti_line widths = widths ) )
        " LINES
        ( LINES OF VALUE #(
            FOR <line2> IN table
            ( LINES OF write_structured_line( structure = <line> widths = widths ) ) ) ) ).

    ELSE.

      " LINES NOT STRUCTURED
      result = VALUE #(
        ( |Table{ c_column_space }| )
        ( LINES OF VALUE #(
            FOR <line2> IN table
            ( LINES OF write_data( <line> ) ) ) ) ).

    ENDIF.

  ENDMETHOD.

  METHOD write_reference.

*    ASSIGN reference->* TO FIELD-SYMBOL(<data>).
*    IF sy-subrc = 0.
*      result = |->{ write_data( <data> ) }|.
*    ELSE.
*      result = '->'.
*    ENDIF.

  ENDMETHOD.

  METHOD get_max_widths.

    IF lines( widths_1 ) <> lines( widths_2 ).
      RAISE EXCEPTION TYPE lcx_error.
    ENDIF.

    LOOP AT widths_1 REFERENCE INTO DATA(width).
      APPEND nmax( val1 = width->* val2 = widths_2[ sy-tabix ] ) TO widths.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_widths_of_component_names.

    LOOP AT rtti->components REFERENCE INTO DATA(component).
      APPEND strlen( component->name ) + strlen( c_column_space ) TO widths.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_widths_of_component_values.

    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE structure TO FIELD-SYMBOL(<component>).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      APPEND get_width_data( <component> ) TO widths.
    ENDDO.

  ENDMETHOD.

  METHOD get_width_data.

    result = get_longest_string_width( write_data( data = data ) ).

  ENDMETHOD.

  METHOD get_longest_string_width.

    result = REDUCE i(
          INIT i = 0
          FOR <string> IN string_table
          NEXT i = nmax( val1 = i val2 = strlen( <string> ) ) ).

  ENDMETHOD.

ENDCLASS.

