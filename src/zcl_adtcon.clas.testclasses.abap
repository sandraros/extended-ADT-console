*"* use this source file for your ABAP unit test classes

CLASS ltcl_compare_with_standard DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      simple_date FOR TESTING RAISING cx_static_check,
      simple_time FOR TESTING RAISING cx_static_check,
      simple_string FOR TESTING RAISING cx_static_check,
      simple_packed_2_decs FOR TESTING RAISING cx_static_check,
      simple_fltp FOR TESTING RAISING cx_static_check,
      simple_decfloat FOR TESTING RAISING cx_static_check,
      simple_hex FOR TESTING RAISING cx_static_check,
      structure FOR TESTING RAISING cx_static_check,
      table FOR TESTING RAISING cx_static_check,

      test IMPORTING data TYPE data,
      new_output
        RETURNING
          VALUE(result) TYPE REF TO if_oo_adt_intrnl_classrun.

ENDCLASS.


CLASS ltcl_compare_with_standard IMPLEMENTATION.

  METHOD simple_date.

    test( sy-datum ).

  ENDMETHOD.

  METHOD simple_time.

    test( sy-uzeit ).

  ENDMETHOD.

  METHOD simple_string.

    DATA(string) = |hello world |.
    test( string ).

  ENDMETHOD.

  METHOD simple_packed_2_decs.

    TYPES ty TYPE p LENGTH 8 DECIMALS 3.
    test( CONV ty( '3.140' ) ).
    test( CONV ty( '0.500' ) ).
    test( CONV ty( '0.000' ) ).
    test( CONV ty( -1 ) ).
    test( CONV ty( -'1.1' ) ).

  ENDMETHOD.

  METHOD simple_fltp.

    test( CONV f( '0.0314' ) ).
    test( CONV f( '1E-2' ) ).
    test( CONV f( 0 ) ).
    test( CONV f( -'0.001' ) ).

  ENDMETHOD.

  METHOD simple_decfloat.

    test( CONV decfloat16( '0.0314' ) ).
    test( CONV decfloat16( '1E-2' ) ).
    test( CONV decfloat16( 0 ) ).
    test( CONV decfloat16( -'0.001' ) ).

  ENDMETHOD.

  METHOD simple_hex.

    TYPES ty TYPE x LENGTH 8.
    test( CONV ty( '00FFBB' ) ).

  ENDMETHOD.

  METHOD structure.

    TYPES : BEGIN OF ty,
              d    TYPE d,
              iiii TYPE i,
              t    TYPE t,
            END OF ty.
    test( VALUE ty( d = sy-datum iiii = 50 t = sy-uzeit ) ).

  ENDMETHOD.

  METHOD table.

    TYPES : BEGIN OF ty,
              c    TYPE C LENGTH 4,
              iiii TYPE i,
              f    TYPE f,
            END OF ty,
            tt TYPE STANDARD TABLE OF ty WITH EMPTY KEY.
    test( VALUE tt( ( c = 'A' iiii = 50 f = 10 ) ( c = 'ABCD' iiii = 5 f = -'5E2' ) ) ).

  ENDMETHOD.

  METHOD test.

    DATA(std) = new_output( ).
    DATA(cus) = CAST if_oo_adt_intrnl_classrun( NEW zcl_adtcon( new_output( ) ) ).

    DATA(std_result) = std->write( data )->get( ).
    DATA(cus_result) = cus->write( data )->get( ).

    cl_abap_unit_assert=>assert_equals( act = cus_result exp = std_result ).

  ENDMETHOD.

  METHOD new_output.

    CALL METHOD ('\CLASS-POOL=CL_OO_ADT_RES_CLASSRUN\CLASS=LCL_CLASSRUN_OUTPUT')=>('NEW')
      EXPORTING
        mode   = 'TEXT'
      RECEIVING
        output = result.

  ENDMETHOD.

ENDCLASS.
