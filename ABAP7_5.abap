*----------------------------*
* Author  : Alper Dedeoglu   *
*----------------------------*
REPORT zbcr_abap_75x.

CLASS cx_overflow DEFINITION INHERITING FROM cx_static_check.
*-Alper
ENDCLASS   .

CLASS cl_main DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_filter,
             kunnr TYPE kna1-kunnr,
           END OF ty_filter.
    CLASS-DATA: lt_filter TYPE HASHED TABLE OF ty_filter
                          WITH UNIQUE KEY kunnr.
    CLASS-METHODS :
*- ABAP Filter Operand Example
      run_filter,
*- ABAP Case Formatter Example
      case_format,
*- ABAP Alpha-Input Output Example.
      alpha_conv,
*- ABAP Date-Formatter Example
      date_time_format,
*- ABAP Switch Operand Example ( Could be useful in Design Patterns ).
      switch,
*- ABAP Corresponding Operand Example ( New Version of MOVE-CORRESPONDING ).
      corresponding,
*- ABAP Base Operand Example
      base,
*- ABAP Cond Opreand Example
      cond,
*- ABAP LOOP with GROUP BY Example
      loop_and_group_by,
*- ABAP REDUCE Count example
      reduce_count.
*- ABAP REDUCE
ENDCLASS.

CLASS cl_main IMPLEMENTATION.
  METHOD run_filter.

    SELECT * FROM kna1 INTO
             TABLE @DATA(lt_kna1)
             UP TO 100 ROWS.

    lt_filter = VALUE #( ( kunnr = '0001141664' ) ).

    DATA(lt_filtered) = FILTER #( lt_kna1 IN lt_filter WHERE kunnr = kunnr ).
    DATA(lt_exc_filtered) = FILTER #( lt_kna1 EXCEPT IN lt_filter WHERE kunnr = kunnr ).

    cl_demo_output=>display( lt_filtered ) .
    cl_demo_output=>display( lt_exc_filtered ).
  ENDMETHOD.

  METHOD case_format.
    cl_demo_output=>display( |{ 'Alper' CASE = (cl_abap_format=>c_raw)   }| ).
    cl_demo_output=>display( |{ 'Alper' CASE = (cl_abap_format=>c_lower) }| ).
    cl_demo_output=>display( |{ 'Alper' CASE = (cl_abap_format=>c_lower) }| ).
  ENDMETHOD.

  METHOD alpha_conv.
    DATA lv_vbeln TYPE vbrk-vbeln.

    lv_vbeln = '90000013'.

    lv_vbeln = |{ lv_vbeln ALPHA = IN }|.
    cl_demo_output=>display( |{ 'Alpha-In:'  && space && lv_vbeln }| ).

    lv_vbeln = |{ lv_vbeln ALPHA = OUT }|.
    cl_demo_output=>display( |{ 'Alpha-Out:' && space && lv_vbeln }| ).

  ENDMETHOD.

  METHOD date_time_format.
*--date conversion
    DATA: lv_date TYPE sy-datum,
          lv_time TYPE sy-uzeit.
    lv_date = sy-datum.
    lv_time = sy-uzeit.
    WRITE / |{ lv_date DATE = ISO }|.
    WRITE / |{ lv_date DATE = USER }|.
    WRITE / |{ lv_date DATE = ENVIRONMENT }|.

    WRITE / |{ lv_date DATE = ISO }| && '/' && |{ lv_time TIME = ISO }|.

  ENDMETHOD.

  METHOD switch.
    DATA(lr_out) = cl_demo_output=>new( ).
    DO.
      TRY.
          lr_out->write( SWITCH string( sy-index
                                      WHEN 1 THEN 'One'
                                      WHEN 2 THEN 'Two'
                                      WHEN 3 THEN 'Three'
                                      ELSE THROW cx_overflow( ) ) ).
        CATCH cx_overflow.
          lr_out->display( ).
          EXIT.
      ENDTRY.
    ENDDO.
  ENDMETHOD.

  METHOD corresponding.
    DATA:
      BEGIN OF ls_struct1,
        col1 TYPE i VALUE 11,
        col2 TYPE i VALUE 12,
      END OF ls_struct1.

    DATA:
      BEGIN OF ls_struct2,
        col2 TYPE i,
        col3 TYPE i VALUE 23,
      END OF ls_struct2.
*---Since the RHS does not know anything of the LHS, component col3 of struct2 does not keep its former value but is initialized.
    ls_struct2 = CORRESPONDING #( ls_struct1 ).
  ENDMETHOD.

  METHOD base.
    DATA lt_itab TYPE TABLE OF i.
*---Again, the RHS does not know the LHS and the result of the second assignment is three lines containing 4, 5, 6.
    lt_itab = VALUE #( ( 1 ) ( 2 ) ( 3 ) ).
    lt_itab = VALUE #( ( 4 ) ( 5 ) ( 6 ) ).
*-- With BASE statement it will append new lines to existing table.
    lt_itab = VALUE #( ( 1 ) ( 2 ) ( 3 ) ).
    lt_itab = VALUE #( BASE lt_itab ( 4 ) ( 5 ) ( 6 ) ).
  ENDMETHOD.

  METHOD cond.
    DATA lv_age TYPE i VALUE 26.
    TRY .
        cl_demo_output=>display( COND string( WHEN lv_age <  30 THEN 'Young'
                                              WHEN lv_age >= 30
                                              AND  lv_age <= 60 THEN 'Adult'
                                              ELSE THROW cx_overflow( ) ) ).
      CATCH cx_overflow.
        cl_demo_output=>display('None').
    ENDTRY.
  ENDMETHOD.

  METHOD loop_and_group_by.

    DATA lt_flights TYPE TABLE OF spfli WITH EMPTY KEY.
    SELECT * FROM  spfli
             WHERE carrid = 'LH'
             INTO TABLE @lt_flights.
**- Firstly creating groups based on carrid and cityfr
**- Then appending data of groups to lt_members table
    DATA lt_members LIKE lt_flights.
    LOOP AT lt_flights INTO DATA(ls_flight)
         GROUP BY ( carrier = ls_flight-carrid cityfr = ls_flight-cityfrom )
                  ASCENDING
                  ASSIGNING FIELD-SYMBOL(<group>).
      CLEAR lt_members.
      LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<flight>).
        lt_members = VALUE #( BASE lt_members ( <flight> ) ).
      ENDLOOP.
      cl_demo_output=>write( lt_members ).
    ENDLOOP.
    cl_demo_output=>display( ).

*    TYPES t_flights LIKE lt_flights.
*    DATA: out2 TYPE REF TO if_demo_output.
*
*    out2 = REDUCE #( INIT o = cl_demo_output=>new( )
*                          FOR GROUPS <group> OF ls_flight IN lt_flights
*                          GROUP BY ( carrier = ls_flight-carrid cityfr = ls_flight-cityfrom ) ASCENDING
*                          LET lt_members = VALUE t_flights( FOR m IN GROUP <group> ( m ) )
*                          IN NEXT o = o->write( lt_members ) ).
*    out2->display( ).
  ENDMETHOD.

  METHOD reduce_count.
    DATA lt_flights TYPE TABLE OF spfli WITH EMPTY KEY.
    SELECT * FROM  spfli
             WHERE carrid = 'LH'
             INTO TABLE @lt_flights.

    DATA(lv_lines) = REDUCE #( INIT lv_count = 0 FOR ls_flights IN lt_flights
                               WHERE ( carrid   = 'LH' AND cityfrom = 'ROM' )
                               NEXT lv_count = lv_count + 1 ).
    WRITE: / lv_lines.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION .

  "cl_main=>run_filter( ).
  "cl_main=>case_format( ).
  "cl_main=>alpha_conv( ).
  "cl_main=>date_time_format( ).
  "cl_main=>switch( ).
  "cl_main=>corresponding( ).
  "cl_main=>base( ).
  "cl_main=>cond( ).
  "cl_main=>loop_and_group_by( ).
  "cl_main=>reduce_count( ).

END-OF-SELECTION.
