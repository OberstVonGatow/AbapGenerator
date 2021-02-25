CLASS zcl_template001_model DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS select_data.

    METHODS get_output_data
      RETURNING
        VALUE(rr_data) TYPE REF TO data.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_output TYPE STANDARD TABLE OF tadir.

ENDCLASS.



CLASS zcl_template001_model IMPLEMENTATION.

  METHOD get_output_data.
    rr_data = REF #( mt_output ).
  ENDMETHOD.

  METHOD select_data.

    SELECT FROM tadir
    FIELDS *
    WHERE author = @sy-uname
    AND object = 'DEVC'
    INTO CORRESPONDING FIELDS OF TABLE @mt_output.

  ENDMETHOD.

ENDCLASS.
