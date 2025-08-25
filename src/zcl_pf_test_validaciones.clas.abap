CLASS zcl_pf_test_validaciones DEFINITION
  PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

ENDCLASS.

CLASS zcl_pf_test_validaciones IMPLEMENTATION.

METHOD if_oo_adt_classrun~main.
  " Declaración explícita de variable
  DATA lv_valid TYPE abap_bool.

  " Instancia del validador
  DATA lo_validator TYPE REF TO zcl_pf_validacines.
  lo_validator = NEW zcl_pf_validacines( ).

  lv_valid = lo_validator->validate_status_and_priority( iv_status = 'PE' iv_priority = 'A' ).
  out->write( |Validación PE/A: { COND string( WHEN lv_valid = abap_true THEN 'SÍ' ELSE 'NO' ) }| ).

  lv_valid = lo_validator->validate_status_and_priority( iv_status = 'CO' iv_priority = 'A' ).
  out->write( |Validación XX/A: { COND string( WHEN lv_valid = abap_true THEN 'SÍ' ELSE 'NO' ) }| ).

  lv_valid = lo_validator->validate_status_and_priority( iv_status = 'PE' iv_priority = 'Z' ).
  out->write( |Validación PE/Z: { COND string( WHEN lv_valid = abap_true THEN 'SÍ' ELSE 'NO' ) }| ).
ENDMETHOD.

ENDCLASS.
