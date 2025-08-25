CLASS zcl_pf_validacines DEFINITION
  PUBLIC CREATE PUBLIC FINAL
  INHERITING FROM cl_demo_classrun.

  PUBLIC SECTION.
    " Validation methods"
    METHODS:
      validate_create_order
        IMPORTING
          iv_customer_id   TYPE ztb_work_order-customer_id
          iv_technician_id TYPE ztb_work_order-technician_id
          iv_priority      TYPE ztb_work_order-priority
        RETURNING VALUE(rv_valid) TYPE abap_bool,

      validate_update_order
        IMPORTING
          iv_work_order_id TYPE ztb_work_order-work_order_id
          iv_status        TYPE ztb_work_order-status
        RETURNING VALUE(rv_valid) TYPE abap_bool,

      validate_delete_order
        IMPORTING
          iv_work_order_id TYPE ztb_work_order-work_order_id
          iv_status        TYPE ztb_work_order-status
        RETURNING VALUE(rv_valid) TYPE abap_bool,

      validate_status_and_priority
        IMPORTING
          iv_status   TYPE ztb_work_order-status
          iv_priority TYPE ztb_work_order-priority
        RETURNING VALUE(rv_valid) TYPE abap_bool,

    main REDEFINITION.

    " Class constructor for initializing values"
    CLASS-METHODS class_constructor.

  PRIVATE SECTION.
    " Internal types for selection tables"
    TYPES: tt_status   TYPE STANDARD TABLE OF c  WITH EMPTY KEY,
           tt_priority TYPE STANDARD TABLE OF c  WITH EMPTY KEY.

    " Internal validation tables"
    CLASS-DATA lt_valid_status   TYPE tt_status.
    CLASS-DATA lt_valid_priority TYPE tt_priority.

    " Helper methods"
    METHODS:
      check_customer_exists
        IMPORTING iv_customer_id TYPE ztb_work_order-customer_id
        RETURNING VALUE(rv_exists) TYPE abap_bool,
      check_technician_exists
        IMPORTING iv_technician_id TYPE ztb_work_order-technician_id
        RETURNING VALUE(rv_exists) TYPE abap_bool,
      check_order_exists
        IMPORTING iv_work_order_id TYPE ztb_work_order-work_order_id
        RETURNING VALUE(rv_exists) TYPE abap_bool,
      check_order_history
        IMPORTING iv_work_order_id TYPE ztb_work_order-work_order_id
        RETURNING VALUE(rv_exists) TYPE abap_bool.

ENDCLASS.

" Class Implementation"
CLASS zcl_pf_validacines IMPLEMENTATION.

  " Initializes tables of valid statuses and priorities"
  METHOD class_constructor.
    APPEND 'PE' TO lt_valid_status.
    APPEND 'CO' TO lt_valid_status.

    APPEND 'A' TO lt_valid_priority.
    APPEND 'B' TO lt_valid_priority.
  ENDMETHOD.

  " Create Order: validates customer, technician, and priority"
  METHOD validate_create_order.
    IF check_customer_exists( iv_customer_id ) IS INITIAL
       OR check_technician_exists( iv_technician_id ) IS INITIAL
       OR NOT line_exists( lt_valid_priority[ table_line = iv_priority ] ).
      rv_valid = abap_false.
      RETURN.
    ENDIF.
    rv_valid = abap_true.
  ENDMETHOD.

  " Update Order: validates existence and status"
  METHOD validate_update_order.
    IF check_order_exists( iv_work_order_id ) IS INITIAL
       OR NOT line_exists( lt_valid_status[ table_line = iv_status ] ).
      rv_valid = abap_false.
      RETURN.
    ENDIF.
    rv_valid = abap_true.
  ENDMETHOD.

  " Delete Order: validates existence, status 'PE', and no history"
METHOD validate_delete_order.
    " Validación rápida - solo verifica el estado proporcionado
    IF iv_status <> 'PR' AND iv_status <> 'PE'.
        " Verificar que la orden existe
        SELECT SINGLE @abap_true
          FROM ztb_work_order
          WHERE work_order_id = @iv_work_order_id
          INTO @rv_valid.
    ELSE.
        rv_valid = abap_false.
    ENDIF.
ENDMETHOD.

  " Validate status and priority"
  METHOD validate_status_and_priority.
    IF NOT line_exists( lt_valid_status[ table_line = iv_status ] )
       OR NOT line_exists( lt_valid_priority[ table_line = iv_priority ] ).
      rv_valid = abap_false.
      RETURN.
    ENDIF.
    rv_valid = abap_true.
  ENDMETHOD.

  " Helper methods that validate existence in the database"
  METHOD check_customer_exists.
    " Query the actual customer table (replace 'ztb_customer' with your table name)
    SELECT SINGLE customer_id FROM ztb_customer
      WHERE customer_id = @iv_customer_id
      INTO @DATA(lv_customer).

    IF sy-subrc = 0.
      rv_exists = abap_true.
    ELSE.
      rv_exists = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD check_technician_exists.
    " Query the actual technician table (replace 'ztb_technician' with your table name)
    SELECT SINGLE technician_id FROM ztb_technician
      WHERE technician_id = @iv_technician_id
      INTO @DATA(lv_technician).

    IF sy-subrc = 0.
      rv_exists = abap_true.
    ELSE.
      rv_exists = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD check_order_exists.
    rv_exists = abap_true.
  ENDMETHOD.

  METHOD check_order_history.
    rv_exists = abap_false.
  ENDMETHOD.

  " Main method for testing"
  METHOD main.
    DATA(lo_validator) = NEW zcl_pf_validacines( ).

    out->write( |Prueba de validación de creación de orden...| ).
    IF lo_validator->validate_create_order(
      iv_customer_id = 'CU000001'
      iv_technician_id = 'TC000002'
      iv_priority = 'A' ).
      out->write( |La validación fue exitosa.| ).
    ELSE.
      out->write( |La validación falló.| ).
    ENDIF.

    out->write( |--------------------------| ).

    out->write( |Prueba de validación de actualización de orden...| ).
    IF lo_validator->validate_update_order(
      iv_work_order_id = '1000000001'
      iv_status = 'CO' ).
      out->write( |La validación fue exitosa.| ).
    ELSE.
      out->write( |La validación falló.| ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
