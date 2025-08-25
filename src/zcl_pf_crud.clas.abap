CLASS zcl_pf_crud DEFINITION
  PUBLIC
  FINAL.

  PUBLIC SECTION.
    " Constructor para inicializar el validador
    METHODS constructor.

    " Métodos CRUD
    METHODS:
      create_work_order
        IMPORTING
          is_order TYPE ztb_work_order
        RETURNING VALUE(rv_success) TYPE abap_bool,

      read_work_order
        IMPORTING
          iv_work_order_id TYPE ztb_work_order-work_order_id
        RETURNING VALUE(rs_order) TYPE ztb_work_order,

      update_work_order
        IMPORTING
          is_order TYPE ztb_work_order
        RETURNING VALUE(rv_success) TYPE abap_bool,

      delete_work_order
        IMPORTING
          iv_work_order_id TYPE ztb_work_order-work_order_id
          iv_status        TYPE ztb_work_order-status
        RETURNING VALUE(rv_success) TYPE abap_bool.

  PRIVATE SECTION.
    " Atributo para instanciar la clase de validación
    DATA lo_validator TYPE REF TO zcl_pf_validacines.

    " Método para generar un ID único para la orden de trabajo
    METHODS generate_unique_id
      RETURNING VALUE(rv_work_order_id) TYPE ztb_work_order-work_order_id.

ENDCLASS.


CLASS zcl_pf_crud IMPLEMENTATION.


  " Constructor para crear el objeto validador una sola vez"
  METHOD constructor.
    CREATE OBJECT lo_validator.
  ENDMETHOD.


  " Crear orden"
  METHOD create_work_order.
    DATA ls_new_order TYPE ztb_work_order.

    " Validar la creación de la orden
    DATA(lv_success) = lo_validator->validate_create_order(
                   iv_customer_id   = is_order-customer_id
                   iv_technician_id = is_order-technician_id
                   iv_priority      = is_order-priority ).

    IF lv_success = abap_true.
      " Generar un ID único antes de insertar
      DATA(lv_new_id) = me->generate_unique_id( ).

      " Crear una copia de los datos de entrada para poder modificar el ID
      ls_new_order = is_order.
      ls_new_order-work_order_id = lv_new_id.

      " Insertar en tabla de base de datos
      INSERT ztb_work_order FROM @ls_new_order.
      IF sy-subrc <> 0.
        rv_success = abap_false.
        RETURN.
      ENDIF.

      " Devolver el ID de la orden creada
      rv_success = abap_true.
    ELSE.
      rv_success = abap_false.
    ENDIF.
  ENDMETHOD.


  " Leer orden"
  METHOD read_work_order.
    SELECT SINGLE * FROM ztb_work_order

      WHERE work_order_id = @iv_work_order_id
      INTO @rs_order.
  ENDMETHOD.


  " Actualizar orden"
  METHOD update_work_order.
    " Validar el estado de la orden antes de actualizar
    rv_success = lo_validator->validate_update_order(
                   iv_work_order_id = is_order-work_order_id
                   iv_status        = is_order-status ).

    IF rv_success = abap_true.
      UPDATE ztb_work_order FROM @is_order.
      IF sy-subrc <> 0.
        rv_success = abap_false.
      ENDIF.
    ENDIF.
  ENDMETHOD.


METHOD delete_work_order.
    " Inicializar éxito como falso
    rv_success = abap_false.

    " Verificar que el estado NO sea PR (Procesando) ni PE (Pendiente)
    IF iv_status = 'PR' OR iv_status = 'PE'.
        " No permitir eliminación si el estado es PR o PE
        RETURN.
    ENDIF.

    " Validación adicional con el validador
    rv_success = lo_validator->validate_delete_order(
                   iv_work_order_id = iv_work_order_id
                   iv_status        = iv_status ).

    IF rv_success = abap_true.
        " Eliminar la orden de la tabla principal
        DELETE FROM ztb_work_order WHERE work_order_id = @iv_work_order_id.

        IF sy-subrc = 0.
            rv_success = abap_true.
            " Opcional: Eliminar registros relacionados en historial
            DELETE FROM ztb_w_order_hist WHERE work_order_id = @iv_work_order_id.
        ELSE.
            rv_success = abap_false.
        ENDIF.
    ENDIF.
ENDMETHOD.


  " Método para generar un ID único para la orden de trabajo"
  METHOD generate_unique_id.
    " En un entorno productivo, aquí se usaría una secuencia o un generador de IDs
    DATA: lv_max_id TYPE ztb_work_order-work_order_id.

    SELECT MAX( work_order_id )  FROM ztb_work_order INTO @lv_max_id.

    IF sy-subrc <> 0 OR lv_max_id IS INITIAL.
      rv_work_order_id = '1000000001'.
    ELSE.
      rv_work_order_id = lv_max_id + 1.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


