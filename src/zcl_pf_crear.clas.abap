CLASS zcl_pf_crear DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

    TYPES: BEGIN OF ztb_work_order,
             client          TYPE mandt,
             work_order_id   TYPE zed_work_id,
             customer_id     TYPE zed_customer_id,
             technician_id   TYPE zed_technician_id,
             creation_date   TYPE d,
             status          TYPE zed_status,
             priority        TYPE zed_priority,
             description     TYPE c LENGTH 50,
           END OF ztb_work_order.

    TYPES: BEGIN OF ztb_w_order_hist,
             client              TYPE mandt,
             work_order_id       TYPE zed_work_id,
             history_id          TYPE numc5,
             modification_date   TYPE dats,
             change_description  TYPE c LENGTH 50,
           END OF ztb_w_order_hist.

    TYPES: ts_work_order TYPE ztb_work_order.

    CONSTANTS: BEGIN OF gc_status,
                 pending    TYPE zed_status VALUE 'PE',
                 in_progress TYPE zed_status VALUE 'PR',
               END OF gc_status.

    METHODS: create_work_order
               IMPORTING
                 iv_customer_id   TYPE zed_customer_id
                 iv_technician_id TYPE zed_technician_id
                 iv_priority      TYPE zed_priority
                 iv_description   TYPE c
               RETURNING
                 VALUE(rs_work_order) TYPE ts_work_order.

  PRIVATE SECTION.
    METHODS: get_next_work_order_id
               RETURNING
                 VALUE(rv_next_id) TYPE zed_work_id.

    METHODS: get_next_history_id
               RETURNING
                 VALUE(rv_next_id) TYPE numc5.

    METHODS: register_history
               IMPORTING
                 iv_work_order_id TYPE zed_work_id
                 iv_description   TYPE c.
ENDCLASS.

CLASS zcl_pf_crear IMPLEMENTATION.

"Test Creacion nde Orden en la Tabla"

  METHOD if_oo_adt_classrun~main.
    DATA: ls_order TYPE ts_work_order.

    out->write( 'CREACION DE ORDENES EN TABLA ZTB_WORK_ORDER' ).

    " Crear orden"
    ls_order = create_work_order(
      iv_customer_id   = 'CU000001'
      iv_technician_id = 'TC000001'
      iv_priority      = 'A'
      iv_description   = 'Orden de reparacion urgente'
    ).

    " Muestra el  resultado
    IF ls_order-work_order_id IS NOT INITIAL.
      out->write( |Orden { ls_order-work_order_id } insertada exitosamente| ).

      " Verifica si se creó el registro en bitácora
      SELECT COUNT(*)
        FROM ztb_w_order_hist
        WHERE work_order_id = @ls_order-work_order_id
        INTO @DATA(lv_hist_count).

      IF lv_hist_count > 0.
        out->write( |Registro en bitacora: SI ({ lv_hist_count } registros)| ).

        " Mostrar los history_id reales
        SELECT history_id, work_order_id, change_description
          FROM ztb_w_order_hist
          WHERE work_order_id = @ls_order-work_order_id
          INTO TABLE @DATA(lt_historial).

        LOOP AT lt_historial INTO DATA(ls_hist).
          out->write( |History ID: { ls_hist-history_id }, Orden: { ls_hist-work_order_id }, Desc: { ls_hist-change_description }| ).
        ENDLOOP.
      ELSE.
        out->write( 'Registro en bitacora: NO' ).
      ENDIF.

    ELSE.
      out->write( |Error al insertar. Codigo: { sy-subrc }| ).
    ENDIF.

    " Mostrar total actual de órdenes
    SELECT COUNT(*)
      FROM ztb_work_order
      INTO @DATA(lv_count).
    out->write( |Total de ordenes: { lv_count }| ).
  ENDMETHOD.

  METHOD create_work_order.
    " Obtener próximo ID
    rs_work_order-work_order_id = get_next_work_order_id( ).

    IF rs_work_order-work_order_id IS INITIAL.
      RETURN.
    ENDIF.

    " Llenar TODOS los campos incluyendo CLIENT
    rs_work_order-client          = sy-mandt.
    rs_work_order-customer_id     = iv_customer_id.
    rs_work_order-technician_id   = iv_technician_id.
    rs_work_order-priority        = iv_priority.
    rs_work_order-description     = iv_description.
    rs_work_order-creation_date   = cl_abap_context_info=>get_system_date( ).
    rs_work_order-status          = gc_status-pending.

    " Insertar en tabla principal
    INSERT INTO ztb_work_order VALUES @rs_work_order.

    IF sy-subrc = 0.
      " Registrar en bitácora
      register_history(
        iv_work_order_id = rs_work_order-work_order_id
        iv_description   = 'Creacion de orden de trabajo'
      ).

      COMMIT WORK.
    ELSE.
      CLEAR rs_work_order-work_order_id.
    ENDIF.
  ENDMETHOD.

  METHOD get_next_work_order_id.
    DATA: lv_max_id TYPE zed_work_id.

    " Obtener máximo ID existente
    SELECT MAX( work_order_id )
      FROM ztb_work_order
      INTO @lv_max_id.

    IF sy-subrc = 0 AND lv_max_id IS NOT INITIAL.
      rv_next_id = lv_max_id + 1.
    ELSE.
      rv_next_id = 1000000001.
    ENDIF.
  ENDMETHOD.

  METHOD get_next_history_id.
    DATA: lv_max_id TYPE numc5.

    " Obtener máximo history_id de TODA la tabla
    SELECT MAX( history_id )
      FROM ztb_w_order_hist
      INTO @lv_max_id.

    IF sy-subrc = 0 AND lv_max_id IS NOT INITIAL.
      rv_next_id = lv_max_id + 1.
    ELSE.
      rv_next_id = 1. " Primer registro de la tabla
    ENDIF.
  ENDMETHOD.

  METHOD register_history.
    DATA: lv_history_id TYPE numc5.

    " Obtener próximo ID de historial
    lv_history_id = get_next_history_id( ).

    " Insertar en bitácora
    INSERT ztb_w_order_hist FROM @( VALUE #(
       client             = sy-mandt
       work_order_id      = iv_work_order_id
       history_id         = lv_history_id
       modification_date  = cl_abap_context_info=>get_system_date( )
       change_description = iv_description
     ) ).

    IF sy-subrc <> 0.

    ENDIF.
  ENDMETHOD.

ENDCLASS.
