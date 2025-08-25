CLASS zcl_pf_update DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

    METHODS: update_work_order_status
               IMPORTING
                 iv_work_order_id TYPE zed_work_id
                 iv_new_status    TYPE zed_status
                 iv_new_priority  TYPE zed_priority OPTIONAL
               RETURNING
                 VALUE(rv_success) TYPE abap_bool.

  PRIVATE SECTION.
    METHODS: get_next_history_id
               RETURNING
                 VALUE(rv_next_id) TYPE numc5.

    METHODS: register_history
               IMPORTING
                 iv_work_order_id TYPE zed_work_id
                 iv_description   TYPE c.
ENDCLASS.

CLASS zcl_pf_update IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
  "Test de Actualización".

    DATA: lv_success TYPE abap_bool.

    out->write( 'ACTUALIZACION DE ESTADO DE ORDEN' ).

    " Actualizar orden de PE a PR
    lv_success = update_work_order_status(
      iv_work_order_id = '1000000001'
      iv_new_status    = 'PR'
      iv_new_priority  = 'A'
    ).

    IF lv_success = abap_true.
      out->write( 'Orden actualizada exitosamente' ).
    ELSE.
      out->write( 'Error al actualizar orden' ).
    ENDIF.
  ENDMETHOD.

  METHOD update_work_order_status.
    DATA: lv_old_status TYPE zed_status,
          lv_old_priority TYPE zed_priority.

    " Primero leer el estado y prioridad actual
    SELECT SINGLE status, priority
      FROM ztb_work_order
      WHERE work_order_id = @iv_work_order_id
      INTO (@lv_old_status, @lv_old_priority).

    IF sy-subrc <> 0.
      rv_success = abap_false.
      RETURN.
    ENDIF.

    " Verificar si realmente hay cambios
    IF lv_old_status = iv_new_status AND
       ( NOT iv_new_priority IS SUPPLIED OR lv_old_priority = iv_new_priority ).
      rv_success = abap_true.
      RETURN.
    ENDIF.

    " Actualizar el registro
    IF iv_new_priority IS SUPPLIED.
      UPDATE ztb_work_order
        SET status   = @iv_new_status,
            priority = @iv_new_priority
        WHERE work_order_id = @iv_work_order_id.
    ELSE.
      UPDATE ztb_work_order
        SET status   = @iv_new_status
        WHERE work_order_id = @iv_work_order_id.
    ENDIF.

    IF sy-subrc = 0.
      " Registrar en bitácora el cambio
      IF iv_new_priority IS SUPPLIED.
        register_history(
          iv_work_order_id = iv_work_order_id
          iv_description   = |Actualizado: Status { lv_old_status }->{ iv_new_status }, Priority { lv_old_priority }->{ iv_new_priority }|
        ).
      ELSE.
        register_history(
          iv_work_order_id = iv_work_order_id
          iv_description   = |Actualizado: Status { lv_old_status }->{ iv_new_status }|
        ).
      ENDIF.

      COMMIT WORK.
      rv_success = abap_true.
    ELSE.
      ROLLBACK WORK.
      rv_success = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD get_next_history_id.
    DATA: lv_max_id TYPE numc5.

    " Obtener el máximo history_id de la tabla
    SELECT MAX( history_id )
      FROM ztb_w_order_hist
      INTO @lv_max_id.

    IF sy-subrc = 0 AND lv_max_id IS NOT INITIAL.
      rv_next_id = lv_max_id + 1.
    ELSE.
      rv_next_id = 1.
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
  ENDMETHOD.

ENDCLASS.
