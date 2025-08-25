CLASS zcl_pf_delete DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

    METHODS: delete_work_order
      IMPORTING
        iv_work_order_id TYPE zed_work_id
      RETURNING
        VALUE(rv_success) TYPE abap_bool.

ENDCLASS.

CLASS zcl_pf_delete IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    DATA: lv_work_order_id TYPE zed_work_id,
          lv_success       TYPE abap_bool.

    " Orden a eliminar
    lv_work_order_id = '1000000001'.

    out->write( 'INICIANDO ELIMINACION DE ORDEN' ).
    out->write( |Orden ID: { lv_work_order_id }| ).

    " Intentar eliminar la orden
    lv_success = delete_work_order( lv_work_order_id ).

    " Mostrar resultado
    IF lv_success = abap_true.
      out->write( 'Orden eliminada con exito.' ).
    ELSE.
      out->write( 'ERROR: No se pudo eliminar la orden.' ).
      out->write( 'Razon: La orden no esta cancelada o tuvo modificaciones.' ).
    ENDIF.
  ENDMETHOD.

  METHOD delete_work_order.
    DATA: lv_current_status TYPE zed_status.
    DATA: lv_has_updates TYPE abap_bool.

    " 1. Verificar que la orden existe y obtener su estado
    SELECT SINGLE status
      FROM ztb_work_order
      WHERE work_order_id = @iv_work_order_id
      INTO @lv_current_status.

    IF sy-subrc <> 0.
      rv_success = abap_false. " Orden no existe
      RETURN.
    ENDIF.

    " 2. Validar que el estado sea EXCLUSIVAMENTE CANCELADA (CA)
    IF lv_current_status <> 'CA'.
      rv_success = abap_false.
      RETURN.
    ENDIF.

    " 3. Verificar que NO tenga updates en la bitácora
    SELECT COUNT(*)
      FROM ztb_w_order_hist
      WHERE work_order_id = @iv_work_order_id
        AND change_description LIKE '%Actualizado%'
      INTO @DATA(lv_update_count).

    IF lv_update_count > 0.
      rv_success = abap_false. " Que si tuvo updates, no se puede eliminar "
      RETURN.
    ENDIF.

    " 4. Verificar adicionalmente por otros tipos de modificaciones
    SELECT COUNT(*)
      FROM ztb_w_order_hist
      WHERE work_order_id = @iv_work_order_id
        AND ( change_description LIKE '%modif%' OR
              change_description LIKE '%camb%' OR
              change_description LIKE '%update%' OR
              change_description LIKE '%change%' )
      INTO @DATA(lv_mod_count).

    IF lv_mod_count > 0.
      rv_success = abap_false. " Si tuvo modificaciones
      RETURN.
    ENDIF.

    "Tes de Delete,  Si pasa todas las validaciones, elimina la orden
    DELETE FROM ztb_work_order WHERE work_order_id = @iv_work_order_id.

    IF sy-subrc = 0.
      COMMIT WORK.
      rv_success = abap_true.
    ELSE.
      ROLLBACK WORK.
      rv_success = abap_false.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
