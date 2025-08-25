CLASS zcl_pf_lectura DEFINITION

  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

ENDCLASS.



CLASS ZCL_PF_LECTURA IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.


  DATA:
  lt_work_orders     TYPE TABLE OF ztb_work_order WITH EMPTY KEY,
  ls_work_order      TYPE ztB_work_order.
  select * from ztB_work_order into TABLE @lt_work_orders.


READ TABLE lt_work_orders INTO ls_work_order WITH KEY work_order_id = 1000000001.
IF sy-subrc = 0.
  out->write( |Orden encontrada: { ls_work_order-work_order_id }| ).
  else.
  out->write( |No se encontró la orden.| ).
ENDIF.



  ENDMETHOD.
ENDCLASS.
