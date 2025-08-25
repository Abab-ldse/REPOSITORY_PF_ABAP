CLASS zzzzprueba DEFINITION
  PUBLIC CREATE PUBLIC FINAL
  INHERITING FROM cl_demo_classrun.

  PUBLIC SECTION.
    METHODS main REDEFINITION.

ENDCLASS.


CLASS zzzzprueba IMPLEMENTATION.
  METHOD main.

    DELETE  from ztb_work_order.
    delete from ztb_w_order_hist.
  ENDMETHOD.
ENDCLASS.

