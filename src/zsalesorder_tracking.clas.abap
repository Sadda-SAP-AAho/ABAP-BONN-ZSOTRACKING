CLASS zsalesorder_tracking DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  INTERFACES if_rap_query_provider.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zsalesorder_tracking IMPLEMENTATION.

   METHOD if_rap_query_provider~select.

      DATA(lv_top)   =   io_request->get_paging( )->get_page_size( ).
      DATA(lv_skip)  =   io_request->get_paging( )->get_offset( ).
      DATA(lv_max_rows) = COND #( WHEN lv_top = if_rap_query_paging=>page_size_unlimited THEN 0 ELSE lv_top ).

*      DATA(lt_clause)  = io_request->get_filter( )->get_as_ranges( ).
      DATA(lt_parameters)  = io_request->get_parameters( ).
      DATA(lt_fileds)  = io_request->get_requested_elements( ).
      DATA(lt_sort)  = io_request->get_sort_elements( ).

      TRY.
          DATA(lt_Filter_cond) = io_request->get_filter( )->get_as_ranges( ).
        CATCH cx_rap_query_filter_no_range INTO DATA(lx_no_sel_option).
         CLEAR lt_Filter_cond.  " Ignore error and continue
      ENDTRY.

      LOOP AT lt_filter_cond INTO DATA(ls_filter_cond).
        IF ls_filter_cond-name = to_upper( 'Plant' ).
          DATA(lt_plant) = ls_filter_cond-range[].
        ELSEIF ls_filter_cond-name = to_upper( 'Customer' ).
          DATA(lt_customer) = ls_FILTER_cond-range[].
        ELSEIF ls_filter_cond-name = to_upper( 'CreationDate' ).
          DATA(lt_date) = ls_filter_cond-range[].
        ELSEIF ls_filter_cond-name = to_upper( 'SalesDocument' ).
          DATA(lt_salesorder) = ls_filter_cond-range[].
        ELSEIF ls_filter_cond-name = to_upper( 'DistributionChannel' ).
          DATA(lt_DistributionChannel) = ls_FILTER_cond-range[].
        ENDIF.
      ENDLOOP.


    DATA: lt_result TYPE STANDARD TABLE OF ZSALESRP_TRACKING,
          ls_line   TYPE ZSALESRP_TRACKING,
          lt_responseout LIKE lt_result,
          ls_responseout LIKE LINE OF lt_responseout.

    SELECT from I_SalesDocument as a
    JOIN I_SalesDocumentItem as b on a~SalesDocument = b~SalesDocument
    FIELDS a~SalesDocument, a~CreationDate, a~CreationTime, a~PurchaseOrderByCustomer, a~TotalNetAmount
    WHERE a~CreationDate IN @lt_date and b~BillToParty in @lt_customer
          and b~Plant in @lt_plant and b~DistributionChannel in @lt_distributionchannel and b~SalesDocument in @lt_salesorder
    GROUP BY a~SalesDocument, a~CreationDate, a~CreationTime, a~PurchaseOrderByCustomer, a~TotalNetAmount
    INTO TABLE @DATA(la_invoice).

    LOOP AT la_invoice INTO DATA(wa_invoice).
        CLEAR ls_line.
        ls_line-CreationDate            =           wa_invoice-CreationDate.
        ls_line-CreationTime            =           wa_invoice-CreationTime.
        ls_line-CustomerRef             =           wa_invoice-PurchaseOrderByCustomer.
        ls_line-OrderAmount             =           wa_invoice-TotalNetAmount.
        ls_line-SalesDocument           =           wa_invoice-SalesDocument.


*       Customer
        SELECT SINGLE FROM I_SalesDocumentItem as a
        JOIN I_Customer as b on b~Customer = a~BillToParty
        FIELDS b~Customer, b~CustomerName
        WHERE a~SalesDocument = @wa_invoice-SalesDocument
        INTO @DATA(Customer).


        ls_line-Customer                =           Customer-Customer.
        ls_line-CustomerName            =           Customer-CustomerName.



*       Order Quantity
        SELECT SINGLE FROM I_SalesDocumentItem as a
         FIELDS SUM( a~CommittedDelivQtyInOrdQtyUnit ) as OrderQty, SUM( a~ItemNetWeight ) as ItemNetWeight
        WHERE a~SalesDocument = @wa_invoice-SalesDocument
        INTO @DATA(OrderQty).

        ls_line-OrderQty                =           OrderQty-OrderQty.
        ls_line-OrderQtyPerKG           =           OrderQty-ItemNetWeight.

         IF ls_line-OrderQty NE 0.
            ls_line-RatePerKG               =           ls_line-OrderAmount / ls_line-OrderQty .
        ENDIF.


*       Credit Blocked
        SELECT SINGLE FROM I_CreditBlockedSalesDocument as a
        FIELDS SalesDocument
        WHERE a~SalesDocument = @wa_invoice-SalesDocument
        INTO @DATA(SalesDoc).

        IF SalesDoc IS INITIAL.
            ls_line-FinApp           =           'No'.
        ELSE.
            ls_line-FinApp           =           'Yes'.
        ENDIF.


        SELECT FROM I_SalesDocumentItem as a
        LEFT JOIN I_DeliveryDocumentItem as c on c~ReferenceSDDocument = a~SalesDocument
        LEFT JOIN I_BILlingDocumentITEM as b on b~ReferenceSDDocument = c~DeliveryDocument and b~ReferenceSDDocumentItem = c~DeliveryDocumentItem
        FIELDS a~DistributionChannel, a~Plant,  a~TransactionCurrency, a~ItemWeightUnit,  c~DeliveryDocument as OBD,b~BillingDocument as Bill
        WHERE a~SalesDocument = @wa_invoice-SalesDocument
        GROUP BY a~DistributionChannel, a~Plant, a~TransactionCurrency, a~ItemWeightUnit,  c~DeliveryDocument,b~BillingDocument
        INTO TABLE @DATA(la_items).


        LOOP AT la_items INTO DATA(wa_items).

            ls_line-DistributionChannel =           wa_items-DistributionChannel.
            ls_line-Plant               =           wa_items-Plant.
            ls_line-ODD                 =           wa_items-obd.
            ls_line-Invoice             =           wa_items-bill.
            ls_line-AmountUnit          =           wa_items-TransactionCurrency.
            ls_line-QtyUnit             =           wa_items-ItemWeightUnit.


            SELECT SINGLE FROM I_BillingDocumentItem as a
            FIELDS SUM( a~BillingQuantity )
            WHERE a~SalesDocument = @wa_invoice-SalesDocument
            INTO @DATA(BillQty).
            ls_line-InvoiceQty              =           BillQty.

            SELECT SINGLE FROM I_BillingDocument as a
            FIELDS SUM( a~TotalNetAmount )
            WHERE a~BillingDocument = @wa_items-bill
            INTO @DATA(InvoiceAmount).
            ls_line-InvoiceAmount              =           InvoiceAmount.



*           Eway Bill
            SELECT SINGLE FROM zr_zirntp as a
            FIELDS a~Ewaybillno
            WHERE a~Bookingno = @wa_items-bill
            INTO @DATA(Ewaybillno).


            IF Ewaybillno IS INITIAL.
                ls_line-Eway                =           'No'.
            ELSE.
                ls_line-Eway                =           'Yes'.
            ENDIF.


*            PGI Reference
            SELECT SINGLE FROM I_DeliveryDocumentItem as a
            FIELDS a~GoodsMovementStatus, sum( a~ActualDeliveryQuantity ) as ActualDeliveryQuantity
            WHERE a~DeliveryDocument = @wa_items-OBD
            GROUP BY a~GoodsMovementStatus
            INTO @DATA(GoodsMovementStatus).


            ls_line-ODDQty          =           GoodsMovementStatus-ActualDeliveryQuantity.
            IF GoodsMovementStatus-GoodsMovementStatus IS INITIAL.
                ls_line-PGI             =           'No'.
            ELSE.
                ls_line-PGI             =           'Yes'.
            ENDIF.



            ls_line-AmtDiff                 =           ls_line-InvoiceAmount - ls_line-OrderAmount.

            APPEND ls_line TO lt_result.
            CLEAR:
                    InvoiceAmount,
                    BillQty,
                    GoodsMovementStatus,
                    Ewaybillno.
        ENDLOOP.

    ENDLOOP.


    lv_max_rows = lv_skip + lv_top.
      IF lv_skip > 0.
        lv_skip = lv_skip + 1.
      ENDIF.

      CLEAR lt_responseout.
      LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<lfs_out_line_item>) FROM lv_skip TO lv_max_rows.
        ls_responseout = <lfs_out_line_item>.
        APPEND ls_responseout TO lt_responseout.
      ENDLOOP.

      io_response->set_total_number_of_records( lines( lt_responseout ) ).
      io_response->set_data( lt_responseout ).

  ENDMETHOD.




ENDCLASS.
