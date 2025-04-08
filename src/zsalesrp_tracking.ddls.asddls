@ObjectModel.query.implementedBy: 'ABAP:ZSALESORDER_TRACKING'
@EndUserText.label: 'Sales Order Traking CDS'
@Metadata.allowExtensions: true
define custom entity ZSALESRP_TRACKING
{
  key Invoice                 : vbeln;
  SalesDocument               : vbeln_va;
  DistributionChannel         : vtweg;
  CreationDate                : erdat;
  CreationTime                : uzeit;
  Customer                    : kunnr;
  CustomerName                : name1_gp;
  @Semantics.quantity.unitOfMeasure: 'QtyUnit'
  OrderQty                    : meng15;
  @Semantics.amount.currencyCode: 'AmountUnit'
  OrderAmount                 : abap.curr(15,2); // WERTV8
  @Semantics.amount.currencyCode: 'AmountUnit'
  RatePerKG                   : abap.curr(15,2);
  QtyUnit                     : vrkme;
  AmountUnit                  : waerk;
  ODD                         : vgbel;
  @Semantics.quantity.unitOfMeasure: 'QtyUnit'
  ODDQty                      : abap.quan( 13, 3 );
//  ODDItem                     : vgpos;
  PGI                         : abap.char(3);
  Eway                        : abap.char(3);
  FinApp                      : abap.char(3);
  @Semantics.quantity.unitOfMeasure: 'QtyUnit'
  InvoiceQty                  : fkimg;
  @Semantics.amount.currencyCode: 'AmountUnit'
  InvoiceAmount               : abap.curr(15,2);
  CustomerRef                 : bstkd;
  @Semantics.amount.currencyCode: 'AmountUnit'
  AmtDiff                     : abap.curr(15,2);
  Plant                       : werks_d;
}
