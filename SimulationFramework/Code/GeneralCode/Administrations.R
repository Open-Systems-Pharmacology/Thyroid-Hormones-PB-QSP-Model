#' Path to the 'ProtocolSchemaItem'-level of the administrations implemented in the model
#'
#' @include enum.R
#' @export
Administrations <- enum(list(
  T4_po_dissolved = "Applications|po_T4|Dissolved|Application_1",
  T3_po_dissolved = "Applications|po_T3|Dissolved|Application_1",
  T3_iv_bolus = "Applications|T3 i.v. Pilo 1990|Application_1",
  T4_iv_bolus = "Applications|T4 i.v. Pilo 1990|Application_1",
  PTU_iv_bolus = "Applications|iv 1.05 mgkg|Application_1",
  MMI_Cooper_7days = "Applications|Cooper_MMI_7days|Application_1",
  PTU_Cooper_7days = "Applications|Cooper_PTU_7days|Application_1",
  perpetrator_iv = "Applications|perpetrator iv|Application_1",
  wong2005 = "Applications|PTU 200mg/kg/day",
  Reeth_1987 = "Applications|Reeth_1987_MMI",
  Andrare_MMI = "Applications|MMI 30mg 1xDaily",
  Laurberg_2007 = "Applications|PTU 400mg 4xDaily",
  Laurberg_2007_MMI = "Applications|10mg 4xDaily",
  PB_3weeks = "Applications|Michael_PB_3weeks",
  PTU_tablet = "Applications|PTU tablet",
  PTU_oral = "Applications|PTU_oral",
  Kato_2010_PB_iv_80mgkg_4d = "Applications|Kato_2010_PB_iv_80mgkg_4d",
  PB_100mg_po_14days = "Applications|PB 100mg po 14days",
  Wong_PB_5days = "Applications|Wong_PB_5days",
  PB_iv_100mgkg_7d = "Applications|PB_iv_100mgkg_7d"
))

