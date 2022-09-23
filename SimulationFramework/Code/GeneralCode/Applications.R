disableApplications <- function(simulation) {
  setParameterValuesByPath(parameterPaths = paste(enumValues(Administrations), "Active", sep = "|"), values = rep(0, len = length(enumValues(Administrations))), simulation = simulation)
}

setApplications <- function(simulation, scenario, dose = NULL) {
  pbPLivFactor <- 2.635858
  
  if (scenario == "Eisenberg2008Fig45_400ug") {
    administrationParam <- getParameter(path = paste(Administrations$T4_po_dissolved, "ProtocolSchemaItem", "Dose", sep = "|"), container = simulation)
    activeParam <- getParameter(path = paste(Administrations$T4_po_dissolved, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    setParameterValues(
      parameters = administrationParam,
      values = toBaseUnit(quantity = administrationParam, values = 0.4, unit = "mg")
    )
    setParameterValues(
      parameters = getParameter(path = paste(Administrations$T4_po_dissolved, "ProtocolSchemaItem", "Start time", sep = "|"), container = simulation),
      values = 264 * 60
    )
  }
  
  if (scenario == "Eisenberg2008Fig45_600ug") {
    administrationParam <- getParameter(path = paste(Administrations$T4_po_dissolved, "ProtocolSchemaItem", "Dose", sep = "|"), container = simulation)
    activeParam <- getParameter(path = paste(Administrations$T4_po_dissolved, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    setParameterValues(
      parameters = administrationParam,
      values = toBaseUnit(quantity = administrationParam, values = 0.6, unit = "mg")
    )
    setParameterValues(
      parameters = getParameter(path = paste(Administrations$T4_po_dissolved, "ProtocolSchemaItem", "Start time", sep = "|"), container = simulation),
      values = 264 * 60
    )
  }
  
  if (scenario == "Pilo1990Fig5_T3") {
    administrationParam <- getParameter(path = paste(Administrations$T3_iv_bolus, "ProtocolSchemaItem", "Dose", sep = "|"), container = simulation)
    activeParam <- getParameter(path = paste(Administrations$T3_iv_bolus, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    setParameterValues(
      parameters = administrationParam,
      values = toBaseUnit(quantity = administrationParam, values = 8.6e-5, unit = "mg")
    )
    setParameterValues(
      parameters = getParameter(path = paste(Administrations$T3_iv_bolus, "ProtocolSchemaItem", "Start time", sep = "|"), container = simulation),
      values = 0
    )
  }
  
  if (scenario == "Pilo1990Fig5_T4") {
    administrationParam <- getParameter(path = paste(Administrations$T4_iv_bolus, "ProtocolSchemaItem", "Dose", sep = "|"), container = simulation)
    activeParam <- getParameter(path = paste(Administrations$T4_iv_bolus, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    setParameterValues(
      parameters = administrationParam,
      values = toBaseUnit(quantity = administrationParam, values = 0.00017, unit = "mg")
    )
    setParameterValues(
      parameters = getParameter(path = paste(Administrations$T4_iv_bolus, "ProtocolSchemaItem", "Start time", sep = "|"), container = simulation),
      values = 0
    )
  }
  
  if (scenario == "DiStefano_1982_T4_iv") {
    administrationParam <- getParameter(path = paste(Administrations$T4_iv_bolus, "ProtocolSchemaItem", "Dose", sep = "|"), container = simulation)
    activeParam <- getParameter(path = paste(Administrations$T4_iv_bolus, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    setParameterValues(
      parameters = administrationParam,
      values = toBaseUnit(quantity = administrationParam, values = 2e-6, unit = "mg")
    )
    setParameterValues(
      parameters = getParameter(path = paste(Administrations$T4_iv_bolus, "ProtocolSchemaItem", "Start time", sep = "|"), container = simulation),
      values = 0
    )
  }
  
  if (scenario == "Silva_1997_T3_iv") {
    bwParam <- getParameter("Organism|Weight", simulation)
    administrationParam <- getParameter(path = paste(Administrations$T3_iv_bolus, "ProtocolSchemaItem", "Dose", sep = "|"), container = simulation)
    activeParam <- getParameter(path = paste(Administrations$T3_iv_bolus, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    setParameterValues(parameters = administrationParam, values = toBaseUnit(quantity = administrationParam, values = 0.0007 * bwParam$value, unit = "mg"))
    setParameterValues(
      parameters = getParameter(path = paste(Administrations$T3_iv_bolus, "ProtocolSchemaItem", "Start time", sep = "|"), container = simulation),
      values = 0
    )
  }

  if (scenario == "Eisenberg2008Fig9") {
    administrationParam <- getParameter(path = paste(Administrations$T3_po_dissolved, "ProtocolSchemaItem", "Dose", sep = "|"), container = simulation)
    activeParam <- getParameter(path = paste(Administrations$T3_po_dissolved, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    setParameterValues(parameters = administrationParam, values = toBaseUnit(quantity = administrationParam, values = 75, unit = "µg"))
    setParameterValues(
      parameters = getParameter(path = paste(Administrations$T3_po_dissolved, "ProtocolSchemaItem", "Start time", sep = "|"), container = simulation),
      values = 240 * 60
    )
  }
  
  if (scenario == "Larsen_1997_T3_iv") {
    bwParam <- getParameter("Organism|Weight", simulation)
    administrationParam <- getParameter(path = paste(Administrations$T3_iv_bolus, "ProtocolSchemaItem", "Dose", sep = "|"), container = simulation)
    activeParam <- getParameter(path = paste(Administrations$T3_iv_bolus, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    setParameterValues(parameters = administrationParam, values = toBaseUnit(quantity = administrationParam, values = 700 * bwParam$value, unit = "ng"))
    setParameterValues(
      parameters = getParameter(path = paste(Administrations$T3_iv_bolus, "ProtocolSchemaItem", "Start time", sep = "|"), container = simulation),
      values = 0
    )
  }
  
  if (scenario == "Larsen_1997_T4_iv") {
    bwParam <- getParameter("Organism|Weight", simulation)
    administrationParam <- getParameter(path = paste(Administrations$T4_iv_bolus, "ProtocolSchemaItem", "Dose", sep = "|"), container = simulation)
    activeParam <- getParameter(path = paste(Administrations$T4_iv_bolus, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    setParameterValues(parameters = administrationParam, values = toBaseUnit(quantity = administrationParam, values = 8000 * bwParam$value, unit = "ng"))
    setParameterValues(
      parameters = getParameter(path = paste(Administrations$T4_iv_bolus, "ProtocolSchemaItem", "Start time", sep = "|"), container = simulation),
      values = 0
    )
  }
  
  if (scenario == "Cooper_1983_MMI") {
    administrationParam <- getParameter(path = paste(Administrations$MMI_Cooper_7days, "ProtocolSchemaItem", "Dose", sep = "|"), container = simulation)
    activeParam <- getParameter(path = paste(Administrations$MMI_Cooper_7days, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    setParameterValues(parameters = administrationParam, values = toBaseUnit(quantity = administrationParam, values = dose, unit = "g"))
    setParameterValues(
      parameters = getParameter(path = paste(Administrations$MMI_Cooper_7days, "ProtocolSchemaItem", "Start time", sep = "|"), container = simulation),
      values = 2*24*60
    )
  }
  
  if (scenario == "Cooper_1983_PTU") {
    administrationParam <- getParameter(path = paste(Administrations$PTU_Cooper_7days, "ProtocolSchemaItem", "Dose", sep = "|"), container = simulation)
    activeParam <- getParameter(path = paste(Administrations$PTU_Cooper_7days, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    setParameterValues(parameters = administrationParam, values = toBaseUnit(quantity = administrationParam, values = dose, unit = "g"))
    setParameterValues(
      parameters = getParameter(path = paste(Administrations$PTU_Cooper_7days, "ProtocolSchemaItem", "Start time", sep = "|"), container = simulation),
      values = 2*24*60
    )
  }
  
  if (scenario == "Wong_2005_PTU") {
    activeParam <- getParameter(path = paste(Administrations$wong2005, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
  }
  
  if (scenario == "Reeth_1987") {
    activeParam <- getParameter(path = paste(Administrations$Reeth_1987, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
  }
  
  if (scenario == "Andrade_1999") {
    activeParam <- getParameter(path = paste(Administrations$Andrare_MMI, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    
    for (i in 25 : 50) {
      param <- getParameter(path = paste0(Administrations$Andrare_MMI, "|Dissolved|Application_", i, "|ProtocolSchemaItem|Dose"), container = simulation)
      setParameterValues(param, 0)
    }
  }
  
  if (scenario == "Laurberg_2007") {
    activeParam <- getParameter(path = paste(Administrations$Laurberg_2007, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    activeParam <- getParameter(path = paste(Administrations$Laurberg_2007_MMI, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
  }
  
  if (scenario == "Michael_PB_3weeks") {
    setParameterValuesByPath(parameterPaths = "T4|Fraction unbound (plasma)", values = 0.00018, simulation = simulation)
    k_livPls <- getParameter("Neighborhoods|Pericentral_int_Pericentral_cell|T4|Partition coefficient (intracellular/plasma)", container = simulation)
    k_livPls$reset()
    refVal <- k_livPls$value
    setParameterValues(k_livPls, values = refVal * pbPLivFactor)
    setParameterValues(getParameter("Neighborhoods|Periportal_int_Periportal_cell|T4|Partition coefficient (intracellular/plasma)", container = simulation),
                       values = refVal * pbPLivFactor)
    
    activeParam <- getParameter(path = paste(Administrations$PB_3weeks, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    activeParam <- getParameter(path = paste(Administrations$T4_iv_bolus, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    activeParam <- getParameter(path = paste(Administrations$T4_iv_bolus, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), container = simulation)
    setParameterValues(
      parameters = activeParam,
      values = toBaseUnit(quantity = activeParam, values = 1e-3, unit = "mg/kg")
    )
    setParameterValues(
      parameters = getParameter(path = paste(Administrations$T4_iv_bolus, "ProtocolSchemaItem", "Start time", sep = "|"), container = simulation),
      values = (3*7*24*60)
    )
  }
  
  if (scenario == "Michael_Placebo") {
    setParameterValuesByPath(parameterPaths = "T4|Fraction unbound (plasma)", values = 0.00018, simulation = simulation)

    activeParam <- getParameter(path = paste(Administrations$T4_iv_bolus, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    activeParam <- getParameter(path = paste(Administrations$T4_iv_bolus, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), container = simulation)
    setParameterValues(
      parameters = activeParam,
      values = toBaseUnit(quantity = activeParam, values = 1e-3, unit = "mg/kg")
    )
    setParameterValues(
      parameters = getParameter(path = paste(Administrations$T4_iv_bolus, "ProtocolSchemaItem", "Start time", sep = "|"), container = simulation),
      values = (3*7*24*60)
    )
  }
  
  if (scenario == "Kato_2010_Placebo") {
    setParameterValuesByPath(parameterPaths = "T4|Fraction unbound (plasma)", values = 0.000233, simulation = simulation)
    
    activeParam <- getParameter(path = paste(Administrations$T4_iv_bolus, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    activeParam <- getParameter(path = paste(Administrations$T4_iv_bolus, "ProtocolSchemaItem", "Dose", sep = "|"), container = simulation)
    setParameterValues(
      parameters = activeParam,
      values = toBaseUnit(quantity = activeParam, values = 0.0001, unit = "mg")
    )
    setParameterValues(
      parameters = getParameter(path = paste(Administrations$T4_iv_bolus, "ProtocolSchemaItem", "Start time", sep = "|"), container = simulation),
      values = (96*60)
    )
  }
  
  if (scenario == "Kato_2010_PB") {
    setParameterValuesByPath(parameterPaths = "T4|Fraction unbound (plasma)", values = 0.000233, simulation = simulation)
    k_livPls <- getParameter("Neighborhoods|Pericentral_int_Pericentral_cell|T4|Partition coefficient (intracellular/plasma)", container = simulation)
    k_livPls$reset()
    refVal <- k_livPls$value
    setParameterValues(k_livPls, values = refVal * pbPLivFactor)
    setParameterValues(getParameter("Neighborhoods|Periportal_int_Periportal_cell|T4|Partition coefficient (intracellular/plasma)", container = simulation),
                       values = refVal * pbPLivFactor)
    
    activeParam <- getParameter(path = paste(Administrations$Kato_2010_PB_iv_80mgkg_4d, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    activeParam <- getParameter(path = paste(Administrations$T4_iv_bolus, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    activeParam <- getParameter(path = paste(Administrations$T4_iv_bolus, "ProtocolSchemaItem", "Dose", sep = "|"), container = simulation)
    setParameterValues(
      parameters = activeParam,
      values = toBaseUnit(quantity = activeParam, values = 0.0001, unit = "mg")
    )
    setParameterValues(
      parameters = getParameter(path = paste(Administrations$T4_iv_bolus, "ProtocolSchemaItem", "Start time", sep = "|"), container = simulation),
      values = (96*60)
    )
  }
  
  if (scenario == "Wong_2005_T4iv_Placebo") {
    activeParam <- getParameter(path = paste(Administrations$T4_iv_bolus, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    activeParam <- getParameter(path = paste(Administrations$T4_iv_bolus, "ProtocolSchemaItem", "Dose", sep = "|"), container = simulation)
    setParameterValues(
      parameters = activeParam,
      values = toBaseUnit(quantity = activeParam, values = 0.1, unit = "µg")
    )
    setParameterValues(
      parameters = getParameter(path = paste(Administrations$T4_iv_bolus, "ProtocolSchemaItem", "Start time", sep = "|"), container = simulation),
      values = (7440)
    )
  }
  
  if (scenario == "Wong_2005_T4iv_PB") {
    k_livPls <- getParameter("Neighborhoods|Pericentral_int_Pericentral_cell|T4|Partition coefficient (intracellular/plasma)", container = simulation)
    k_livPls$reset()
    refVal <- k_livPls$value
    setParameterValues(k_livPls, values = refVal * pbPLivFactor)
    setParameterValues(getParameter("Neighborhoods|Periportal_int_Periportal_cell|T4|Partition coefficient (intracellular/plasma)", container = simulation),
                       values = refVal * pbPLivFactor)
    
    activeParam <- getParameter(path = paste(Administrations$Wong_PB_5days, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    activeParam <- getParameter(path = paste(Administrations$T4_iv_bolus, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    activeParam <- getParameter(path = paste(Administrations$T4_iv_bolus, "ProtocolSchemaItem", "Dose", sep = "|"), container = simulation)
    setParameterValues(
      parameters = activeParam,
      values = toBaseUnit(quantity = activeParam, values = 0.1, unit = "µg")
    )
    setParameterValues(
      parameters = getParameter(path = paste(Administrations$T4_iv_bolus, "ProtocolSchemaItem", "Start time", sep = "|"), container = simulation),
      values = (7440)
    )
  }
  
  if (scenario == "Michael_PB 2weeks") {
    k_livPls <- getParameter("Neighborhoods|Pericentral_int_Pericentral_cell|T4|Partition coefficient (intracellular/plasma)", container = simulation)
    k_livPls$reset()
    refVal <- k_livPls$value
    setParameterValues(k_livPls, values = refVal * pbPLivFactor)
    setParameterValues(getParameter("Neighborhoods|Periportal_int_Periportal_cell|T4|Partition coefficient (intracellular/plasma)", container = simulation),
                       values = refVal * pbPLivFactor)
    
    activeParam <- getParameter(path = paste(Administrations$PB_3weeks, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
  }
  
  if (scenario == "Oppenheimer_1968_Placebo") {
    activeParam <- getParameter(path = paste(Administrations$T4_iv_bolus, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    activeParam <- getParameter(path = paste(Administrations$T4_iv_bolus, "ProtocolSchemaItem", "Dose", sep = "|"), container = simulation)
    setParameterValues(
      parameters = activeParam,
      values = toBaseUnit(quantity = activeParam, values = 0.04, unit = "µg")
    )
    setParameterValues(
      parameters = getParameter(path = paste(Administrations$T4_iv_bolus, "ProtocolSchemaItem", "Start time", sep = "|"), container = simulation),
      values = (5*24*60)
    )
  }
  
  if (scenario == "Oppenheimer_1968_PB") {
    k_livPls <- getParameter("Neighborhoods|Pericentral_int_Pericentral_cell|T4|Partition coefficient (intracellular/plasma)", container = simulation)
    k_livPls$reset()
    refVal <- k_livPls$value
    setParameterValues(k_livPls, values = refVal * pbPLivFactor)
    setParameterValues(getParameter("Neighborhoods|Periportal_int_Periportal_cell|T4|Partition coefficient (intracellular/plasma)", container = simulation),
                       values = refVal * pbPLivFactor)
    
    activeParam <- getParameter(path = paste(Administrations$PB_iv_100mgkg_7d, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    
    activeParam <- getParameter(path = paste(Administrations$T4_iv_bolus, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    activeParam <- getParameter(path = paste(Administrations$T4_iv_bolus, "ProtocolSchemaItem", "Dose", sep = "|"), container = simulation)
    setParameterValues(
      parameters = activeParam,
      values = toBaseUnit(quantity = activeParam, values = 0.04, unit = "µg")
    )
    setParameterValues(
      parameters = getParameter(path = paste(Administrations$T4_iv_bolus, "ProtocolSchemaItem", "Start time", sep = "|"), container = simulation),
      values = (5*24*60)
    )
  }
  
  if (scenario == "Human_PB_2Weeks") {
    k_livPls <- getParameter("Neighborhoods|Pericentral_int_Pericentral_cell|T4|Partition coefficient (intracellular/plasma)", container = simulation)
    k_livPls$reset()
    refVal <- k_livPls$value
    setParameterValues(k_livPls, values = refVal * pbPLivFactor)
    setParameterValues(getParameter("Neighborhoods|Periportal_int_Periportal_cell|T4|Partition coefficient (intracellular/plasma)", container = simulation),
                       values = refVal * pbPLivFactor)
    
    activeParam <- getParameter(path = paste(Administrations$PB_100mg_po_14days, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
  }
  
  if (scenario == "Skellern_MMI_rat") {
    administrationParam <- getParameter(path = paste(Administrations$perpetrator_iv, "ProtocolSchemaItem", "DosePerBodyWeight", sep = "|"), container = simulation)
    activeParam <- getParameter(path = paste(Administrations$perpetrator_iv, "Active", sep = "|"), container = simulation)
    setParameterValues(activeParam, 1)
    setParameterValues(parameters = administrationParam, values = toBaseUnit(quantity = administrationParam, values = 3.3, unit = "mg/kg"))
  }
}
