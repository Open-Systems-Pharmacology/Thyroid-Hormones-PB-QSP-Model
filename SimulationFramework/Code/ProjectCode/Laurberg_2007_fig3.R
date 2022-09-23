Laurberg_2007_fig3 <- function(runConfiguration){
  runConfiguration$scenarioName <- "Laurberg_2007_fig3"
  runConfiguration$simulationTime <- 100
  runConfiguration$steadyStateTime <- 2000 * 60
  runConfiguration$paramSheets <- c("Global", "PI human", "Human GD")

  simulatedScenario <- runScenario(runConfiguration = runConfiguration)
  
  simulatedScenarios <- list()
  t3Vals <- list()
  t4Vals <- list()
  t3T4Ratios <- list()
  runConfiguration$simulationTime <- 2000 * 60
  setOutputInterval(simulation = simulatedScenario$simulation, startTime = 0, endTime = runConfiguration$simulationTime, resolution = 1)
  tshRA <- seq(1e-7, 1e-5, by = 5e-7)
  for (tshVal in tshRA){
    setParameterValuesByPath(parameterPaths = "T4_synthesis|TSH_Antibody",
                             values = tshVal,
                             simulation = simulatedScenario$simulation)
    simulationResults <- runSimulation(simulation = simulatedScenario$simulation)
    simulatedScenarios[[as.character(tshVal)]] <- getOutputValues(simulationResults, quantitiesOrPaths = getAllQuantitiesMatching(enumValues(OutputPaths), simulatedScenario$simulation))
    t3 <- simulatedScenarios[[as.character(tshVal)]]$data[[OutputPaths$T3_PVB]]
    t4 <- simulatedScenarios[[as.character(tshVal)]]$data[[OutputPaths$T4_PVB]]
    t3Vals <- c(t3Vals, t3[[length(t3)]])
    t4Vals <- c(t4Vals, t4[[length(t4)]])
    t3T4Ratios <- c(t3T4Ratios, t3[[length(t3)]] / t4[[length(t4)]] * 100)
  }

  t3Day0Mapping <- PlotMapping$new()
  t3Day0Mapping$addXYSeries(xValsList = unlist(t3Vals)*1e3,
                        yValsList = unlist(t3T4Ratios),
                        lables = "GD")
  t3Day0Mapping$xLim <- c(2.5, 15)
  t3Day0Mapping$yLim <- c(1.5, 4.5)
  t3Day0Mapping$xLab = "T3 day 0 [nmol/l]"
  t3Day0Mapping$yLab = "T3 % of T4 day 0 [%]"
  
  t4Day0Mapping <- PlotMapping$new()
  t4Day0Mapping$addXYSeries(xValsList = unlist(t4Vals)*1e3,
                            yValsList = unlist(t3T4Ratios),
                            lables = "GD")
  t4Day0Mapping$xLim <- c(150, 350)
  t4Day0Mapping$yLim <- c(1.5, 4.5)
  t4Day0Mapping$xLab = "T4 day 0 [nmol/l]"
  t4Day0Mapping$yLab = "T3 % of T4 day 0 [%]"
  
  plotConfiguration <- enum(list(
    outputToPNG = runConfiguration$outputToPNG,
    outputName = runConfiguration$scenarioName,
    outputPath = runConfiguration$outputPath,
    width = NULL,
    height = NULL,
    nrOfCols = NULL,
    res = 600,
    pointsize = 8,
    addTitle = TRUE
  ))
  
  plotMultiPanel(c(t3Day0Mapping, t4Day0Mapping), plotConfiguration)
  
  
  
  #############
  runConfiguration$scenarioName <- "Laurberg_2007"
  runConfiguration$simulationTime <- 4 * 24 * 60
  runConfiguration$steadyStateTime <- 2000 * 60
  runConfiguration$paramSheets <- c("Global", "PI human", "Human GD", "PTU_human", "MMI_human")
  
  simulatedScenario <- runScenario(runConfiguration = runConfiguration)
  
  simulatedScenarios_PTU <- list()
  t3Vals_PTU <- list()
  t4Vals_PTU <- list()
  t3T4Ratios_PTU <- list()
  for (tshVal in tshRA){
    setParameterValuesByPath(parameterPaths = "T4_synthesis|TSH_Antibody",
                             values = tshVal,
                             simulation = simulatedScenario$simulation)
      initialValues <- getSteadyState(simulation = simulatedScenario$simulation, 
                                      steadyStateTime = runConfiguration$steadyStateTime)
      setMoleculeInitialValues(molecules = initialValues$quantities, 
                               initialValues$values)
      
    simulationResults <- runSimulation(simulation = simulatedScenario$simulation)
    simulatedScenarios_PTU[[as.character(tshVal)]] <- getOutputValues(simulationResults, quantitiesOrPaths = getAllQuantitiesMatching(enumValues(OutputPaths), simulatedScenario$simulation))
    t3 <- simulatedScenarios_PTU[[as.character(tshVal)]]$data[[OutputPaths$T3_PVB]]
    t4 <- simulatedScenarios_PTU[[as.character(tshVal)]]$data[[OutputPaths$T4_PVB]]
    t3Vals_PTU <- c(t3Vals_PTU, t3[[length(t3)]])
    t4Vals_PTU <- c(t4Vals_PTU, t4[[length(t4)]])
    t3T4Ratios_PTU <- c(t3T4Ratios_PTU, t3[[length(t3)]] / t4[[length(t4)]] * 100)
  }
  
  t3Day0Mapping <- PlotMapping$new()
  t3Day0Mapping$addXYSeries(xValsList = unlist(t3Vals)*1e3,
                            yValsList = unlist(t3T4Ratios_PTU) / unlist(t3T4Ratios) * 100,
                            lables = "GD")
  t3Day0Mapping$xLim <- c(2.5, 15)
 # t3Day0Mapping$yLim <- c(1.5, 4.5)
  t3Day0Mapping$xLab = "T3 day 0 [nmol/l]"
  t3Day0Mapping$yLab = "T3/T4 d4/0 [%]"
  
  t4Day0Mapping <- PlotMapping$new()
  t4Day0Mapping$addXYSeries(xValsList = unlist(t4Vals)*1e3,
                            yValsList = unlist(t3T4Ratios),
                            lables = "GD")
  t4Day0Mapping$xLim <- c(150, 350)
  t4Day0Mapping$yLim <- c(1.5, 4.5)
  t4Day0Mapping$xLab = "T4 day 0 [nmol/l]"
  t4Day0Mapping$yLab = "T3 % of T4 day 0 [%]"
  
  plotConfiguration <- enum(list(
    outputToPNG = runConfiguration$outputToPNG,
    outputName = runConfiguration$scenarioName,
    outputPath = runConfiguration$outputPath,
    width = NULL,
    height = NULL,
    nrOfCols = NULL,
    res = 600,
    pointsize = 8,
    addTitle = TRUE
  ))
  
  plotMultiPanel(c(t3Day0Mapping, t4Day0Mapping), plotConfiguration)
  
}