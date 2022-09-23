MMI_PK_rat <- function(runConfiguration){
  runConfiguration$scenarioName <- "Skellern_MMI_rat"
  runConfiguration$simulationTime <- 120 * 60
  runConfiguration$steadyStateTime <- 2000 * 60
  runConfiguration$simulateSteadyState <- FALSE
  runConfiguration$paramSheets <- c("Global", "Physiology_rat", "PI rat", "MMI_rat")
  runConfiguration$individualCharacteristics <- NULL
  runConfiguration$dataSheets <- c("Skellern_1973",
                                   "Cooper_1984b")
  
  simulatedScenario_Skellern <- runScenario(runConfiguration = runConfiguration)
  runConfiguration$scenarioName <- "Cooper_1984b_PD"
  simulatedScenario_Cooper <- runScenario(runConfiguration = runConfiguration, dose = 0.087696)
    
  observedData <- readObservedData(runConfiguration)
  
  #Factor for conversion of standard output units
  mwT4 <- 776.8
  convFacT4 <- 1e-6 * mwT4 * 1e6
  mwT3 <- 650.98
  convFacT3 <- 1e-6 * mwT3 * 1e6
  mwTSH <- 30000
  convFacTSH <- 1e-6 * mwTSH * 1e6
  convFacTSH_data = 1
  mwMMI <- 114.17
  convFactMMI <- 1e-6 * mwMMI
  
  plotMappingConfiguration <- enum(list(
    xFac = list("MMI VB simulated" = 1/60
    ),
    
    yFac = list("MMI VB simulated" = convFactMMI
    ),
    
    xOffset = list(
    ),
    
    lineTypes = list()
  ))
  
  allColors <- esqLABS_colors(1)
  plotMappingConfiguration$colors <- list("T3 PVB simulated" = allColors[[1]],
                                          "T3 data" = allColors[[1]],
                                          "T4 PVB simulated" = allColors[[1]],
                                          "T4 data" = allColors[[1]]
                                          
  )
  
  #Create plot mappings
  X_VALS_COL_NAME <- "Time.[day(s)]"
  Y_VALS_COL_NAME <- "Concentration.(mass).[ng/ml]"
  Y_ERROR_COL_NAME <- "Error.[ng/ml]"
  
  ####T4 plotmapping
  t4Mapping <- PlotMapping$new()
  t4Mapping$addModelOutputs(paths = OutputPaths$T4_PVB,  resultsData = simulatedScenario$outputValues, lables = "T4 PVB simulated")
  t4Mapping$addXYSeries(xValsList = observedData$Reeth_1987$T4[[X_VALS_COL_NAME]],
                        yValsList = observedData$Reeth_1987$T4[[Y_VALS_COL_NAME]],
                        yErrorList = observedData$Reeth_1987$T4[[Y_ERROR_COL_NAME]],
                        lables = "T4 data")
  t4Mapping$setPlotConfiguration(plotMappingConfiguration)
  #  t4Mapping$xLim <- c(0, 24)
  #  t4Mapping$yLim <- c(0, 7)
  t4Mapping$xLab = "Time [h]"
  t4Mapping$yLab = "T4 PVB [ng/ml]"
  
  ####T3 plotmapping
  t3Mapping <- PlotMapping$new()
  t3Mapping$addModelOutputs(paths = OutputPaths$T3_PVB,  resultsData = simulatedScenario$outputValues, lables = "T3 PVB simulated")
  t3Mapping$addXYSeries(xValsList = observedData$Reeth_1987$T3[[X_VALS_COL_NAME]],
                        yValsList = observedData$Reeth_1987$T3[[Y_VALS_COL_NAME]],
                        yErrorList = observedData$Reeth_1987$T3[[Y_ERROR_COL_NAME]],
                        lables = "T3 data")
  t3Mapping$setPlotConfiguration(plotMappingConfiguration)
  #  t4Mapping$xLim <- c(0, 24)
  #  t4Mapping$yLim <- c(0, 7)
  t3Mapping$xLab = "Time [h]"
  t3Mapping$yLab = "T3 PVB [ng/ml]"
  
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
  
  plotMultiPanel(c(t4Mapping, t3Mapping), plotConfiguration)
}