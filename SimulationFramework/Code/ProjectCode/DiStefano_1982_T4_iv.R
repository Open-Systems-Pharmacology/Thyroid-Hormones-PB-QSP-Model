DiStefano_1982_T4_iv <- function(runConfiguration){
  runConfiguration$scenarioName <- "DiStefano_1982_T4_iv"
  runConfiguration$simulationTime <- 24 * 60
  runConfiguration$steadyStateTime <- 2000 * 60
  runConfiguration$paramSheets <- c("Global", "Physiology_rat", "PI rat", "disableEndogenousSecretion")
  runConfiguration$individualCharacteristics <- NULL
  runConfiguration$dataSheets <- "DiStefano1982bFig2"
  
  simulatedScenario <- runScenario(runConfiguration = runConfiguration)
  observedData <- readObservedData(runConfiguration)
  
  #Factor for conversion of standard output units
  mwT4 <- 776.8
  convFacT4 <- 1e-6 * mwT4 * 1e6
  mwT3 <- 650.98
  convFacT3 <- 1e-6 * mwT3 * 1e6
  mwTSH <- 30000
  convFacTSH <- 1e-6 * mwTSH * 1e6
  
  plotMappingConfiguration <- enum(list(
    xFac = list("T4 VB simulated" = 1/60,
                "T4 data" = 1/60
    ),
    
    yFac = list("T4 VB simulated" = convFacT4
    ),
    
    xOffset = list(
    ),
    
    lineTypes = list()
  ))
  
  allColors <- esqLABS_colors(1)
  plotMappingConfiguration$colors <- list("T4 VB simulated" = allColors[[1]],
                                          "T4 data" = allColors[[1]]
  )
  
  #Create plot mappings
  X_VALS_COL_NAME <- "Time.[min]"
  Y_VALS_COL_NAME <- "Concentration.[ng/mL]"
  
  ####T3 plotmapping
  t4Mapping <- PlotMapping$new()
  t4Mapping$addModelOutputs(paths = OutputPaths$T4_VB,  resultsData = simulatedScenario$outputValues, lables = "T4 VB simulated")
  t4Mapping$addXYSeries(xValsList = observedData$DiStefano1982bFig2$IV_13.6ng_125T4_Rat_125T4[[X_VALS_COL_NAME]],
                           yValsList = observedData$DiStefano1982bFig2$IV_13.6ng_125T4_Rat_125T4[[Y_VALS_COL_NAME]],
                           lables = "T4 data")
  t4Mapping$setPlotConfiguration(plotMappingConfiguration)
  t4Mapping$xLim <- c(0, 24)
  t4Mapping$yLim <- c(0.001, 0.3)
  t4Mapping$xLab <- "Time [h]"
  t4Mapping$yLab <- "T4 VB [ng/ml]"
  t4Mapping$log <- "y"
  
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
  
  plotMultiPanel(t4Mapping, plotConfiguration)
}
