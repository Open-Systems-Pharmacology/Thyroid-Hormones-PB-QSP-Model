Andrare_1999 <- function(runConfiguration){
  runConfiguration$scenarioName <- "Andrade_1999"
  runConfiguration$simulationTime <- 50 * 24 * 60
  runConfiguration$steadyStateTime <- 2000 * 60
  runConfiguration$paramSheets <- c("Global", "PI human", "Human GD", "MMI_human")
  runConfiguration$individualCharacteristics <- NULL
  runConfiguration$dataSheets <- "Andrare_1999"
  
  simulatedScenario <- runScenario(runConfiguration = runConfiguration)
  observedData <- readObservedData(runConfiguration)
  
  #Factor for conversion of standard output units
  mwT4 <- 776.8
  convFacT4 <- 1e-6 * mwT4 * 1e6 * 1e-3
  mwT3 <- 650.98
  convFacT3 <- 1e-6 * mwT3 * 1e6 * 1e-3
  mwTSH <- 30000
  convFacTSH <- 1e-6 * mwTSH * 1e6
  convFacTSH_data = 1
  
  plotMappingConfiguration <- enum(list(
    xFac = list("T3 PVB simulated" = 1/60,
                "T4 PVB simulated" = 1/60,
                "T3 data" = 24,
                "T4 data" = 24
    ),
    
    yFac = list("T3 PVB simulated" = convFacT3,
                "T4 PVB simulated" = convFacT4
    ),
    
    xOffset = list(
      "T3 data" = 25,
      "T4 data" = 25
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
  Y_VALS_COL_NAME <- "Concentration.(mass).[µg/ml]"
  Y_ERROR_COL_NAME <- "Error.[µg/ml]"
  
  ####T4 plotmapping
  t4Mapping <- PlotMapping$new()
  t4Mapping$addModelOutputs(paths = OutputPaths$T4_PVB,  resultsData = simulatedScenario$outputValues, lables = "T4 PVB simulated")
  t4Mapping$addXYSeries(xValsList = observedData$Andrare_1999$T4[[X_VALS_COL_NAME]],
                        yValsList = observedData$Andrare_1999$T4[[Y_VALS_COL_NAME]],
                        lables = "T4 data")
  t4Mapping$addXYSeries(xValsList = simulatedScenario$outputValues$data$Time / 60,
                        yValsList = rep(0.1811, length(simulatedScenario$outputValues$data$Time)),
                        lables = "T4 baseline")
  t4Mapping$setPlotConfiguration(plotMappingConfiguration)
  #  t4Mapping$xLim <- c(0, 24)
  #  t4Mapping$yLim <- c(0, 7)
  t4Mapping$xLab = "Time [h]"
  t4Mapping$yLab = "T4 PVB [µg/ml]"
  t4Mapping$setColors(lables = "T4 baseline", colors = "grey")
  t4Mapping$setTypes(lables = "T4 baseline", types = "l")
  
  ####T3 plotmapping
  t3Mapping <- PlotMapping$new()
  t3Mapping$addModelOutputs(paths = OutputPaths$T3_PVB,  resultsData = simulatedScenario$outputValues, lables = "T3 PVB simulated")
  t3Mapping$addXYSeries(xValsList = observedData$Andrare_1999$T3[[X_VALS_COL_NAME]],
                        yValsList = observedData$Andrare_1999$T3[[Y_VALS_COL_NAME]],
                        lables = "T3 data")
  t3Mapping$addXYSeries(xValsList = simulatedScenario$outputValues$data$Time / 60,
                        yValsList = rep(0.00421, length(simulatedScenario$outputValues$data$Time)),
                        lables = "T3 baseline")
  t3Mapping$setPlotConfiguration(plotMappingConfiguration)
  #  t4Mapping$xLim <- c(0, 24)
  #  t4Mapping$yLim <- c(0, 7)
  t3Mapping$xLab = "Time [h]"
  t3Mapping$yLab = "T3 PVB [µg/ml]"
  t3Mapping$setColors(lables = "T3 baseline", colors = "grey")
  t3Mapping$setTypes(lables = "T3 baseline", types = "l")
  
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