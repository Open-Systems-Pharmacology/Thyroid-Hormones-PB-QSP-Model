Pilo_1990_T3 <- function(runConfiguration){
  runConfiguration$scenarioName <- "Pilo1990Fig5_T3"
  runConfiguration$simulationTime <- 100 * 60
  runConfiguration$steadyStateTime <- 2000 * 60
  runConfiguration$paramSheets <- c("Global", "PI human", "disableEndogenousSecretion")
  runConfiguration$individualCharacteristics <- NULL
  runConfiguration$dataSheets <- "Pilo1990Fig5"
  
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
    xFac = list("T3 PVB simulated" = 1/60
    ),
    
    yFac = list("T3 PVB simulated" = convFacT3
    ),
    
    xOffset = list(
    ),
    
    lineTypes = list()
  ))
  
  allColors <- esqLABS_colors(1)
  plotMappingConfiguration$colors <- list("T3 PVB simulated" = allColors[[1]],
                                          "T3 data" = allColors[[1]]
  )
  
  #Create plot mappings
  X_VALS_COL_NAME <- "time.[h]"
  Y_VALS_COL_NAME <- "Concentration.[µg/L]"

  ####T3 plotmapping
  Pilo_1990_T3 <- PlotMapping$new()
  Pilo_1990_T3$addModelOutputs(paths = OutputPaths$T3_PVB,  resultsData = simulatedScenario$outputValues, lables = "T3 PVB simulated")
  Pilo_1990_T3$addXYSeries(xValsList = observedData$Pilo1990Fig5$`IV_170ng125T4-86ng131T3_Human_131T3`[[X_VALS_COL_NAME]],
                                   yValsList = observedData$Pilo1990Fig5$`IV_170ng125T4-86ng131T3_Human_131T3`[[Y_VALS_COL_NAME]],
                                   lables = "T3 data")
  Pilo_1990_T3$setPlotConfiguration(plotMappingConfiguration)
  Pilo_1990_T3$xLim <- c(0, 100)
#  Pilo_1990_T3$yLim <- c(0, 3)
  Pilo_1990_T3$xLab <- "Time [h]"
  Pilo_1990_T3$yLab <- "T3 PVB [ng/ml]"
  Pilo_1990_T3$log <- "y"

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
  
  plotMultiPanel(Pilo_1990_T3, plotConfiguration)
}
