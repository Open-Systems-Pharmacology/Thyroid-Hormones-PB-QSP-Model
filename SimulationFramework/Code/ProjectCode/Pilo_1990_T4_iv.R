Pilo_1990_T4 <- function(runConfiguration){
  runConfiguration$scenarioName <- "Pilo1990Fig5_T4"
  runConfiguration$simulationTime <- 170 * 60
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
    xFac = list("T3 PVB simulated" = 1/60,
                "T4 PVB simulated" = 1/60
    ),
    
    yFac = list("T3 PVB simulated" = convFacT3,
                "T4 PVB simulated" = convFacT4
    ),
    
    xOffset = list(
    ),
    
    lineTypes = list()
  ))
  
  allColors <- esqLABS_colors(2)
  plotMappingConfiguration$colors <- list("T3 PVB simulated" = allColors[[1]],
                                          "T3 data" = allColors[[1]],
                                          "T4 PVB simulated" = allColors[[2]],
                                          "T4 data" = allColors[[2]]
  )
  
  #Create plot mappings
  X_VALS_COL_NAME <- "time.[h]"
  Y_VALS_COL_NAME <- "Concentration.[µg/L]"
  
  ####T3 plotmapping
  Pilo_1990_mapping <- PlotMapping$new()
  Pilo_1990_mapping$addModelOutputs(paths = c(OutputPaths$T3_PVB, OutputPaths$T4_PVB),
                                    resultsData = simulatedScenario$outputValues, lables = c("T3 PVB simulated", "T4 PVB simulated"))
  Pilo_1990_mapping$addXYSeries(xValsList = list(observedData$Pilo1990Fig5$`IV_170ng125T4-86ng131T3_Human_125T3`[[X_VALS_COL_NAME]],
                                                 observedData$Pilo1990Fig5$`IV_170ng125T4-86ng131T3_Human_125T4`[[X_VALS_COL_NAME]]),
                           yValsList = list(observedData$Pilo1990Fig5$`IV_170ng125T4-86ng131T3_Human_125T3`[[Y_VALS_COL_NAME]],
                                            observedData$Pilo1990Fig5$`IV_170ng125T4-86ng131T3_Human_125T4`[[Y_VALS_COL_NAME]]),
                           lables = c("T3 data", "T4 data"))
  Pilo_1990_mapping$setPlotConfiguration(plotMappingConfiguration)
  Pilo_1990_mapping$xLim <- c(0, 170)
  Pilo_1990_mapping$yLim <- c(0, 0.05)
  Pilo_1990_mapping$xLab = "Time [h]"
  Pilo_1990_mapping$yLab = "Concentration PVB [ng/ml]"
  Pilo_1990_mapping$log <- "y"
  
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
  
  plotMultiPanel(Pilo_1990_mapping, plotConfiguration)
}