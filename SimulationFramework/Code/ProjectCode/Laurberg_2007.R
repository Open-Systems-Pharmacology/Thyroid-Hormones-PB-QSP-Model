Laurberg_2007 <- function(runConfiguration){
  runConfiguration$scenarioName <- "Laurberg_2007"
  runConfiguration$simulationTime <- 21 * 24 * 60
  runConfiguration$steadyStateTime <- 2000 * 60
  runConfiguration$paramSheets <- c("Global", "PI human", "Human GD", "PTU_human", "MMI_human")
  runConfiguration$individualCharacteristics <- NULL
  runConfiguration$dataSheets <- "Laurberg_2007"
  
  simulatedScenario <- runScenario(runConfiguration = runConfiguration)
  observedData <- readObservedData(runConfiguration)
  
  #Factor for conversion of standard output units
  mwT4 <- 776.8
  convFacT4 <- mwT4#1e-6 * mwT4 * 1e6
  convFacT4_data <- mwT4 * 1e-3
  mwT3 <- 650.98
  convFacT3 <- mwT3#1e-6 * mwT3 * 1e6
  convFacT3_data <- mwT3 * 1e-3
  mwTSH <- 30000
  convFacTSH <- 1e-6 * mwTSH * 1e6
  convFacTSH_data = 1
  
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
  
  allColors <- esqLABS_colors(1)
  plotMappingConfiguration$colors <- list("T3 PVB simulated" = allColors[[1]],
                                          "T3 data" = allColors[[1]],
                                          "T4 PVB simulated" = allColors[[1]],
                                          "T4 data" = allColors[[1]]
                                          
  )
  
  #Create plot mappings
  X_VALS_COL_NAME <- "Time.[h]"
  Y_VALS_COL_NAME <- "Concentration.(molar).[nmol/l]"
  Y_ERROR_COL_NAME <- "Error.[nmol/l]"
  
  ####T4 plotmapping
  t4Mapping <- PlotMapping$new()
  t4Mapping$addModelOutputs(paths = OutputPaths$T4_PVB, resultsData = simulatedScenario$outputValues, lables = "T4 PVB simulated")
  t4Mapping$addXYSeries(xValsList = observedData$Laurberg_2007$T4[[X_VALS_COL_NAME]],
                        yValsList = observedData$Laurberg_2007$T4[[Y_VALS_COL_NAME]] * convFacT4_data,
                        yErrorList = observedData$Laurberg_2007$T4[[Y_ERROR_COL_NAME]] * convFacT4_data,
                        lables = "T4 data")
  t4Mapping$addXYSeries(xValsList = simulatedScenario$outputValues$data$Time / 60,
                        yValsList = rep(231 * convFacT4_data, length(simulatedScenario$outputValues$data$Time)),
                        lables = "T4 baseline")
  t4Mapping$setPlotConfiguration(plotMappingConfiguration)
#    t4Mapping$xLim <- c(0, 168)
  #  t4Mapping$yLim <- c(0, 7)
  t4Mapping$xLab = "Time [h]"
  t4Mapping$yLab = "T4 [ng/ml]"
  t4Mapping$setColors(lables = "T4 baseline", colors = "grey")
  t4Mapping$setTypes(lables = "T4 baseline", types = "l")
  
  ####T3 plotmapping
  t3Mapping <- PlotMapping$new()
  t3Mapping$addModelOutputs(paths = OutputPaths$T3_PVB,  resultsData = simulatedScenario$outputValues, lables = "T3 PVB simulated")
  t3Mapping$addXYSeries(xValsList = observedData$Laurberg_2007$T3[[X_VALS_COL_NAME]],
                        yValsList = observedData$Laurberg_2007$T3[[Y_VALS_COL_NAME]] * convFacT3_data,
                        yErrorList = observedData$Laurberg_2007$T3[[Y_ERROR_COL_NAME]] * convFacT3_data,
                        lables = "T3 data")
  t3Mapping$addXYSeries(xValsList = simulatedScenario$outputValues$data$Time / 60,
                        yValsList = rep(6.47 * convFacT3_data, length(simulatedScenario$outputValues$data$Time)),
                        lables = "T3 baseline")
  t3Mapping$setPlotConfiguration(plotMappingConfiguration)
 #   t3Mapping$xLim <- c(0, 168)
  #  t4Mapping$yLim <- c(0, 7)
  t3Mapping$xLab = "Time [h]"
  t3Mapping$yLab = "T3 [ng/ml]"
  t3Mapping$setColors(lables = "T3 baseline", colors = "grey")
  t3Mapping$setTypes(lables = "T3 baseline", types = "l")
  
  plotConfiguration <- enum(list(
    outputToPNG = runConfiguration$outputToPNG,
    outputName = runConfiguration$scenarioName,
    outputPath = runConfiguration$outputPath,
    width = NULL,
    height = 9,
    nrOfCols = NULL,
    res = 600,
    pointsize = 12,
    addTitle = FALSE
  ))
  
  plotConfiguration$outputName <- "Laurberg_2007_T4"
  plotMultiPanel(t4Mapping, plotConfiguration)
  
  plotConfiguration$outputName <- "Laurberg_2007_T3"
  plotMultiPanel(t3Mapping, plotConfiguration)
}