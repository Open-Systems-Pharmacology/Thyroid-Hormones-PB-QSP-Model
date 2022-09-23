Wong_2005_PTU <- function(runConfiguration){
  runConfiguration$scenarioName <- "Wong_2005_PTU"
  runConfiguration$simulationTime <- 220 * 60
  runConfiguration$steadyStateTime <- 2000 * 60
  runConfiguration$paramSheets <- c("Global", "Physiology_rat", "PI rat", "PTU_rat")
  runConfiguration$individualCharacteristics <- NULL
  runConfiguration$dataSheets <- "Wong_2005"
  
  simulatedScenario <- runScenario(runConfiguration = runConfiguration)
  observedData <- readObservedData(runConfiguration)
  
  #Factor for conversion of standard output units
  mwT4 <- 776.8
  convFacT4 <- 1e-6 * mwT4 * 1e9 * 1e-3
  mwT3 <- 650.98
  convFacT3 <- 1e-6 * mwT3 * 1e6
  mwTSH <- 30000
  convFacTSH <- 1e-6 * mwTSH * 1e6
  convFacTSH_data = 1
  
  plotMappingConfiguration <- enum(list(
    xFac = list("T3 VB simulated" = 1/60,
                "T4 VB simulated" = 1/60,
                "TSH VB simulated" = 1/60
    ),
    
    yFac = list("T4 VB simulated" = convFacT4,
                "T3 VB simulated" = convFacT3,
                "TSH VB simulated" = convFacTSH
    ),
    
    xOffset = list("T3 VB simulated" = -2*24*60,
                   "T3 data" = -2*24,
                   "T4 VB simulated" = -2*24*60,
                   "T4 data" = -2*24,
                   "TSH VB simulated" = -2*24*60,
                   "TSH data" = -2*24
    ),
    
    lineTypes = list()
  ))
  
  allColors <- esqLABS_colors(1)
  plotMappingConfiguration$colors <- list("T3 VB simulated" = allColors[[1]],
                                          "T3 data" = allColors[[1]],
                                          "T4 VB simulated" = allColors[[1]],
                                          "T4 data" = allColors[[1]],
                                          "TSH VB simulated" = allColors[[1]],
                                          "TSH data" = allColors[[1]]
  )
  
  #Create plot mappings
  X_VALS_COL_NAME <- "Time.[h]"
  Y_VALS_COL_NAME <- "Concentration.(mass).[ng/ml]"
  
  ####T4 plotmapping
  t4Mapping <- PlotMapping$new()
  t4Mapping$addModelOutputs(paths = OutputPaths$T4_VB,  resultsData = simulatedScenario$outputValues, lables = "T4 VB simulated")
  t4Mapping$addXYSeries(xValsList = observedData$Wong_2005$T4[[X_VALS_COL_NAME]],
                        yValsList = observedData$Wong_2005$T4[[Y_VALS_COL_NAME]],
                        lables = "T4 data")
  t4Mapping$addXYSeries(xValsList = simulatedScenario$outputValues$data$Time / 60,
                        yValsList = rep(28, length(simulatedScenario$outputValues$data$Time)),
                        lables = "T4 baseline")
  t4Mapping$setPlotConfiguration(plotMappingConfiguration)
  t4Mapping$xLim <- c(0, 150)
#  t3Mapping$yLim <- c(0, 7)
  t4Mapping$xLab = "Time [h]"
  t4Mapping$yLab = "T4 VB [ng/ml]"
  t4Mapping$setColors(lables = "T4 baseline", colors = "grey")
  t4Mapping$setTypes(lables = "T4 baseline", types = "l")
  
  ####T3 plotmapping
  t3Mapping <- PlotMapping$new()
  t3Mapping$addModelOutputs(paths = OutputPaths$T3_VB,  resultsData = simulatedScenario$outputValues, lables = "T3 VB simulated")
  t3Mapping$addXYSeries(xValsList = observedData$Wong_2005$T3[[X_VALS_COL_NAME]],
                        yValsList = observedData$Wong_2005$T3[[Y_VALS_COL_NAME]],
                        lables = "T3 data")
  t3Mapping$addXYSeries(xValsList = simulatedScenario$outputValues$data$Time / 60,
                        yValsList = rep(0.4, length(simulatedScenario$outputValues$data$Time)),
                        lables = "T3 baseline")
  t3Mapping$setPlotConfiguration(plotMappingConfiguration)
  t3Mapping$xLim <- c(0, 150)
  #  t3Mapping$yLim <- c(0, 7)
  t3Mapping$xLab = "Time [h]"
  t3Mapping$yLab = "T3 VB [ng/ml]"
  t3Mapping$setColors(lables = "T3 baseline", colors = "grey")
  t3Mapping$setTypes(lables = "T3 baseline", types = "l")
  
  ####TSH plotmapping
  tshMapping <- PlotMapping$new()
  tshMapping$addModelOutputs(paths = OutputPaths$TSH_VB,  resultsData = simulatedScenario$outputValues, lables = "TSH VB simulated")
  tshMapping$addXYSeries(xValsList = observedData$Wong_2005$TSH[[X_VALS_COL_NAME]],
                        yValsList = observedData$Wong_2005$TSH[[Y_VALS_COL_NAME]],
                        lables = "TSH data")
  tshMapping$addXYSeries(xValsList = simulatedScenario$outputValues$data$Time / 60,
                        yValsList = rep(2.5, length(simulatedScenario$outputValues$data$Time)),
                        lables = "TSH baseline")
  tshMapping$setPlotConfiguration(plotMappingConfiguration)
  tshMapping$xLim <- c(0, 150)
  #  t3Mapping$yLim <- c(0, 7)
  tshMapping$xLab = "Time [h]"
  tshMapping$yLab = "TSH VB [ng/ml]"
  tshMapping$setColors(lables = "TSH baseline", colors = "grey")
  tshMapping$setTypes(lables = "TSH baseline", types = "l")
  
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
  
  plotMultiPanel(c(t4Mapping, t3Mapping, tshMapping), plotConfiguration)
}