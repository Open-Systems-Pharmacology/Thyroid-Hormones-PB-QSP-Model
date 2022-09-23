Cooper_1983_PTU <- function(projectConfiguration, saveSimulation = FALSE){
  runConfiguration$scenarioName <- "Cooper_1983_PTU"
  runConfiguration$simulationTime <- 400*60
  runConfiguration$steadyStateTime <- 800 * 60
  runConfiguration$paramSheets <- c("Global", "Physiology_rat", "PI rat", "PTU_rat")
  runConfiguration$individualCharacteristics <- NULL
  runConfiguration$dataSheets <- "Leonard2016Fig3"
  
  simulationDose <- c(
    0.000175392,
    0.00087696,
    0.00175392,
    0.0087696,
    0.0175392,
    0.087696
  )
  
  exports <- list(
    "Administrations", "disableApplications", "setApplications",
    "OutputPaths", "getTestParameters"
  )
  
  simulatedScenarios <- executeInParallel(
    fun = runScenario, firstArguments = simulationDose,
    exports = exports, runConfiguration = runConfiguration,
    outputNames = paste(runConfiguration$scenarioName, simulationDose, sep = "_")
  )
  observedData <- readObservedData(runConfiguration)
  
  if (saveSimulation){
    refSim <- runScenario(simulationDose[[length(simulationDose)]], runConfiguration)
    saveSimulation(refSim$simulation, paste0("../", runConfiguration$scenarioName, ".pkml"))
  }
  
  mwT4 <- 776.8
  convFacT4 <- 1e-6 * mwT4 * 1e6 * 1e-3
  mwT3 <- 650.98
  convFacT3 <- 1e-6 * mwT3 * 1e6 * 1e-3
  mwTSH <- 30000
  convFacTSH <- 1e-6 * mwTSH * 1e6 * 1e-3
  observedT3 <- c(
    0.00036,
    0.00032,
    0.000286667,
    0.00024,
    0.000226667,
    0.000255
  )
  errorT3 <- c(0.00005,
               0.00003,
               0.00004,
               0.00002,
               0.00003,
               0)
  observedT4 <- c(
    0.038666667,
    0.025333333,
    0.018666667,
    0.010666667,
    0.006666667,
    6.34E-03
  )
  errorT4 <- c(0.004,
               0.004,
               0.004,
               0.002,
               0.001,
               0)
  observedTSH <- c(
    0.003965517,
    0.010086207,
    0.008448276,
    0.010172414,
    0.008965517,
    0
  )
  errorTSH <- c(0.001,
                0.0015,
                0.0007,
                0.0018,
                0.0006,
                0)
  
  simulatedT3 <- sapply(simulatedScenarios, function(x){
    idx <- getIndexClosestToValue(9*24*60, x$outputValues$data$Time)
    x$outputValues$data[[OutputPaths$T3_VB]][[idx]] * convFacT3
  },
  USE.NAMES = FALSE
  )
  simulatedT4 <- sapply(simulatedScenarios, function(x){
    idx <- getIndexClosestToValue(9*24*60, x$outputValues$data$Time)
    x$outputValues$data[[OutputPaths$T4_VB]][[idx]] * convFacT4
  },
  USE.NAMES = FALSE
  )
  simulatedTSH <- sapply(simulatedScenarios, function(x){
    idx <- getIndexClosestToValue(9*24*60, x$outputValues$data$Time)
    x$outputValues$data[[OutputPaths$TSH_VB]][[idx]] * convFacTSH
  },
  USE.NAMES = FALSE
  )
  
  plotMapping_TSH <- PlotMapping$new()
  plotMapping_TSH$addXYSeries(xValsList = simulationDose,
                              yValsList = observedTSH,
                              yErrorList = errorTSH,
                              lables = "TSH data")
  plotMapping_TSH$addXYSeries(xValsList = simulationDose,
                              yValsList = simulatedTSH,
                              lables = "TSH simulated")
  plotMapping_TSH$addXYSeries(xValsList = simulationDose,
                              yValsList = rep(0.0025,
                                              times = length(simulationDose)),
                              lables = "TSH baseline")
  plotMapping_TSH$xLab <- "Dose [%]"
  plotMapping_TSH$yLab <- "TSH VB [µg/ml]"
  plotMapping_TSH$log <- "x"
  plotMapping_TSH$setColors(lables = "TSH baseline", colors = "grey")
  plotMapping_TSH$setTypes(lables = "TSH baseline", types = "l")
  plotMapping_TSH$setTypes(lables = "TSH simulated", types = "b")
  
  plotMapping_T3 <- PlotMapping$new()
  plotMapping_T3$addXYSeries(xValsList = simulationDose,
                             yValsList = observedT3,
                             yErrorList = errorT3,
                             lables = "T3 data")
  plotMapping_T3$addXYSeries(xValsList = simulationDose,
                             yValsList = simulatedT3,
                             lables = "T3 simulated")
  plotMapping_T3$addXYSeries(xValsList = simulationDose,
                             yValsList = rep(0.0004,
                                             times = length(simulationDose)),
                             lables = "T3 baseline")
  plotMapping_T3$xLab <- "Dose [%]"
  plotMapping_T3$yLab <- "T3 VB [µg/ml]"
  plotMapping_T3$log <- "x"
  plotMapping_T3$setColors(lables = "T3 baseline", colors = "grey")
  plotMapping_T3$setTypes(lables = "T3 baseline", types = "l")
  plotMapping_T3$setTypes(lables = "T3 simulated", types = "b")
  
  plotMapping_T4 <- PlotMapping$new()
  plotMapping_T4$addXYSeries(xValsList = simulationDose,
                             yValsList = observedT4,
                             yErrorList = errorT4,
                             lables = "T4 data")
  plotMapping_T4$addXYSeries(xValsList = simulationDose,
                             yValsList = simulatedT4,
                             lables = "T4 simulated")
  plotMapping_T4$addXYSeries(xValsList = simulationDose,
                             yValsList = rep(0.028,
                                             times = length(simulationDose)),
                             lables = "T4 baseline")
  plotMapping_T4$xLab <- "Dose [%]"
  plotMapping_T4$yLab <- "T4 VB [µg/ml]"
  plotMapping_T4$log <- "x"
  plotMapping_T4$setColors(lables = "T4 baseline", colors = "grey")
  plotMapping_T4$setTypes(lables = "T4 baseline", types = "l")
  plotMapping_T4$setTypes(lables = "T4 simulated", types = "b")
  
  plotConfiguration <- enum(list(
    outputToPNG = runConfiguration$outputToPNG,
    outputName = "Cooper_1983_PTU",
    outputPath = runConfiguration$outputPath,
    width = NULL,
    height = NULL,
    nrOfCols = NULL,
    res = 600,
    pointsize = 8,
    addTitle = TRUE
  ))
  
  plotMultiPanel(c(plotMapping_T4, plotMapping_T3, plotMapping_TSH), plotConfiguration)
  
  
  #############
  convFacT4 <- 1e-6 * mwT4 * 1e6
  convFacT3 <- 1e-6 * mwT3 * 1e6
  
  plotMappingConfiguration <- enum(list(
    xFac = list("T3 VB simulated" = 1/60,
                "T4 VB simulated" = 1/60,
                "TSH VB simulated" = 1/60,
                "T3 data" = 24,
                "T4 data" = 24
    ),
    
    yFac = list("T4 VB simulated" = convFacT4,
                "T3 VB simulated" = convFacT3,
                "TSH VB simulated" = convFacTSH
    ),
    
    xOffset = list("T3 VB simulated" = -2*24*60,
                   "T3 data" = -2,
                   "T4 VB simulated" = -2*24*60,
                   "T4 data" = -2,
                   "TSH VB simulated" = -2*24*60,
                   "TSH data" = -2
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
  X_VALS_COL_NAME <- "Time.[day(s)]"
  Y_VALS_COL_NAME <- "Concentration.[ng/mL]"
  
  ####T4 plotmapping
  t4Mapping <- PlotMapping$new()
  t4Mapping$addModelOutputs(paths = OutputPaths$T4_VB,  resultsData = simulatedScenarios$Cooper_1983_PTU_0.087696$outputValues, lables = "T4 VB simulated")
  t4Mapping$addXYSeries(xValsList = observedData$Leonard2016Fig3$PO_12.5mg_PTU_Rat_T4[[X_VALS_COL_NAME]],
                        yValsList = observedData$Leonard2016Fig3$PO_12.5mg_PTU_Rat_T4[[Y_VALS_COL_NAME]],
                        lables = "T4 data")
  t4Mapping$addXYSeries(xValsList = simulatedScenarios$Cooper_1983_PTU_0.087696$outputValues$data$Time / 60,
                        yValsList = rep(28, length(simulatedScenarios$Cooper_1983_PTU_0.087696$outputValues$data$Time)),
                        lables = "T4 baseline")
  t4Mapping$setPlotConfiguration(plotMappingConfiguration)
  #t4Mapping$xLim <- c(0, 110)
  #  t3Mapping$yLim <- c(0, 7)
  t4Mapping$xLab = "Time [h]"
  t4Mapping$yLab = "T4 VB [ng/ml]"
  t4Mapping$setColors(lables = "T4 baseline", colors = "grey")
  t4Mapping$setTypes(lables = "T4 baseline", types = "l")
  
  ####T3 plotmapping
  t3Mapping <- PlotMapping$new()
  t3Mapping$addModelOutputs(paths = OutputPaths$T3_VB,  resultsData = simulatedScenarios$Cooper_1983_PTU_0.087696$outputValues, lables = "T3 VB simulated")
  t3Mapping$addXYSeries(xValsList = observedData$Leonard2016Fig3$PO_12.5mg_PTU_Rat_T3[[X_VALS_COL_NAME]],
                        yValsList = observedData$Leonard2016Fig3$PO_12.5mg_PTU_Rat_T3[[Y_VALS_COL_NAME]],
                        lables = "T3 data")
  t3Mapping$addXYSeries(xValsList = simulatedScenarios$Cooper_1983_PTU_0.087696$outputValues$data$Time / 60,
                        yValsList = rep(0.4, length(simulatedScenarios$Cooper_1983_PTU_0.087696$outputValues$data$Time)),
                        lables = "T3 baseline")
  t3Mapping$setPlotConfiguration(plotMappingConfiguration)
  # t3Mapping$xLim <- c(0, 110)
  #  t3Mapping$yLim <- c(0, 7)
  t3Mapping$xLab = "Time [h]"
  t3Mapping$yLab = "T3 VB [ng/ml]"
  t3Mapping$setColors(lables = "T3 baseline", colors = "grey")
  t3Mapping$setTypes(lables = "T3 baseline", types = "l")
  
  plotConfiguration <- enum(list(
    outputToPNG = runConfiguration$outputToPNG,
    outputName = "Cooper_1983_PTU_0.05",
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
