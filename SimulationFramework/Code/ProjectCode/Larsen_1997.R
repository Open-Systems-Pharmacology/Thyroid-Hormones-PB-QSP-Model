Larsen_1997 <- function(projectConfiguration){
  runConfiguration$scenarioName <- "Pilo1990Fig5_T3"
  runConfiguration$simulationTime <- 100 * 60
  runConfiguration$steadyStateTime <- 2000 * 60
  runConfiguration$paramSheets <- c("Global", "PI human", "disableEndogenousSecretion")
  runConfiguration$individualCharacteristics <- NULL
  runConfiguration$dataSheets <- "Pilo1990Fig5"

  runConfiguration$scenarioName <- "Larsen_1997_T3_iv"
  runConfiguration$simulationTime <- 7 * 60
  runConfiguration$steadyStateTime <- 800 * 60
  runConfiguration$paramSheets <- c("Global", "Physiology_rat", "PI rat", "PI rat_tx")
  runConfiguration$individualCharacteristics <- NULL
  runConfiguration$dataSheets <- "Larsen_1977"
  observedData <- readObservedData(runConfiguration)
  
  simulatedScenario_T3 <- runScenario(runConfiguration)
  runConfiguration$scenarioName <- "Larsen_1997_T4_iv"
  simulatedScenario_T4 <- runScenario(runConfiguration)

  
  #Factor for conversion of standard output units
  mwT4 <- 776.8
  convFacT4 <- 1e-6 * mwT4 * 1e6
  mwT3 <- 650.98
  convFacT3 <- 1e-6 * mwT3 * 1e6
  mwTSH <- 30000
  convFacTSH <- 1e-6 * mwTSH * 1e6
  
  plotMappingConfiguration <- enum(list(
    xFac = list("TSH VB T3 administration simulated" = 1/60,
                "TSH VB T4 administration simulated" = 1/60,
                
                "T3 VB T3 administration simulated" = 1/60,
                "T3 VB T4 administration simulated" = 1/60,
                "T4 VB T4 administration simulated" = 1/60,
                "T4 thyroid simulated" = 1/60,
                "T4 pituitary simulated" = 1/60,
                "T3 VB simulated" = 1/60,
                "T3 thyroid simulated" = 1/60,
                "T3 pituitary simulated" = 1/60,
                "T4 VB simulated" = 1/60,
                "T4 thyroid simulated" = 1/60,
                "T4 pituitary simulated" = 1/60
    ),
    
    yFac = list("TSH VB T3 administration simulated" = convFacTSH,
                "TSH VB T4 administration simulated" = convFacTSH,
                "T3 VB T3 administration simulated" = convFacT3,
                "T3 VB T4 administration simulated" = convFacT3,
                "T4 VB T4 administration simulated" = convFacT4
    ),
    
    xOffset = list(
      "T3 data" = 0,
      "TSH data" = 0
    ),
    
    lineTypes = list()
  ))
  
  allColors <- esqLABS_colors(2)
  plotMappingConfiguration$colors <- list("TSH VB T3 administration simulated" = allColors[[1]],
                                          "TSH VB T4 administration simulated" = allColors[[2]],
                                          "T3 VB T3 administration simulated" = allColors[[1]],
                                          "T3 VB T4 administration simulated" = allColors[[2]],
                                          "TSH VB T3 administration data" = allColors[[1]],
                                          "TSH VB T4 administration data" = allColors[[2]],
                                          "T3 VB T3 administration data" = allColors[[1]],
                                          "T3 VB T4 administration data" = allColors[[2]],
                                          "T4 VB T4 administration simulated" = allColors[[1]],
                                          "T4 VB T4 administration data" = allColors[[1]]
  )
  
  #Create plot mappings
  X_VALS_COL_NAME <- "Time.[h]"
  Y_VALS_COL_NAME <- "Concentration.(mass).[ng/ml]"
  Y_ERROR_COL_NAME <- "Error.[ng/ml]"
  
  plotMapping_TSH <- PlotMapping$new()
  plotMapping_TSH$addModelOutputs(paths = OutputPaths$TSH_VB,  resultsData = simulatedScenario_T3$outputValues, lables = "TSH VB T3 administration simulated")
  plotMapping_TSH$addModelOutputs(paths = OutputPaths$TSH_VB,  resultsData = simulatedScenario_T4$outputValues, lables = "TSH VB T4 administration simulated")
  plotMapping_TSH$addXYSeries(xValsList = observedData$Larsen_1977$TSH_T3_admin[[X_VALS_COL_NAME]],
                              yValsList = observedData$Larsen_1977$TSH_T3_admin[[Y_VALS_COL_NAME]],
                              yErrorList = observedData$Larsen_1977$TSH_T3_admin[[Y_ERROR_COL_NAME]],
                              lables = "TSH VB T3 administration data")
  plotMapping_TSH$addXYSeries(xValsList = observedData$Larsen_1977$TSH_T4_admin[[X_VALS_COL_NAME]],
                              yValsList = observedData$Larsen_1977$TSH_T4_admin[[Y_VALS_COL_NAME]],
                              yErrorList = observedData$Larsen_1977$TSH_T4_admin[[Y_ERROR_COL_NAME]],
                              lables = "TSH VB T4 administration data")
  plotMapping_TSH$setPlotConfiguration(plotMappingConfiguration)
  plotMapping_TSH$xLim <- c(0, 8)
  #  plotMapping$yLim <- c(0, 3)
  plotMapping_TSH$xLab <- "Time [h]"
  plotMapping_TSH$yLab <- "TSH VB [ng/ml]"
  plotMapping_TSH$log <- ""
  legend("topright", legend = c("T3 administration", "T4 administration"), col = c(allColors[[1]], allColors[[2]]),
         lty = c(1, 2), pch = c(1,2))
  
  plotMapping_T3 <- PlotMapping$new()
  plotMapping_T3$addModelOutputs(paths = OutputPaths$T3_VB,  resultsData = simulatedScenario_T3$outputValues, lables = "T3 VB T3 administration simulated")
  plotMapping_T3$addModelOutputs(paths = OutputPaths$T3_VB,  resultsData = simulatedScenario_T4$outputValues, lables = "T3 VB T4 administration simulated")
  plotMapping_T3$addXYSeries(xValsList = observedData$Larsen_1977$T3_T3_admin[[X_VALS_COL_NAME]],
                              yValsList = observedData$Larsen_1977$T3_T3_admin[[Y_VALS_COL_NAME]],
                              yErrorList = observedData$Larsen_1977$T3_T3_admin[[Y_ERROR_COL_NAME]],
                              lables = "T3 VB T3 administration data")
  plotMapping_T3$addXYSeries(xValsList = observedData$Larsen_1977$T3_T4_admin[[X_VALS_COL_NAME]],
                              yValsList = observedData$Larsen_1977$T3_T4_admin[[Y_VALS_COL_NAME]],
                              yErrorList = observedData$Larsen_1977$T3_T4_admin[[Y_ERROR_COL_NAME]],
                              lables = "T3 VB T4 administration data")
  plotMapping_T3$setPlotConfiguration(plotMappingConfiguration)
  plotMapping_T3$xLim <- c(0, 8)
  #  plotMapping$yLim <- c(0, 3)
  plotMapping_T3$xLab <- "Time [h]"
  plotMapping_T3$yLab <- "T3 VB [ng/ml]"
  plotMapping_T3$log <- ""
  legend("topright", legend = c("T3 administration", "T4 administration"), col = c(allColors[[1]], allColors[[2]]),
         lty = c(1, 2), pch = c(1,2))
  
  plotMapping_T4 <- PlotMapping$new()
  plotMapping_T4$addModelOutputs(paths = OutputPaths$T4_VB,  resultsData = simulatedScenario_T4$outputValues, lables = "T4 VB T4 administration simulated")
  plotMapping_T4$addXYSeries(xValsList = observedData$Larsen_1977$T4_T4_admin[[X_VALS_COL_NAME]],
                             yValsList = observedData$Larsen_1977$T4_T4_admin[[Y_VALS_COL_NAME]],
                             yErrorList = observedData$Larsen_1977$T4_T4_admin[[Y_ERROR_COL_NAME]],
                             lables = "T4 VB T4 administration data")
  plotMapping_T4$setPlotConfiguration(plotMappingConfiguration)
  plotMapping_T4$xLim <- c(0, 8)
  #  plotMapping$yLim <- c(0, 3)
  plotMapping_T4$xLab <- "Time [h]"
  plotMapping_T4$yLab <- "T4 VB [ng/ml]"
  plotMapping_T4$log <- ""
  
  plotConfiguration <- enum(list(
    outputToPNG = runConfiguration$outputToPNG,
    outputName = "Silva_1997_T3_iv_PD",
    outputPath = runConfiguration$outputPath,
    width = NULL,
    height = NULL,
    nrOfCols = NULL,
    res = 600,
    pointsize = 8,
    addTitle = TRUE
  ))
  
  plotMultiPanel(c(plotMapping_TSH, plotMapping_T3, plotMapping_T4), plotConfiguration)
  
  #######Concentrations in Thyroid and Pituitary
  ####T4 plotmapping
  # plotMappingT4_thyroid <- PlotMapping$new()
  # plotMappingT4_thyroid$addModelOutputs(paths = OutputPaths$T4_thyrInt, resultsData = simulatedScenario$outputValues, lables = "T4 thyroid simulated")
  # plotMappingT4_thyroid$setPlotConfiguration(plotMappingConfiguration)
  # plotMappingT4_thyroid$xLim <- c(0, 24)
  # plotMappingT4_thyroid$xLab = "Time [h]"
  # plotMappingT4_thyroid$yLab = "T4 thyroid [µmol/l]"
  # 
  # plotMappingT4_pituitary <- PlotMapping$new()
  # plotMappingT4_pituitary$addModelOutputs(paths = OutputPaths$T4_pitInt, resultsData = simulatedScenario$outputValues, lables = "T4 pituitary simulated")
  # plotMappingT4_pituitary$setPlotConfiguration(plotMappingConfiguration)
  # plotMappingT4_pituitary$xLim <- c(0, 24)
  # plotMappingT4_pituitary$xLab = "Time [h]"
  # plotMappingT4_pituitary$yLab = "T4 pituitary [µmol/l]"
  # 
  # ####T3 plotmapping
  # plotMappingT3_thyroid <- PlotMapping$new()
  # plotMappingT3_thyroid$addModelOutputs(paths = OutputPaths$T3_thyrInt, resultsData = simulatedScenario$outputValues, lables = "T3 thyroid simulated")
  # plotMappingT3_thyroid$setPlotConfiguration(plotMappingConfiguration)
  # plotMappingT3_thyroid$xLim <- c(0, 24)
  # plotMappingT3_thyroid$xLab = "Time [h]"
  # plotMappingT3_thyroid$yLab = "T4 thyroid [µmol/l]"
  # 
  # plotMappingT3_pituitary <- PlotMapping$new()
  # plotMappingT3_pituitary$addModelOutputs(paths = OutputPaths$T3_pitInt, resultsData = simulatedScenario$outputValues, lables = "T3 pituitary simulated")
  # plotMappingT3_pituitary$setPlotConfiguration(plotMappingConfiguration)
  # plotMappingT3_pituitary$xLim <- c(0, 24)
  # plotMappingT3_pituitary$xLab = "Time [h]"
  # plotMappingT3_pituitary$yLab = "T3 pituitary [µmol/l]"
  # 
  # ####TSH plotmapping
  # plotMappingTSH_thyroid <- PlotMapping$new()
  # plotMappingTSH_thyroid$addModelOutputs(paths = OutputPaths$TSH_thyrInt, resultsData = simulatedScenario$outputValues, lables = "T3 thyroid simulated")
  # plotMappingTSH_thyroid$setPlotConfiguration(plotMappingConfiguration)
  # plotMappingTSH_thyroid$xLim <- c(0, 24)
  # plotMappingTSH_thyroid$xLab = "Time [h]"
  # plotMappingTSH_thyroid$yLab = "TSH thyroid [µmol/l]"
  # 
  # plotMappingTSH_pituitary <- PlotMapping$new()
  # plotMappingTSH_pituitary$addModelOutputs(paths = OutputPaths$TSH_pitInt, resultsData = simulatedScenario$outputValues, lables = "T3 pituitary simulated")
  # plotMappingTSH_pituitary$setPlotConfiguration(plotMappingConfiguration)
  # plotMappingTSH_pituitary$xLim <- c(0, 24)
  # plotMappingTSH_pituitary$xLab = "Time [h]"
  # plotMappingTSH_pituitary$yLab = "TSH pituitary [µmol/l]"
  # 
  # plotConfiguration$outputName <- "Silva 1997 thyroid concentrations"
  # plotMultiPanel(c(plotMappingT4_thyroid, plotMappingT3_thyroid, plotMappingTSH_thyroid), plotConfiguration)
  # plotConfiguration$outputName <- "Silva 1997 pituitary concentrations"
  # plotMultiPanel(c(plotMappingTSH_pituitary, plotMappingT3_pituitary, plotMappingT4_pituitary), plotConfiguration)
}
