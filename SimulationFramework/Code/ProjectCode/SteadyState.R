SteadyState_rat <- function(runConfiguration, export = FALSE){
  runConfiguration$scenarioName <- "SteadyState_rat"
  runConfiguration$simulationTime <- 300 * 60
  runConfiguration$steadyStateTime <- 800 * 60
  runConfiguration$paramSheets <- c("Global", "Physiology_rat", "PI rat")
  runConfiguration$individualCharacteristics <- NULL
  runConfiguration$dataSheets <- c("DiStefano1982baselineT3&T4", "Tuomisto1975baselineTSH")
  
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
    xFac = list("TSH VB simulated" = 1/60,
                "T4 thyroid simulated" = 1/60,
                "T4 pituitary simulated" = 1/60,
                "T3 VB simulated" = 1/60,
                "T3 thyroid simulated" = 1/60,
                "T3 pituitary simulated" = 1/60,
                "T4 VB simulated" = 1/60,
                "T4 thyroid simulated" = 1/60,
                "T4 pituitary simulated" = 1/60
    ),
    
    yFac = list("TSH VB simulated" = convFacTSH,
                "T3 VB simulated" = convFacT3,
                "T4 VB simulated" = convFacT4
    ),
    
    xOffset = list(
      "TSH VB simulated" = -200*60,
      "T4 thyroid simulated" = -200*60,
      "T4 pituitary simulated" = -200*60,
      "T3 VB simulated" = -200*60,
      "T3 thyroid simulated" = -200*60,
      "T3 pituitary simulated" = -200*60,
      "T4 VB simulated" = -200*60,
      "T4 thyroid simulated" = -200*60,
      "T4 pituitary simulated" = -200*60,
      "TSH data" = -240,
      "T4 data" = -240,
      "T3 data" = -240
    ),
    
    lineTypes = list()
  ))
  
  allColors <- esqLABS_colors(1)
  plotMappingConfiguration$colors <- list("TSH VB simulated" = allColors[[1]],
                                          "TSH data" = allColors[[1]]
  )
  
  #Create plot mappings
  X_VALS_COL_NAME <- "Time.[h]"
  Y_VALS_COL_NAME <- "Concentration.[ng/mL]"
  Y_ERROR_COL_NAME <- "ERROR.[ng/mL]"
  
  
  plotMapping_TSH <- PlotMapping$new()
  plotMapping_TSH$addModelOutputs(paths = OutputPaths$TSH_VB,  resultsData = simulatedScenario$outputValues, lables = "TSH VB simulated")
  plotMapping_TSH$addXYSeries(xValsList = observedData$Tuomisto1975baselineTSH$`IV_Rat_TSH-baseline`[[X_VALS_COL_NAME]],
                              yValsList = observedData$Tuomisto1975baselineTSH$`IV_Rat_TSH-baseline`[[Y_VALS_COL_NAME]],
                              yErrorList = observedData$Tuomisto1975baselineTSH$`IV_Rat_TSH-baseline`[[Y_ERROR_COL_NAME]],
                              lables = "TSH data")
  plotMapping_TSH$setPlotConfiguration(plotMappingConfiguration)
  plotMapping_TSH$xLim <- c(60, 80)
#  plotMapping_TSH$yLim <- c(0, 5)
  plotMapping_TSH$xLab <- "Time [h]"
  plotMapping_TSH$yLab <- "TSH VB [ng/ml]"
  plotMapping_TSH$log <- ""
  
  plotMapping_T3 <- PlotMapping$new()
  plotMapping_T3$addModelOutputs(paths = OutputPaths$T3_VB,  resultsData = simulatedScenario$outputValues, lables = "T3 VB simulated")
  plotMapping_T3$addXYSeries(xValsList = observedData$`DiStefano1982baselineT3&T4`$`IV_Rat_T3-baseline`[[X_VALS_COL_NAME]],
                              yValsList = observedData$`DiStefano1982baselineT3&T4`$`IV_Rat_T3-baseline`[[Y_VALS_COL_NAME]],
                              yErrorList = observedData$`DiStefano1982baselineT3&T4`$`IV_Rat_T3-baseline`[[Y_ERROR_COL_NAME]],
                              lables = "T3 data")
  plotMapping_T3$setPlotConfiguration(plotMappingConfiguration)

  plotMapping_T3$xLim <- c(60, 80)
  #  plotMapping$yLim <- c(0, 3)
  plotMapping_T3$xLab <- "Time [h]"
  plotMapping_T3$yLab <- "T3 VB [ng/ml]"
  plotMapping_T3$log <- ""
  
  plotMapping_T4 <- PlotMapping$new()
  plotMapping_T4$addModelOutputs(paths = OutputPaths$T4_VB,  resultsData = simulatedScenario$outputValues, lables = "T4 VB simulated")
  plotMapping_T4$addXYSeries(xValsList = observedData$`DiStefano1982baselineT3&T4`$`IV_Rat_T4-baseline`[[X_VALS_COL_NAME]],
                              yValsList = observedData$`DiStefano1982baselineT3&T4`$`IV_Rat_T4-baseline`[[Y_VALS_COL_NAME]],
                              yErrorList = observedData$`DiStefano1982baselineT3&T4`$`IV_Rat_T4-baseline`[[Y_ERROR_COL_NAME]],
                              lables = "T4 data")
  plotMapping_T4$setPlotConfiguration(plotMappingConfiguration)
  plotMapping_T4$xLim <- c(60, 80)
  #  plotMapping$yLim <- c(0, 3)
  plotMapping_T4$xLab <- "Time [h]"
  plotMapping_T4$yLab <- "T4 VB [ng/ml]"
  plotMapping_T4$log <- ""
  
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

  plotMapping_M_TSH <- PlotMapping$new()
  plotMapping_M_TSH$addModelOutputs(paths = OutputPaths$M_TSH_T4_sec, lables = "M_TSH_T4_sec", resultsData = simulatedScenario$outputValues)
  plotMapping_M_TSH$xLab <- "Time [min]"
  plotMapping_M_TSH$yLab <- "M_TSH_T4_syn"
  
  plotMultiPanel(c(plotMapping_TSH, plotMapping_T3, plotMapping_T4), plotConfiguration)
  if (export) 
    esqlabsR::exportSteadyStateToXLS(simulation = simulatedScenario$simulation, steadyStateTime = runConfiguration$steadyStateTime)
}

SteadyState_ratTx <- function(runConfiguration){
  runConfiguration$scenarioName <- "SteadyState_ratTx"
  runConfiguration$simulationTime <- 100 * 60
  runConfiguration$steadyStateTime <- 800 * 60
  runConfiguration$paramSheets <- c("Global", "Physiology_rat", "PI rat", "PI rat_tx")
  runConfiguration$individualCharacteristics <- NULL
  
  simulatedScenario <- runScenario(runConfiguration = runConfiguration)
  esqlabsR::exportSteadyStateToXLS(simulation = simulatedScenario$simulation, steadyStateTime = runConfiguration$steadyStateTime)
}

SteadyState_Human <- function(runConfiguration){
  runConfiguration$scenarioName <- "SteadyState_Human"
  runConfiguration$simulationTime <- 100 * 60
  runConfiguration$steadyStateTime <- 800 * 60
  runConfiguration$paramSheets <- c("Global", "PI human")
  runConfiguration$individualCharacteristics <- NULL
  
  simulatedScenario <- runScenario(runConfiguration = runConfiguration)
  esqlabsR::exportSteadyStateToXLS(simulation = simulatedScenario$simulation, steadyStateTime = runConfiguration$steadyStateTime)
}