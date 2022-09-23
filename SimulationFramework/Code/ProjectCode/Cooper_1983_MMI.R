Cooper_1983_MMI <- function(projectConfiguration, saveSimulation = FALSE){
  runConfiguration$scenarioName <- "Cooper_1983_MMI"
  runConfiguration$simulationTime <- 10*24*60
  runConfiguration$steadyStateTime <- 800 * 60
  runConfiguration$paramSheets <- c("Global", "Physiology_rat", "PI rat", "MMI_rat")
  runConfiguration$individualCharacteristics <- NULL
  runConfiguration$dataSheets <- "Cooper_1984b_PD"
  
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
  
  if (saveSimulation){
    refSim <- runScenario(simulationDose[[length(simulationDose)]], runConfiguration)
    saveSimulation(refSim$simulation, paste0("../", runConfiguration$scenarioName, ".pkml"))
  }
  
  mwT4 <- 776.8
  convFacT4 <- 1e-6 * mwT4 * 1e6 * 1
  mwT3 <- 650.98
  convFacT3 <- 1e-6 * mwT3 * 1e6 * 1
  mwTSH <- 30000
  convFacTSH <- 1e-6 * mwTSH * 1e6 * 1
  observedT3 <- c(
    0.000337255,
    0.000359664,
    0.000286275,
    0.000112045,
    6.33053E-05,
    5.9944E-05)
  errorT3 <- c(2.52101E-05,
               2.80112E-05,
               3.41737E-05,
               6.16246E-06,
               8.23529E-05,
               1.17647E-05
  )
  observedT4 <- c(
    0.02191944,
    0.022605954,
    0.016819615,
    0.005492119,
    0.003579685,
    0.00137303)
  errorT4 <- c(0.00152014,
               0.001814361,
               0.001569177,
               0.000833625,
               0.000539405,
               0.000196147
  )
  observedTSH <- c(
    0.003171937,
    0.004901186,
    0.006531621,
    0.008527668,
    0.008725296,
    0.007470356)
  errorTSH <- c(0.00083004,
                0.000750988,
                0.000839921,
                0.000662055,
                0.000800395,
                0.000602767
  )
  
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
                              yValsList = observedTSH * 1e3,
                              yErrorList = errorTSH * 1e3,
                              lables = "TSH data")
  plotMapping_TSH$addXYSeries(xValsList = simulationDose,
                              yValsList = simulatedTSH,
                              lables = "TSH simulated")
  plotMapping_TSH$addXYSeries(xValsList = simulationDose,
                              yValsList = rep(0.0025 * 1e3,
                                              times = length(simulationDose)),
                              lables = "TSH baseline")
  plotMapping_TSH$xLab <- "Dose [% in drinking water]"
  plotMapping_TSH$yLab <- "TSH [ng/ml]"
  plotMapping_TSH$log <- "x"
  plotMapping_TSH$setColors(lables = "TSH baseline", colors = "grey")
  plotMapping_TSH$setTypes(lables = "TSH baseline", types = "l")
  plotMapping_TSH$setTypes(lables = "TSH simulated", types = "b")
  
  plotMapping_T3 <- PlotMapping$new()
  plotMapping_T3$addXYSeries(xValsList = simulationDose,
                             yValsList = observedT3 * 1e3,
                             yErrorList = errorT3 * 1e3,
                             lables = "T3 data")
  plotMapping_T3$addXYSeries(xValsList = simulationDose,
                             yValsList = simulatedT3,
                             lables = "T3 simulated")
  plotMapping_T3$addXYSeries(xValsList = simulationDose,
                             yValsList = rep(0.0004 * 1e3,
                                             times = length(simulationDose)),
                             lables = "T3 baseline")
  plotMapping_T3$xLab <- "Dose [% in drinking water]"
  plotMapping_T3$yLab <- "T3 [ng/ml]"
  plotMapping_T3$log <- "x"
  plotMapping_T3$setColors(lables = "T3 baseline", colors = "grey")
  plotMapping_T3$setTypes(lables = "T3 baseline", types = "l")
  plotMapping_T3$setTypes(lables = "T3 simulated", types = "b")
  
  plotMapping_T4 <- PlotMapping$new()
  plotMapping_T4$addXYSeries(xValsList = simulationDose,
                             yValsList = observedT4 * 1e3,
                             yErrorList = errorT4 * 1e3,
                             lables = "T4 data")
  plotMapping_T4$addXYSeries(xValsList = simulationDose,
                             yValsList = simulatedT4,
                             lables = "T4 simulated")
  plotMapping_T4$addXYSeries(xValsList = simulationDose,
                             yValsList = rep(0.028 * 1e3,
                                             times = length(simulationDose)),
                             lables = "T4 baseline")
  plotMapping_T4$xLab <- "Dose [% in drinking water]"
  plotMapping_T4$yLab <- "T4 [ng/ml]"
  plotMapping_T4$log <- "x"
  plotMapping_T4$setColors(lables = "T4 baseline", colors = "grey")
  plotMapping_T4$setTypes(lables = "T4 baseline", types = "l")
  plotMapping_T4$setTypes(lables = "T4 simulated", types = "b")
  
  plotConfiguration <- enum(list(
    outputToPNG = runConfiguration$outputToPNG,
    outputName = "Cooper_1983_MMI",
    outputPath = runConfiguration$outputPath,
    width = NULL,
    height = 9,
    nrOfCols = NULL,
    res = 600,
    pointsize = 12,
    addTitle = FALSE
  ))
  
  plotConfiguration$outputName <- "Cooper_1983_MMI_T4"
  plotMultiPanel(plotMapping_T4, plotConfiguration)
  plotConfiguration$outputName <- "Cooper_1983_MMI_T3"
  plotMultiPanel(plotMapping_T3, plotConfiguration)
  plotConfiguration$outputName <- "Cooper_1983_MMI_TSH"
  plotMultiPanel(plotMapping_TSH, plotConfiguration)
  
}
