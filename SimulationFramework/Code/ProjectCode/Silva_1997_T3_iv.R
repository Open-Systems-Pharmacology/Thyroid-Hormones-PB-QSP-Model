Silva_1997_T3_iv <- function(runConfiguration){
  runConfiguration$scenarioName <- "Silva_1997_T3_iv"
  runConfiguration$simulationTime <- 264 * 60
  runConfiguration$steadyStateTime <- 800 * 60
  runConfiguration$paramSheets <- c("Global", "Physiology_rat", "PI rat", "disableEndogenousSecretion")
  runConfiguration$individualCharacteristics <- NULL
  runConfiguration$dataSheets <- "Silva1977Fig1"
  
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
    xFac = list("T3 VB simulated" = 1/60,
                "T3 pituitary cell simulated" = 1/60
    ),
    
    yFac = list("T3 VB simulated" = convFacT3,
                "T3 pituitary cell simulated" = convFacT3
    ),
    
    xOffset = list(
      "T3 VB data" = -240,
      "T3 pituitary data" = -240
    ),
    
    lineTypes = list()
  ))
  
  allColors <- esqLABS_colors(2)
  plotMappingConfiguration$colors <- list("T3 VB simulated" = allColors[[1]],
                                          "T3 pituitary cell simulated" = allColors[[2]],
                                          "T3 VB data" = allColors[[1]],
                                          "T3 pituitary data" = allColors[[2]]
  )
  
  #Create plot mappings
  X_VALS_COL_NAME <- "Time.[h]"
  Y_VALS_COL_NAME <- "Concentration.[ng/mL]"
  Y_ERROR_COL_NAME <- "ERROR.[ng/mL]"
  
  ####T3 plotmapping
  plotMapping <- PlotMapping$new()
  plotMapping$addModelOutputs(paths = OutputPaths$T3_VB,  resultsData = simulatedScenario$outputValues, lables = "T3 VB simulated")
  plotMapping$addModelOutputs(paths = OutputPaths$T3_pitCell,  resultsData = simulatedScenario$outputValues, lables = "T3 pituitary cell simulated")
  plotMapping$addXYSeries(xValsList = observedData$Silva1977Fig1$IV_70ng100g_125T3_Rat_T3[[X_VALS_COL_NAME]],
                           yValsList = observedData$Silva1977Fig1$IV_70ng100g_125T3_Rat_T3[[Y_VALS_COL_NAME]],
                          yErrorList = observedData$Silva1977Fig1$IV_70ng100g_125T3_Rat_T3[[Y_ERROR_COL_NAME]],
                           lables = "T3 VB data")
  plotMapping$addXYSeries(xValsList = observedData$Silva1977Fig1$IV_70ng100g_125T3_Rat_T3_nuclear[[X_VALS_COL_NAME]],
                          yValsList = observedData$Silva1977Fig1$IV_70ng100g_125T3_Rat_T3_nuclear[[Y_VALS_COL_NAME]],
                          yErrorList = observedData$Silva1977Fig1$IV_70ng100g_125T3_Rat_T3_nuclear[[Y_ERROR_COL_NAME]],
                          lables = "T3 pituitary data")
  plotMapping$setPlotConfiguration(plotMappingConfiguration)
  plotMapping$xLim <- c(0, 8)
  plotMapping$yLim <- c(0.2, 3)
  plotMapping$xLab <- "Time [h]"
  plotMapping$yLab <- "T3 VB [ng/ml]"
  plotMapping$log <- "y"
  
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
  
  plotMultiPanel(plotMapping, plotConfiguration)
}
