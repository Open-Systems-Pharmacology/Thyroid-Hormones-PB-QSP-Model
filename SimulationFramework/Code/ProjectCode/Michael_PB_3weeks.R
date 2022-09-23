Michael_PB_3weeks <- function(runConfiguration){
  runConfiguration$scenarioName <- "Michael_Placebo"
  runConfiguration$simulationTime <- 3*7*24*60 + 5*24*60
  runConfiguration$steadyStateTime <- 2000 * 60
  runConfiguration$paramSheets <- c("Global", "Physiology_rat", "PI rat", "PI rat_tx", "Phenobarbital_rat")
  runConfiguration$individualCharacteristics <- NULL
  runConfiguration$dataSheets <- "Michael_1989"
  
  simulatedScenario <- runScenario(runConfiguration = runConfiguration)
  
  runConfiguration$scenarioName <- "Michael_PB_3weeks"
  simulatedScenario_treatment <- runScenario(runConfiguration = runConfiguration)
  
  observedData <- readObservedData(runConfiguration)
  
  #Factor for conversion of standard output units
  mwT4 <- 776.8
  convFacT4 <- 1e-6 * mwT4 * 1e6
  mwT3 <- 650.98
  convFacT3 <- 1e-6 * mwT3 * 1e6
  mwTSH <- 30000
  convFacTSH <- 1e-6 * mwTSH * 1e6
  convFacTSH_data = 1
  
  plotMappingConfiguration <- enum(list(
    xFac = list("T4 control simulated" = 1/60/24,
                "T4 PB simulated" = 1/60/24,
                "T4 control data" = 1/24,
                "T4 PB data" = 1/24
    ),
    
    yFac = list("T4 control simulated" = convFacT4,
                "T4 PB simulated" = convFacT4,
                "T4 control data" = 1e3,
                "T4 PB data" = 1e3
    ),
    
    xOffset = list(
      "T4 control simulated" = -(3*7*24*60),
      "T4 PB simulated" = -(3*7*24*60),
      "T4 control data" = -(20*24),
      "T4 PB data" = -(20*24)
    ),
    
    lineTypes = list()
  ))
  
  allColors <- esqLABS_colors(2)
  plotMappingConfiguration$colors <- list("T4 control simulated" = allColors[[1]],
                                          "T4 control data" = allColors[[1]],
                                          "T4 PB simulated" = allColors[[2]],
                                          "T4 PB data" = allColors[[2]]
                                          
  )
  
  #Create plot mappings
  X_VALS_COL_NAME <- "Time.[h]"
  Y_VALS_COL_NAME <- "Concentration.(mass).[µg/ml]"
  Y_ERROR_COL_NAME <- "Error.[µg/ml]"
  
  ####T4 plotmapping
  t4Mapping <- PlotMapping$new()
  t4Mapping$addModelOutputs(paths = OutputPaths$T4_VB,  resultsData = simulatedScenario$outputValues, lables = "T4 control simulated")
  t4Mapping$addModelOutputs(paths = OutputPaths$T4_VB,  resultsData = simulatedScenario_treatment$outputValues, lables = "T4 PB simulated")
  t4Mapping$addXYSeries(xValsList = observedData$Michael_1989$control[[X_VALS_COL_NAME]],
                        yValsList = observedData$Michael_1989$control[[Y_VALS_COL_NAME]],
                        yErrorList = observedData$Michael_1989$control[[Y_ERROR_COL_NAME]],
                        lables = "T4 control data")
  t4Mapping$addXYSeries(xValsList = observedData$Michael_1989$treatment[[X_VALS_COL_NAME]],
                        yValsList = observedData$Michael_1989$treatment[[Y_VALS_COL_NAME]],
                        yErrorList = observedData$Michael_1989$treatment[[Y_ERROR_COL_NAME]],
                        lables = "T4 PB data")
  t4Mapping$setPlotConfiguration(plotMappingConfiguration)
  t4Mapping$xLim <- c(0, 4)
  t4Mapping$log <- "y"
  t4Mapping$yLim <- c(0.1, 20)
  t4Mapping$xLab = "Time [days]"
  t4Mapping$yLab = "T4 PVB [ng/ml]"
 
  plotConfiguration <- enum(list(
    outputToPNG = runConfiguration$outputToPNG,
    outputName = "Michael_1989 rat",
    outputPath = runConfiguration$outputPath,
    width = NULL,
    height = NULL,
    nrOfCols = NULL,
    res = 600,
    pointsize = 8,
    addTitle = TRUE
  ))
  
  plotMultiPanel(c(t4Mapping), plotConfiguration)
}