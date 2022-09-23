Leonard_2016_Triclosan_po <- function(projectConfiguration){
  runConfiguration$scenarioName <- "Leonard_2016_Triclosan_po"
  runConfiguration$simulationTime <- 60 * 60
  runConfiguration$steadyStateTime <- 2000 * 60
  runConfiguration$paramSheets <- c("Global", "Physiology_rat", "PI rat", "disableEndogenousSecretion")
  runConfiguration$individualCharacteristics <- NULL
  runConfiguration$dataSheets <- "Leonard2016Fig5"
  
  simulatedScenario <- runScenario(runConfiguration)
  observedData <- readObservedData(runConfiguration)
  
  #Factor for conversion of standard output units
  mwT4 <- 776.8
  convFacT4 <- 1e-6 * mwT4 * 1e6
  mwT3 <- 650.98
  convFacT3 <- 1e-6 * mwT3 * 1e6
  mwTSH <- 30000
  convFacTSH <- 1e-6 * mwTSH * 1e6
  mwTriclosan <- 269.99
  convFacTriclosan <- 1e-6 * mwTriclosan * 1e6
  
  plotMappingConfiguration <- enum(list(
    xFac = list("Triclosan VB simulated" = 1/60
    ),
    
    yFac = list("Triclosan VB simulated" = convFacTriclosan
    ),
    
    xOffset = list(
    ),
    
    lineTypes = list()
  ))
  
  allColors <- esqLABS_colors(1)
  plotMappingConfiguration$colors <- list("Triclosan VB simulated" = allColors[[1]],
                                          "Triclosan data" = allColors[[1]]
  )
  
  #Create plot mappings
  X_VALS_COL_NAME <- "Time.[h]"
  Y_VALS_COL_NAME <- "Concentration.[ng/mL]"
  
  ####T3 plotmapping
  plotMapping <- PlotMapping$new()
  plotMapping$addModelOutputs(paths = OutputPaths$Perpetrator_VB,  resultsData = simulatedScenario$outputValues, lables = "Triclosan VB simulated")
  plotMapping$addXYSeries(xValsList = observedData$Leonard2016Fig5$PO_5mgkg_TCS_Rat_TCSblood[[X_VALS_COL_NAME]],
                          yValsList = observedData$Leonard2016Fig5$PO_5mgkg_TCS_Rat_TCSblood[[Y_VALS_COL_NAME]],
                          lables = "Triclosan data")
  plotMapping$setPlotConfiguration(plotMappingConfiguration)
  plotMapping$xLim <- c(0, 60)
  #  Pilo_1990_T3$yLim <- c(0, 3)
  plotMapping$xLab <- "Time [h]"
  plotMapping$yLab <- "Triclosan VB [ng/ml]"
  plotMapping$log <- ""
  
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
