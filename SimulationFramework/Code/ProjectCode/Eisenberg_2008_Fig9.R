Eisenberg_2008_Fig9 <- function(projectConfiguration){
  runConfiguration$scenarioName <- "Eisenberg2008Fig9"
  runConfiguration$simulationTime <- 360 * 60
  runConfiguration$steadyStateTime <- 2000 * 60
  runConfiguration$paramSheets <- c("Global", "PI human")
  runConfiguration$individualCharacteristics <- NULL
  runConfiguration$dataSheets <- "Eisenberg2008Fig9"
  
  simulatedScenario <- runScenario(runConfiguration = runConfiguration)
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
    xFac = list("T3 PVB simulated" = 1/60,
                "TSH PVB simulated" = 1/60,
                "T4 thyroid simulated" = 1/60,
                "T4 pituitary simulated" = 1/60,
                "T3 thyroid simulated" = 1/60,
                "T3 pituitary simulated" = 1/60,
                "TSH thyroid simulated" = 1/60,
                "TSH pituitary simulated" = 1/60
    ),
    
    yFac = list("T3 PVB simulated" = convFacT3,
                "TSH PVB simulated" = convFacTSH,
                "TSH data " = 1
    ),
    
    xOffset = list("T3 PVB simulated" = -240 * 60,
                   "T3 data" = -240,
                   "TSH PVB simulated" = -240 * 60,
                   "TSH data" = -240,
                   "T4 thyroid simulated" = -240 * 60,
                   "T4 pituitary simulated" = -240 * 60,
                   "T3 thyroid simulated" = -240 * 60,
                   "T3 pituitary simulated" = -240 * 60,
                   "TSH thyroid simulated" = -240 * 60,
                   "TSH pituitary simulated" = -240 * 60
    ),
    
    lineTypes = list()
  ))
  
  allColors <- esqLABS_colors(1)
  plotMappingConfiguration$colors <- list("T3 PVB simulated" = allColors[[1]],
                                          "T3 data" = allColors[[1]],
                                          "TSH PVB simulated" = allColors[[1]],
                                          "TSH data" = allColors[[1]]
  )
  
  #Create plot mappings
  X_VALS_COL_NAME <- "Time.[h]"
  Y_VALS_COL_NAME <- "Concentration.[ng/mL]"
  ####T3 plotmapping
  t3Mapping <- PlotMapping$new()
  t3Mapping$addModelOutputs(paths = OutputPaths$T3_PVB,  resultsData = simulatedScenario$outputValues, lables = "T3 PVB simulated")
  t3Mapping$addXYSeries(xValsList = observedData$Eisenberg2008Fig9$PO_75ugT3_Human_T3[[X_VALS_COL_NAME]],
                                   yValsList = observedData$Eisenberg2008Fig9$PO_75ugT3_Human_T3[[Y_VALS_COL_NAME]],
                                   lables = "T3 data")
  t3Mapping$setPlotConfiguration(plotMappingConfiguration)
  t3Mapping$xLim <- c(0, 24)
  t3Mapping$yLim <- c(0, 7)
  t3Mapping$xLab = "Time [h]"
  t3Mapping$yLab = "T3 PVB [ng/ml]"
  
  ####TSH plotmapping
  tshMapping <- PlotMapping$new()
  tshMapping$addModelOutputs(paths = OutputPaths$TSH_PVB,  resultsData = simulatedScenario$outputValues, lables = "TSH PVB simulated")
  tshMapping$addXYSeries(xValsList = observedData$Eisenberg2008Fig9$PO_75ugT3_Human_TSH[[X_VALS_COL_NAME]],
                                    yValsList = observedData$Eisenberg2008Fig9$PO_75ugT3_Human_TSH[[Y_VALS_COL_NAME]],
                                    lables = "TSH data")
  tshMapping$setPlotConfiguration(plotMappingConfiguration)
  tshMapping$xLim <- c(0, 24)
  tshMapping$yLim <- c(0, 0.05)
  tshMapping$xLab = "Time [h]"
  tshMapping$yLab = "TSH PVB [ng/ml]"

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
  
  plotMultiPanel(c(t3Mapping, tshMapping), plotConfiguration)
  
  #######Concentrations in Thyroid and Pituitary
  ####T4 plotmapping
  plotMappingT4_thyroid <- PlotMapping$new()
  plotMappingT4_thyroid$addModelOutputs(paths = OutputPaths$T4_thyrInt, resultsData = simulatedScenario$outputValues, lables = "T4 thyroid simulated")
  plotMappingT4_thyroid$setPlotConfiguration(plotMappingConfiguration)
  plotMappingT4_thyroid$xLim <- c(0, 120)
  plotMappingT4_thyroid$xLab = "Time [h]"
  plotMappingT4_thyroid$yLab = "T4 thyroid [µmol/l]"
  
  plotMappingT4_pituitary <- PlotMapping$new()
  plotMappingT4_pituitary$addModelOutputs(paths = OutputPaths$T4_pitInt, resultsData = simulatedScenario$outputValues, lables = "T4 pituitary simulated")
  plotMappingT4_pituitary$setPlotConfiguration(plotMappingConfiguration)
  plotMappingT4_pituitary$xLim <- c(0, 120)
  plotMappingT4_pituitary$xLab = "Time [h]"
  plotMappingT4_pituitary$yLab = "T4 pituitary [µmol/l]"
  
  ####T3 plotmapping
  plotMappingT3_thyroid <- PlotMapping$new()
  plotMappingT3_thyroid$addModelOutputs(paths = OutputPaths$T3_thyrInt, resultsData = simulatedScenario$outputValues, lables = "T3 thyroid simulated")
  plotMappingT3_thyroid$setPlotConfiguration(plotMappingConfiguration)
  plotMappingT3_thyroid$xLim <- c(0, 120)
  plotMappingT3_thyroid$xLab = "Time [h]"
  plotMappingT3_thyroid$yLab = "T3 thyroid [µmol/l]"
  
  plotMappingT3_pituitary <- PlotMapping$new()
  plotMappingT3_pituitary$addModelOutputs(paths = OutputPaths$T3_pitInt, resultsData = simulatedScenario$outputValues, lables = "T3 pituitary simulated")
  plotMappingT3_pituitary$setPlotConfiguration(plotMappingConfiguration)
  plotMappingT3_pituitary$xLim <- c(0, 120)
  plotMappingT3_pituitary$xLab = "Time [h]"
  plotMappingT3_pituitary$yLab = "T3 pituitary [µmol/l]"
  
  ####TSH plotmapping
  plotMappingTSH_thyroid <- PlotMapping$new()
  plotMappingTSH_thyroid$addModelOutputs(paths = OutputPaths$TSH_thyrInt, resultsData = simulatedScenario$outputValues, lables = "T3 thyroid simulated")
  plotMappingTSH_thyroid$setPlotConfiguration(plotMappingConfiguration)
  plotMappingTSH_thyroid$xLim <- c(0, 120)
  plotMappingTSH_thyroid$xLab = "Time [h]"
  plotMappingTSH_thyroid$yLab = "TSH thyroid [µmol/l]"
  
  plotMappingTSH_pituitary <- PlotMapping$new()
  plotMappingTSH_pituitary$addModelOutputs(paths = OutputPaths$TSH_pitInt, resultsData = simulatedScenario$outputValues, lables = "T3 pituitary simulated")
  plotMappingTSH_pituitary$setPlotConfiguration(plotMappingConfiguration)
  plotMappingTSH_pituitary$xLim <- c(0, 120)
  plotMappingTSH_pituitary$xLab = "Time [h]"
  plotMappingTSH_pituitary$yLab = "TSH pituitary [µmol/l]"
  
  plotConfiguration$outputName <- "Eisenberg Fig 9 thyroid concentrations"
 # plotMultiPanel(c(plotMappingT4_thyroid, plotMappingT3_thyroid, plotMappingTSH_thyroid), plotConfiguration)
  plotConfiguration$outputName <- "Eisenberg Fig 9 pituitary concentrations"
 # plotMultiPanel(c(plotMappingT4_pituitary, plotMappingT3_pituitary, plotMappingTSH_pituitary), plotConfiguration)
}