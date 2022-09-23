Eisenberg_2008_Fig45_400ug <- function(runConfiguration){
  runConfiguration$scenarioName <- "Eisenberg2008Fig45_400ug"
  runConfiguration$simulationTime <- 360 * 60
  runConfiguration$steadyStateTime <- 2000 * 60
  runConfiguration$paramSheets <- c("Global", "PI human")
  runConfiguration$individualCharacteristics <- NULL
  runConfiguration$dataSheets <- c("Eisenberg2008Fig45", "Eisenberg2008Fig8")
  
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
  xFac = list("T4 PVB simulated" = 1/60,
              "T4 thyroid simulated" = 1/60,
              "T4 pituitary simulated" = 1/60,
              "T3 PVB simulated" = 1/60,
              "T3 thyroid simulated" = 1/60,
              "T3 pituitary simulated" = 1/60,
              "TSH PVB simulated" = 1/60,
              "TSH thyroid simulated" = 1/60,
              "TSH pituitary simulated" = 1/60
               ),
  
  yFac = list("T4 PVB simulated" = convFacT4,
              "T3 PVB simulated" = convFacT3,
               "TSH PVB simulated" = convFacTSH
               ),
  
  xOffset = list("T4 PVB simulated" = -240 * 60,
                 "T4 thyroid simulated" = -240 * 60,
                 "T4 pituitary simulated" = -240 * 60,
                 "T4 data" = -240,
                 "T3 PVB simulated" = -240 * 60,
                 "T3 thyroid simulated" = -240 * 60,
                 "T3 pituitary simulated" = -240 * 60,
                 "T3 data" = -240,
                 "TSH PVB simulated" = -240 * 60,
                 "TSH thyroid simulated" = -240 * 60,
                 "TSH pituitary simulated" = -240 * 60,
                 "TSH data fig 5" = -240,
                 "TSH data fig 8 1" = -240,
                 "TSH data fig 8 2" = -240,
                 "TSH data fig 8 3" = -240,
                 "TSH data fig 8 4" = -240
                 ),
  
  lineTypes = list()
  ))
  
  allColors <- esqLABS_colors(1)
  plotMappingConfiguration$colors <- list("T4 PVB simulated" = allColors[[1]],
                     "T4 data" = allColors[[1]],
                 "T3 PVB simulated" = allColors[[1]],
                 "T3 data" = allColors[[1]],
                 "TSH PVB simulated" = allColors[[1]],
                 "TSH data fig 5" = allColors[[1]]
                 )
  
  #Create plot mappings
  X_VALS_COL_NAME <- "time.[h]"
  Y_VALS_COL_NAME <- "Concentration.[ng/mL]"
  ####T4 plotmapping
  eisenberg2008Fig4_T4 <- PlotMapping$new()
  eisenberg2008Fig4_T4$addModelOutputs(paths = OutputPaths$T4_PVB,  resultsData = simulatedScenario$outputValues, lables = "T4 PVB simulated")
  eisenberg2008Fig4_T4$addXYSeries(xValsList = observedData$Eisenberg2008Fig45$IV_400ugT4_Human_T4[[X_VALS_COL_NAME]],
                              yValsList = observedData$Eisenberg2008Fig45$IV_400ugT4_Human_T4[[Y_VALS_COL_NAME]],
                              lables = "T4 data")
  eisenberg2008Fig4_T4$setPlotConfiguration(plotMappingConfiguration)
  eisenberg2008Fig4_T4$xLim <- c(0, 120)
  eisenberg2008Fig4_T4$xLab = "Time [h]"
  eisenberg2008Fig4_T4$yLab = "T4 PVB [ng/ml]"

  ####T3 plotmapping
  eisenberg2008Fig4_T3 <- PlotMapping$new()
  eisenberg2008Fig4_T3$addModelOutputs(paths = OutputPaths$T3_PVB,  resultsData = simulatedScenario$outputValues, lables = "T3 PVB simulated")
  eisenberg2008Fig4_T3$addXYSeries(xValsList = observedData$Eisenberg2008Fig45$IV_400ugT4_Human_T3[[X_VALS_COL_NAME]],
                                   yValsList = observedData$Eisenberg2008Fig45$IV_400ugT4_Human_T3[[Y_VALS_COL_NAME]],
                                   lables = "T3 data")
  eisenberg2008Fig4_T3$setPlotConfiguration(plotMappingConfiguration)
  eisenberg2008Fig4_T3$xLim <- c(0, 120)
  eisenberg2008Fig4_T3$yLim <- c(0, 3)
  eisenberg2008Fig4_T3$xLab = "Time [h]"
  eisenberg2008Fig4_T3$yLab = "T3 PVB [ng/ml]"
  
  ####TSH plotmapping
  eisenberg2008Fig4_TSH <- PlotMapping$new()
  eisenberg2008Fig4_TSH$addModelOutputs(paths = OutputPaths$TSH_PVB,  resultsData = simulatedScenario$outputValues, lables = "TSH PVB simulated")
  eisenberg2008Fig4_TSH$addXYSeries(xValsList = observedData$Eisenberg2008Fig45$IV_400ugT4_Human_TSH[[X_VALS_COL_NAME]],
                                    yValsList = observedData$Eisenberg2008Fig45$IV_400ugT4_Human_TSH[[Y_VALS_COL_NAME]],
                                    lables = "TSH data fig 5")
  eisenberg2008Fig4_TSH$setPlotConfiguration(plotMappingConfiguration)
  eisenberg2008Fig4_TSH$xLim <- c(0, 120)
  eisenberg2008Fig4_TSH$yLim <- c(0, 0.1)
  eisenberg2008Fig4_TSH$xLab = "Time [h]"
  eisenberg2008Fig4_TSH$yLab = "TSH PVB [ng/ml]"
  
  ####TSH plotmapping basal
  X_VALS_COL_NAME <- "Time.[h]"
  Y_VALS_COL_NAME <- "Concentration.[ng/mL]"
  eisenberg2008Fig8_TSH <- PlotMapping$new()
  eisenberg2008Fig8_TSH$addModelOutputs(paths = OutputPaths$TSH_PVB,  resultsData = simulatedScenario$outputValues, lables = "TSH PVB simulated")
  eisenberg2008Fig8_TSH$addXYSeries(xValsList = list(observedData$Eisenberg2008Fig8$Blakesley2004_Rectangles[[X_VALS_COL_NAME]],
                                                    observedData$Eisenberg2008Fig8$Samuels1993_Diamonds[[X_VALS_COL_NAME]],
                                                    observedData$Eisenberg2008Fig8$Samuels1993_Triangles[[X_VALS_COL_NAME]],
                                                    observedData$Eisenberg2008Fig8$Samuels1994_Circles[[X_VALS_COL_NAME]]),
                                   yValsList = list(observedData$Eisenberg2008Fig8$Blakesley2004_Rectangles[[Y_VALS_COL_NAME]],
                                                    observedData$Eisenberg2008Fig8$Samuels1993_Diamonds[[Y_VALS_COL_NAME]],
                                                    observedData$Eisenberg2008Fig8$Samuels1993_Triangles[[Y_VALS_COL_NAME]],
                                                    observedData$Eisenberg2008Fig8$Samuels1994_Circles[[Y_VALS_COL_NAME]]),
                                   lables = list("TSH data fig 8 1", "TSH data fig 8 2", "TSH data fig 8 3", "TSH data fig 8 4"))
  eisenberg2008Fig8_TSH$setPlotConfiguration(plotMappingConfiguration)
  eisenberg2008Fig8_TSH$xLim <- c(0, 24)
  eisenberg2008Fig8_TSH$yLim <- c(0, 0.15)
  eisenberg2008Fig8_TSH$xLab = "Time [h]"
  eisenberg2008Fig8_TSH$yLab = "TSH PVB [ng/ml]"
  
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

  plotMultiPanel(c(eisenberg2008Fig4_T4, eisenberg2008Fig4_T3, eisenberg2008Fig4_TSH, eisenberg2008Fig8_TSH), plotConfiguration)
  
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
  
  plotConfiguration$outputName <- "Eisenberg 400µg thyroid concentrations"
#  plotMultiPanel(c(plotMappingT4_thyroid, plotMappingT3_thyroid, plotMappingTSH_thyroid), plotConfiguration)
  plotConfiguration$outputName <- "Eisenberg 400µg pituitary concentrations"
 # plotMultiPanel(c(plotMappingT4_pituitary, plotMappingT3_pituitary, plotMappingTSH_pituitary), plotConfiguration)
}