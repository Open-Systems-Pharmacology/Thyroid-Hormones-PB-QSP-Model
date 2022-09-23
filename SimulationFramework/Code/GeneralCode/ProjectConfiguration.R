projectConfiguration <- function() {
  modelFolder <- file.path(getwd(), "../Models/Simulations")
  modelFile <- "Thyroid_QSP.pkml"
  paramsFolder <- file.path(getwd(), "../Parameters")
  paramsFile <- "Thyroid_QSP_parameters.xlsx"
  dataFolder <- file.path(getwd(), "../Data")
  dataFile <- "CompiledDataSet.xlsx"
  outputFolder <- file.path(getwd(), "../Results")
  
  modelPath <- file.path(modelFolder, modelFile)
  paramsPath <- file.path(paramsFolder, paramsFile)
  dataPath <- file.path(dataFolder, dataFile)

  projectConfiguration <- enum(list(
    modelPath = file.path(modelFolder, modelFile),
    paramsPath = file.path(paramsFolder, paramsFile),
    outputPath = outputFolder <- file.path(getwd(), "../Results"),
    dataPath = dataPath <- file.path(dataFolder, dataFile)
  ))
}
