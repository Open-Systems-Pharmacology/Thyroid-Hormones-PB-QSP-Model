DataFileConfiguration <- enum(list(
  GROUP_COL_IDX = 1,
  TIME_COL_IDX = 2
))


readObservedData <- function(runConfiguration){
  observedData <- list()
  for (sheet in runConfiguration$dataSheets) {
    data <- read.xlsx(xlsxFile = runConfiguration$dataPath, sheet = sheet)
    data <- split(data, data$Group)
    observedData[[sheet]] <- data
  }
  
  return(observedData)
}
  
  
# 
# X_VALS_COL_NAME <- "Time.[h]"
# Y_VALS_COL_NAME <- "Concentration.[ng/mL]"
# OBSERVED_DATA_X_FACTOR <- 60
# #Moleular weight of the compound
# mw <- 30000
# OBSERVED_DATA_Y_FACTOR <- 1 / (1e-6 * mw * 1e6)
# SIMULATION_Y_FACTOR <- 1 / OBSERVED_DATA_Y_FACTOR
# observedData <- read.xlsx(runConfiguration$dataPath, sheet = DATA_SHEET)
# observedData$xVals <- (observedData[[X_VALS_COL_NAME]] - observedData[[X_VALS_COL_NAME]][[1]]) * OBSERVED_DATA_X_FACTOR
# observedData$yVals <- observedData[[Y_VALS_COL_NAME]] * OBSERVED_DATA_Y_FACTOR
# observedData <- split(observedData, observedData$Group)