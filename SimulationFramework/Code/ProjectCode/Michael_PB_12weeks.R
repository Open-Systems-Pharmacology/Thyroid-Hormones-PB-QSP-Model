Michael_PB_2weeks <- function(runConfiguration){
  runConfiguration$scenarioName <- "Michael_Placebo 2weeks"
  runConfiguration$simulationTime <- 2*7*24*60
  runConfiguration$steadyStateTime <- 2000 * 60
  runConfiguration$paramSheets <- c("Global", "Physiology_rat", "PI rat", "Phenobarbital_rat")
  runConfiguration$individualCharacteristics <- NULL
#  runConfiguration$dataSheets <- "Michael_1989"
  
  simulatedScenario <- runScenario(runConfiguration = runConfiguration)
  
  runConfiguration$scenarioName <- "Michael_PB 2weeks"
  simulatedScenario_treatment <- runScenario(runConfiguration = runConfiguration)
  
  #Observed change in %
  observedT3 <- (107 - 114) / 114 * 100
  observedT4 <- (2.9 - 4.2) / 4.2 * 100
  observedTSH <- (3.8 - 1.6) / 1.6 * 100
  
  lastIdx <- length(simulatedScenario_treatment$outputValues$data$Time)
  simulatedT3 <- (1 - (simulatedScenario_treatment$outputValues$data[[OutputPaths$T3_VB]][[lastIdx]] / 
                         simulatedScenario$outputValues$data[[OutputPaths$T3_VB]][[lastIdx]])) * 100
  
  simulatedT4 <- (1 - (simulatedScenario_treatment$outputValues$data[[OutputPaths$T4_VB]][[lastIdx]] / 
                         simulatedScenario$outputValues$data[[OutputPaths$T4_VB]][[lastIdx]])) * 100
  
  simulatedTSH <- (1 - (simulatedScenario_treatment$outputValues$data[[OutputPaths$TSH_VB]][[lastIdx]] / 
                         simulatedScenario$outputValues$data[[OutputPaths$TSH_VB]][[lastIdx]])) * 100

  print("Changes after 2 weeks PB treatment (observed vs. simulated:")
  print(paste0("T4: ", observedT4, " vs ", -simulatedT4))
  print(paste0("T3: ", observedT3, " vs ", -simulatedT3))
  print(paste0("TSH: ", observedTSH, " vs ", -simulatedTSH))
  
}