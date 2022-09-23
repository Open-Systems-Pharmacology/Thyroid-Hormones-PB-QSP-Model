Human_PB_2Weeks <- function(runConfiguration){
  runConfiguration$scenarioName <- "Human_PB_2Weeks placebo"
  runConfiguration$simulationTime <- 2*7*24*60
  runConfiguration$steadyStateTime <- 2000 * 60
  runConfiguration$paramSheets <- c("Global", "PI human", "Phenobarbital_human")
  runConfiguration$individualCharacteristics <- NULL
  
  simulatedScenario <- runScenario(runConfiguration = runConfiguration)
  
  runConfiguration$scenarioName <- "Human_PB_2Weeks"
  simulatedScenario_treatment <- runScenario(runConfiguration = runConfiguration)
  
  #Observed change in %
  observedT3 <- (1.9 - 1.8) / 1.8 * 100
  observedT4 <- (118.8 - 112.3) / 112.3 * 100
  observedTSH <- (3.9 - 3.4) / 3.4 * 100
  
  lastIdx <- length(simulatedScenario_treatment$outputValues$data$Time)
  simulatedT3 <- (1 - (simulatedScenario_treatment$outputValues$data[[OutputPaths$T3_VB]][[lastIdx]] / 
                         simulatedScenario$outputValues$data[[OutputPaths$T3_VB]][[lastIdx]])) * 100
  
  simulatedT4 <- (1 - (simulatedScenario_treatment$outputValues$data[[OutputPaths$T4_VB]][[lastIdx]] / 
                         simulatedScenario$outputValues$data[[OutputPaths$T4_VB]][[lastIdx]])) * 100
  
  simulatedTSH <- (1 - (simulatedScenario_treatment$outputValues$data[[OutputPaths$TSH_VB]][[lastIdx]] / 
                          simulatedScenario$outputValues$data[[OutputPaths$TSH_VB]][[lastIdx]])) * 100
  
  print("Human healthy changes after 2 weeks PB treatment (observed vs. simulated:")
  print(paste0("T4: ", observedT4, " vs ", -simulatedT4))
  print(paste0("T3: ", observedT3, " vs ", -simulatedT3))
  print(paste0("TSH: ", observedTSH, " vs ", -simulatedTSH))
  
}