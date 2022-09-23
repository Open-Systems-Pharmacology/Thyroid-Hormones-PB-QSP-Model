runScenario <- function(dose = NULL, runConfiguration){
  simulation <- loadSimulation(filePath = runConfiguration$modelPath)
  params <- readParametersFromXLS(runConfiguration$paramsPath, runConfiguration$paramSheets)
  if (runConfiguration$setTestParameters){
    params <- getTestParameters(params)
  }
  disableApplications(simulation)
  initializeSimulation(simulation, runConfiguration$individualCharacteristics, params, simulateSteadyState = runConfiguration$simulateSteadyState, steadyStateTime = runConfiguration$steadyStateTime)
  
  #Set the outputs
  clearOutputs(simulation)
  addOutputs(quantitiesOrPaths = enumValues(OutputPaths), simulation = simulation)
  #Set simulation time
  setOutputInterval(simulation = simulation, startTime = 0, endTime = runConfiguration$simulationTime, resolution = 1)
  
  # Set administration protocol
  setApplications(simulation, runConfiguration$scenarioName, dose)
  #Run simulation and get the results
  simulationResults <- runSimulation(simulation = simulation)
  return(list(simulation = simulation, results = simulationResults, 
              outputValues = getOutputValues(simulationResults, quantitiesOrPaths = getAllQuantitiesMatching(enumValues(OutputPaths), simulation))))
}