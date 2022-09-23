library(esqlabsR)
sourceAll(file.path(getwd(), "GeneralCode"))
sourceAll(file.path(getwd(), "TransferFunctions"))
sourceAll(file.path(getwd(), "ProjectCode"))
runConfiguration <- projectConfiguration()
#######INPUTS###########
#Change this value to "TRUE" if you want figures written to a file
runConfiguration$"outputToPNG" <- T
runConfiguration$"setTestParameters" <- F
runConfiguration$"simulateSteadyState" <- TRUE

######Scenarios#########

Pilo_1990_T3(runConfiguration)
Pilo_1990_T4(runConfiguration)

Eisenberg_2008_Fig45_400ug(runConfiguration)
Eisenberg_2008_Fig45_600ug(runConfiguration)
Eisenberg_2008_Fig9(runConfiguration)

Reeth_1987(runConfiguration)
Andrare_1999(runConfiguration)
Laurberg_2007(runConfiguration)

#Rat
#SteadyState_ratTx(runConfiguration)
DiStefano_1982_T4_iv(runConfiguration)
Silva_1997_T3_iv(runConfiguration)
Silva_1997_T3_iv_PD(runConfiguration)
SteadyState_rat(runConfiguration)
Cooper_1983_MMI(runConfiguration, F)
Cooper_1983_PTU(runConfiguration, F)
Wong_2005_PTU(runConfiguration)

#PB
Michael_PB_3weeks(runConfiguration)
Kato_2010_PB(runConfiguration)
Wong_2005_PB(runConfiguration)

Michael_PB_2weeks(runConfiguration)
Human_PB_2Weeks(runConfiguration)
