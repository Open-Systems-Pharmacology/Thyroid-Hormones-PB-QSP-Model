getTestParameters <- function(params){
  paramVals <- enum(list(
    ######TSH clearance#####
    # 'TSH-Total Hepatic Clearance-Lit|Specific clearance' = 0.64,
    ######TSH synthesis#####
    # 'TSH_synthesis|Circadian_Amplitude' = 0,
    # 'TSH_synthesis|Circadian_x_Shift' = -7,
    #     'TSH_synthesis|k_syn' = 0.0033,
    #      'TSH_synthesis|alpha_T4_TSH_syn' = 3,#2,
    #    'TSH_synthesis|alpha_T3_TSH_syn' = 4,#4,
    #    'TSH_synthesis|EC50_T3_TSH_syn' = 0.6#0.55,
    #      'TSH_synthesis|EC50_T4_TSH_syn' = 2.3,#1.9
    ######T4 synthesis######
    #    'T4_synthesis|k_sec' = 1,
    #  'T4_synthesis|k_transfer' = 5e-5,
    #  'T4_synthesis|k_syn' = 6e-3, # 0.006944435
    #"T4_synthesis|alpha_M_TSH_T4_syn" = 2,
    #     'T4_synthesis|Km_M_TSH_T4_syn' = 0.027,
    #      'T4_synthesis|Vmax_M_TSH_T4_syn' = 6,
#         "T4_synthesis|Imax_Perpetrator" = 1,              #!!!!
#    "T4_synthesis|Imax_PTU" = 1,                           #!!!!
    # "T4_synthesis|IC_50_PTU" = 1.2,
    ######T3 synthesis#####
    #    'T3_synthesis|k_syn' = 4e-3,
 #   'T3_synthesis|k_sec' = 2e-4,
    #'T3_synthesis|k_transfer' = 9e-5,
#    "T3_synthesis|Imax_PTU" = 0.2,                        #!!!!
#      "T3_synthesis|Imax_Perpetrator" = 1,                  #!!!!
    # "T3_synthesis|Km_M_TSH_T3_syn" = 0.02,
    # "T3_synthesis|Vmax_M_TSH_T3_syn" = 2.6,
    #"T3_synthesis|alpha_M_TSH_T3_syn" = 2,
    #####T4-T3 conversion#####
 #        "T4-DIO-Human|Imax_PTU" = 1          ,           #!!!!
 #      "T4-DIO-Human|IC_50_PTU" = 0.5,
    #"T4-DIO-Human|Specific clearance" = 0.4, #FIXED FOR HUMAN
    ######T4 hepatic clearance#####
    #"T4-Total Hepatic Clearance-NA|Specific clearance" = 2 #FIXED FOR HUMAN
    "T4-Total Hepatic Clearance-NA|Vmax_Perpetrator" = 1,
    "T4-Total Hepatic Clearance-NA|Km_Perpetrator" = 0.1,
"T4-Total Hepatic Clearance-NA|tau_M_Perpetrator" = 24
#"Perpetrator-ThyroBP-Complex-Elimination|ke" = 0
  ))
  
  paramUnits <- enum(list(
    ######TSH clearance#####
    'TSH-Total Hepatic Clearance-Lit|Specific clearance' = "1/min",
    ######TSH synthesis#####
    'TSH_synthesis|Circadian_Amplitude' = "",
    'TSH_synthesis|Circadian_x_Shift' = "h",
    'TSH_synthesis|k_syn' = "µmol/min",
    'TSH_synthesis|alpha_T4_TSH_syn' = "",
    "TSH_synthesis|alpha_T3_TSH_syn" = "",
    'TSH_synthesis|EC50_T3_TSH_syn' = "nmol/l",
    'TSH_synthesis|EC50_T4_TSH_syn' = "nmol/l",
    ######T4 synthesis######
    'T4_synthesis|k_syn' = "µmol/min",
    'T4_synthesis|k_transfer' = "1/min",
    'T4_synthesis|k_sec' = "1/min",
    "T4_synthesis|Imax_Perpetrator" = "",
    "T4_synthesis|Km_M_TSH_T4_syn" = "nmol/l",
    "T4_synthesis|Vmax_M_TSH_T4_syn" = "",
    "T4_synthesis|Imax_PTU" = "",
    "T4_synthesis|IC_50_PTU" = "µmol/l",
    'T4_synthesis|alpha_M_TSH_T4_syn' = "",
    'T4_synthesis|Km_M_TSH_T4_syn' = "nmol/l",
    'T4_synthesis|Vmax_M_TSH_T4_syn' = "",
    ######T3 synthesis#####
    'T3_synthesis|k_syn' = "µmol/min",
    'T3_synthesis|k_sec' = "1/min",
    'T3_synthesis|k_transfer' = "1/min",
    "T3_synthesis|Imax_PTU" = "",
    "T3_synthesis|Imax_Perpetrator" = "",
    "T3_synthesis|Km_M_TSH_T3_syn" = "nmol/l",
    "T3_synthesis|Vmax_M_TSH_T3_syn" = "",
    "T3_synthesis|alpha_M_TSH_T3_syn" = "",
    #####T4-T3 conversion#####
    "T4-DIO-Human|Specific clearance" = "1/min",
    "T4-DIO-Human|Imax_PTU" = "",
    "T4-DIO-Human|IC_50_PTU" = "µmol/l",
    ######T4 hepatic clearance#####
    "T4-Total Hepatic Clearance-NA|Specific clearance" = "1/min",
    "T4-Total Hepatic Clearance-NA|Vmax_Perpetrator" = "",
    "T4-Total Hepatic Clearance-NA|Km_Perpetrator" = "µmol/l",
    "Perpetrator-ThyroBP-Complex-Elimination|ke" = "1/min",
    "T4-Total Hepatic Clearance-NA|tau_M_Perpetrator" = "h"
  ))
  
  
  idx <- match(enumKeys(paramVals), params$paths)
  
  # Add entries if the parameter paths are not present in the original params
  notPresent <- which(is.na(idx))
  for (i in notPresent){
    path <- enumKeys(paramVals)[[i]]
    value <- paramVals[[i]]
    unit <- enumGetValue(key = path, enum = paramUnits)
    
    params$paths <- append(params$paths, path)
    params$values <- append(params$values, value)
    params$units <- append(params$units, unit)
    
    idx[[i]] <- length(params$paths)
  }
  
  params$values[idx] <- enumValues(paramVals)
  params$units[idx] <- lapply(enumKeys(paramVals), function(x){enumGetValue(enum = paramUnits, key = x)})
  
  return(params)
}
