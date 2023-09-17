# SCRIPT TO PULL FACTOR LOADINGS FROM MPLUS OUTPUT FILES
########################################################

# Script edited by K. O'Laughlin 2021-11-22 to add annotations and edit user-specific file paths
# Script edited by K. O'Laughlin 2022-12-01 to set consistent file paths


library(stringr)

# Read in Mplus output files from current directory
outgrepf = grep(".*\\.out", dir('./Scripts/Mplus CFA Inputs'))
outputfilesf = dir('./Scripts/Mplus CFA Inputs')[outgrepf]
outputf = lapply(paste0('./Scripts/Mplus CFA Inputs/', outputfilesf), readLines)
longoutputf = lapply(outputf, function(x) paste(x, collapse = ''))


# Extract Standardized Factor Loadings
######################################
BSS = lapply(longoutputf, function(x)
  
  as.numeric(str_extract(str_extract(str_extract(x, "STANDARDIZED MODEL RESULTS(.*?)Means" ), 'BSS(.*?)[[:digit:]]+\\.[[:digit:]]{3}'), '-?[[:digit:]]+\\.[[:digit:]]{3}'))
  
  )

FSS = lapply(longoutputf, function(x)
  
  as.numeric(str_extract(str_extract(str_extract(x, "STANDARDIZED MODEL RESULTS(.*?)Means" ), 'FSS(.*?)[[:digit:]]+\\.[[:digit:]]{3}'), '-?[[:digit:]]+\\.[[:digit:]]{3}'))
  
)


STSUS = lapply(longoutputf, function(x)
  
  as.numeric(str_extract(str_extract(str_extract(x, "STANDARDIZED MODEL RESULTS(.*?)Means" ), 'STSUS(.*?)[[:digit:]]+\\.[[:digit:]]{3}'), '-?[[:digit:]]+\\.[[:digit:]]{3}'))
  
)

STIMP = lapply(longoutputf, function(x)
  
  as.numeric(str_extract(str_extract(str_extract(x, "STANDARDIZED MODEL RESULTS(.*?)Means" ), 'STIMP(.*?)[[:digit:]]+\\.[[:digit:]]{3}'), '-?[[:digit:]]+\\.[[:digit:]]{3}'))
  
)

TNT = lapply(longoutputf, function(x)
  
  as.numeric(str_extract(str_extract(str_extract(x, "STANDARDIZED MODEL RESULTS(.*?)Means" ), 'TNT(.*?)[[:digit:]]+\\.[[:digit:]]{3}'), '-?[[:digit:]]+\\.[[:digit:]]{3}'))
  
)

STRP = lapply(longoutputf, function(x)
  
  as.numeric(str_extract(str_extract(str_extract(x, "STANDARDIZED MODEL RESULTS(.*?)Means" ), 'STRP(.*?)[[:digit:]]+\\.[[:digit:]]{3}'), '-?[[:digit:]]+\\.[[:digit:]]{3}'))
  
)

FLNK = lapply(longoutputf, function(x)
  
  as.numeric(str_extract(str_extract(str_extract(x, "STANDARDIZED MODEL RESULTS(.*?)Means" ), 'FLNK(.*?)[[:digit:]]+\\.[[:digit:]]{3}'), '-?[[:digit:]]+\\.[[:digit:]]{3}'))
  
)

BOX = lapply(longoutputf, function(x)
  
  as.numeric(str_extract(str_extract(str_extract(x, "STANDARDIZED MODEL RESULTS(.*?)Means" ), 'BOX(.*?)[[:digit:]]+\\.[[:digit:]]{3}'), '-?[[:digit:]]+\\.[[:digit:]]{3}'))
  
)


TDwWM = lapply(longoutputf, function(x)
  
  as.numeric(str_extract(str_extract(str_extract(str_extract(x, "STANDARDIZED MODEL RESULTS(.*?)Means"), "WITH(.*?)Means" ), 'WM(.*?)[[:digit:]]+\\.[[:digit:]]{3}'), '-?[[:digit:]]+\\.[[:digit:]]{3}'))
  
)


TDwIR = lapply(longoutputf, function(x)
  
  as.numeric(str_extract(str_extract(str_extract(str_extract(x, "STANDARDIZED MODEL RESULTS(.*?)Means"), "WITH(.*?)Means" ), 'TD(.*?)[[:digit:]]+\\.[[:digit:]]{3}'), '-?[[:digit:]]+\\.[[:digit:]]{3}'))

)



IRwWM = lapply(longoutputf, function(x)
  
  as.numeric(str_extract(str_extract_all(str_extract(str_extract(x, "STANDARDIZED MODEL RESULTS(.*?)Means"), "WITH(.*?)Means" ), 'WM(.*?)[[:digit:]]+\\.[[:digit:]]{3}')[[1]][2], '-?[[:digit:]]+\\.[[:digit:]]{3}'))

)


IRwAC = lapply(longoutputf, function(x)
  
  as.numeric(str_extract(str_extract(str_extract(str_extract(x, "STANDARDIZED MODEL RESULTS(.*?)Means"), "WITH(.*?)Means" ), 'AC(.*?)[[:digit:]]+\\.[[:digit:]]{3}'), '-?[[:digit:]]+\\.[[:digit:]]{3}'))
  
)


# Set sequence for 1-factor model results 
F1 = seq(1, length(longoutputf), by = 5)

# Create empty matrix to store results
out1F = matrix(nrow = 8, ncol = 13)
colnames(out1F) = c('task', 'g3t1', 'g3t2', 'g3t3', 'g3t4', 'g5t1', 'g5t2', 'g5t3', 'g5t4', 'g7t1', 'g7t2', 'g7t3', 'g7t4')

# Store results for 1-factor models
out1F[1, 1:13] = c('BSS', unlist(BSS)[F1])
out1F[2, 1:13] = c('FSS', unlist(FSS)[F1])
out1F[3, 1:13] = c('SSUS', unlist(STSUS)[F1])
out1F[4, 1:13] = c('SIMP', unlist(STIMP)[F1])
out1F[5, 1:13] = c('TNT', unlist(TNT)[F1])
out1F[6, 1:13] = c('STRP', unlist(STRP)[F1])
out1F[7, 1:13] = c('FLNK', unlist(FLNK)[F1])
out1F[8, 1:13] = c('BOX', unlist(BOX)[F1])

# Convert results to data.frame
out1F = as.data.frame(out1F)

# Write results to .csv file
write.csv(out1F, file = paste0('./Results/CFA_Models/iLEAD_CFAs_1Factor_Loadings_', Sys.Date(), '.csv'), row.names = F)



# Set sequence for 2-factor model results
F2_ir.w.ac = seq(2, length(longoutputf), by = 5)

# Create matrix to store 2-factor model results
out2F_ir.w.ac = matrix(nrow = 9, ncol = 13)
colnames(out2F_ir.w.ac) = c('task', 'g3t1', 'g3t2', 'g3t3', 'g3t4', 'g5t1', 'g5t2', 'g5t3', 'g5t4', 'g7t1', 'g7t2', 'g7t3', 'g7t4')

# Store 2-factor model results
out2F_ir.w.ac[1, 1:13] = c('BSS', unlist(BSS)[F2_ir.w.ac])
out2F_ir.w.ac[2, 1:13] = c('FSS', unlist(FSS)[F2_ir.w.ac])
out2F_ir.w.ac[3, 1:13] = c('SSUS', unlist(STSUS)[F2_ir.w.ac])
out2F_ir.w.ac[4, 1:13] = c('SIMP', unlist(STIMP)[F2_ir.w.ac])
out2F_ir.w.ac[5, 1:13] = c('TNT', unlist(TNT)[F2_ir.w.ac])
out2F_ir.w.ac[6, 1:13] = c('STRP', unlist(STRP)[F2_ir.w.ac])
out2F_ir.w.ac[7, 1:13] = c('FLNK', unlist(FLNK)[F2_ir.w.ac])
out2F_ir.w.ac[8, 1:13] = c('BOX', unlist(BOX)[F2_ir.w.ac])
out2F_ir.w.ac[9, 1:13] = c('IR+RI w WM', unlist(TDwWM)[F2_ir.w.ac])

out2F_ir.w.ac = as.data.frame(out2F_ir.w.ac)

# Write 2-factor model results to .csv file
write.csv(out2F_ir.w.ac, file = paste0('./Results/CFA_Models/iLEAD_CFAs_2Factor_ir.w.ac_Loadings_', Sys.Date(), '.csv'), row.names = F)



F2_wm.w.ac = seq(3, length(longoutputf), by = 5)

out2F_wm.w.ac = matrix(nrow = 9, ncol = 13)

colnames(out2F_wm.w.ac) = c('task', 'g3t1', 'g3t2', 'g3t3', 'g3t4', 'g5t1', 'g5t2', 'g5t3', 'g5t4', 'g7t1', 'g7t2', 'g7t3', 'g7t4')

out2F_wm.w.ac[1, 1:13] = c('BSS', unlist(BSS)[F2_wm.w.ac])
out2F_wm.w.ac[2, 1:13] = c('FSS', unlist(FSS)[F2_wm.w.ac])
out2F_wm.w.ac[3, 1:13] = c('SSUS', unlist(STSUS)[F2_wm.w.ac])
out2F_wm.w.ac[4, 1:13] = c('SIMP', unlist(STIMP)[F2_wm.w.ac])
out2F_wm.w.ac[5, 1:13] = c('TNT', unlist(TNT)[F2_wm.w.ac])
out2F_wm.w.ac[6, 1:13] = c('STRP', unlist(STRP)[F2_wm.w.ac])
out2F_wm.w.ac[7, 1:13] = c('FLNK', unlist(FLNK)[F2_wm.w.ac])
out2F_wm.w.ac[8, 1:13] = c('BOX', unlist(BOX)[F2_wm.w.ac])
out2F_wm.w.ac[9, 1:13] = c('WM+RI w IR', unlist(TDwIR)[F2_wm.w.ac])


out2F_wm.w.ac = as.data.frame(out2F_wm.w.ac)

write.csv(out2F_wm.w.ac, file = paste0('./Results/CFA_Models/iLEAD_CFAs_2Factor_wm.w.ac_Loadings_', Sys.Date(), '.csv'), row.names = F)




# Set sequence for 2-factor model results
F2_wm.w.ir = seq(4, length(longoutputf), by = 5)

# Create matrix to store 2-factor model results
out2F_wm.w.ir = matrix(nrow = 9, ncol = 13)
colnames(out2F_wm.w.ir) = c('task', 'g3t1', 'g3t2', 'g3t3', 'g3t4', 'g5t1', 'g5t2', 'g5t3', 'g5t4', 'g7t1', 'g7t2', 'g7t3', 'g7t4')

# Store 2-factor model results
out2F_wm.w.ir[1, 1:13] = c('BSS', unlist(BSS)[F2_wm.w.ir])
out2F_wm.w.ir[2, 1:13] = c('FSS', unlist(FSS)[F2_wm.w.ir])
out2F_wm.w.ir[3, 1:13] = c('SSUS', unlist(STSUS)[F2_wm.w.ir])
out2F_wm.w.ir[4, 1:13] = c('SIMP', unlist(STIMP)[F2_wm.w.ir])
out2F_wm.w.ir[5, 1:13] = c('TNT', unlist(TNT)[F2_wm.w.ir])
out2F_wm.w.ir[6, 1:13] = c('STRP', unlist(STRP)[F2_wm.w.ir])
out2F_wm.w.ir[7, 1:13] = c('FLNK', unlist(FLNK)[F2_wm.w.ir])
out2F_wm.w.ir[8, 1:13] = c('BOX', unlist(BOX)[F2_wm.w.ir])
out2F_wm.w.ir[9, 1:13] = c('RI w WM+IR', unlist(TDwIR)[F2_wm.w.ir])

out2F_wm.w.ir = as.data.frame(out2F_wm.w.ir)

# Write 2-factor model results to .csv file
write.csv(out2F_wm.w.ir, file = paste0('./Results/CFA_Models/iLEAD_CFAs_2Factor_wm.w.ir_Loadings_', Sys.Date(), '.csv'), row.names = F)



# Set sequence for 3-factor model results
F3 = seq(5, length(longoutputf), by = 5)

# Create matrix to store 3-factor model results
out3F = matrix(nrow = 11, ncol = 13)
colnames(out3F) = c('task', 'g3t1', 'g3t2', 'g3t3', 'g3t4', 'g5t1', 'g5t2', 'g5t3', 'g5t4', 'g7t1', 'g7t2', 'g7t3', 'g7t4')

# Store 3-factor model results
out3F[1, 1:13] = c('BSS', unlist(BSS)[F3])
out3F[2, 1:13] = c('FSS', unlist(FSS)[F3])
out3F[3, 1:13] = c('SSUS', unlist(STSUS)[F3])
out3F[4, 1:13] = c('SIMP', unlist(STIMP)[F3])
out3F[5, 1:13] = c('TNT', unlist(TNT)[F3])
out3F[6, 1:13] = c('STRP', unlist(STRP)[F3])
out3F[7, 1:13] = c('FLNK', unlist(FLNK)[F3])
out3F[8, 1:13] = c('BOX', unlist(BOX)[F3])
out3F[9, 1:13] = c('RI w WM', unlist(TDwWM)[F3])
out3F[10, 1:13] = c('IR w WM', unlist(IRwWM)[F3])
out3F[11, 1:13] = c('IR w RI', unlist(IRwAC)[F3])

out3F = as.data.frame(out3F)

# Write 3-factor model results to .csv file
write.csv(out3F, file = paste0('./Results/CFA_Models/iLEAD_CFAs_3Factor_Loadings_', Sys.Date(), '.csv'), row.names = F)

