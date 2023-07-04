# SCRIPT TO PULL AND COMPUTE RELEVANT TESTS OF MODEL FIT
########################################################

# Script edited by K. O'Laughlin 2021-11-22 to add annotations and edit user-specific file paths
# Script edited by K. O'Laughlin 2022-12-06 for file path consistency

library(stringr)

# Read in Mplus output files from current directory
###################################################
outgrep = grep(".*\\.out", dir('./Scripts/Mplus CFA Inputs (No TKSW)')) 
outputfiles = dir('./Scripts/Mplus CFA Inputs', full.names = T)[outgrep]
output = lapply(outputfiles, readLines)

# Identify errors
#################
forerror = lapply(output, function(x) grep("*** ERROR", x, fixed = T))
error = unlist(lapply(forerror, function(x)
  ifelse(length(x) >=1, TRUE, FALSE)))

sum(error)

# Get model estimates
#####################
longoutput = lapply(output, function(x) paste(x, collapse = ''))

# Get chi-square test of model fit
chi_sq = lapply(longoutput, function(x)
  
  as.numeric(str_extract(str_extract(x, "Value \\s+ [[:digit:]]+\\.[[:digit:]]{3}"), "[[:digit:]]+\\.[[:digit:]]{3}"))
  
)


# Get chi-square degrees of Freedom
chi_df = lapply(longoutput, function(x)
  
  as.numeric(str_extract(str_extract(x, "Degrees of Freedom \\s+ [[:digit:]]{2}"), "[[:digit:]]{2}"))
  
)


# Get chi-square p-value
chi_sq_p = lapply(longoutput, function(x)
  
  as.numeric(str_extract(str_extract(x, "P-Value \\s+ 0\\.[[:digit:]]{4}"), "0\\.[[:digit:]]{4}"))
  
)


# Get RMSEA
rmsea = lapply(longoutput, function(x)
  
  as.numeric(str_extract(str_extract(x, "Estimate \\s+ 0\\.[[:digit:]]{3}"), "0\\.[[:digit:]]{3}"))
  
) 

# Get RMSEA confidence interval (low)
rmsea.low = lapply(longoutput, function(x)
  
  as.numeric(str_extract(str_extract(x, "90 Percent C.I. \\s+ 0\\.[[:digit:]]{3}"), "0\\.[[:digit:]]{3}"))
    
)


# Get RMSEA confidence interval (high)
rmsea.high = lapply(longoutput, function(x)
  
  as.numeric(str_extract_all(str_extract(x, "90 Percent C.I. \\s+ 0\\.[[:digit:]]{3}  0\\.[[:digit:]]{3}"), "0\\.[[:digit:]]{3}")[[1]][2])
  
)


# Get CFI
cfi = lapply(longoutput, function(x)
  
  as.numeric(str_extract(str_extract(x, "CFI \\s+ [0-1]\\.[[:digit:]]{3}"), "[0-1]\\.[[:digit:]]{3}"))
  
) 


# Get AIC
aic = lapply(longoutput, function(x)
  
  as.numeric(str_extract(str_extract(x, "(AIC)(.*?)-?[[:digit:]]+\\.[[:digit:]]{3}"), "-?[[:digit:]]+\\.[[:digit:]]{3}"))
  
) 


# Get BIC
bic = lapply(longoutput, function(x)
  
  as.numeric(str_extract(str_extract(x, "BIC \\s+ -?[[:digit:]]+\\.[[:digit:]]{3}"), "-?[[:digit:]]+\\.[[:digit:]]{3}"))
  
) 


# Get log-likelihood of null model
L = lapply(longoutput, function(x)
  
  as.numeric(str_extract(str_extract(x, "H0 Value \\s+ -[[:digit:]]+\\.[[:digit:]]{3}"), "-[[:digit:]]+\\.[[:digit:]]{3}"))
  
)


# Get scaling correction factor
c0 = lapply(longoutput, function(x)

  # Need to pull  second occurrence
  as.numeric(str_extract(str_extract_all(x, "Scaling Correction Factor \\s+ -?[[:digit:]]+\\.[[:digit:]]{4}")[[1]][3], "-?[[:digit:]]+\\.[[:digit:]]{4}"))
  
)


# Get model degrees of freedom
numparams = lapply(longoutput, function(x)
  
  as.numeric(str_extract(str_extract(x, "Degrees of Freedom \\s+ [[:digit:]]{2}"), "[[:digit:]]{2}"))
  
)


# Compile fit statistics in data.frame
res = data.frame(Grade = rep(c(3, 5, 7), each = 20), Time = rep(1:4, each = 5),
                 Model = rep(c('1-Factor', '2-Factor (IR with AC)', '2-Factor (WM with AC)', '2-Factor (WM with IR)', '3-Factor'), each = 1),
                 Chi_sq = unlist(chi_sq), pvalue = unlist(chi_sq_p), 
                 CFI = unlist(cfi), 
                 RMSEA = unlist(rmsea), AIC = unlist(aic), 
                 BIC = unlist(bic), L = unlist(L), 
                 c0 = unlist(c0),
                 numparams = unlist(numparams))



# COMPUTE SATORRA-BENTLER CHI-SQUARE TESTS
##########################################
# Subset each set of 2-factor model results into separate data.frames
res_ir.w.ac = res[-which(res$Model == '2-Factor (WM with AC)' |
            res$Model == '2-Factor (WM with IR)'), ]
 
res_wm.w.ac = res[-which(res$Model == '2-Factor (IR with AC)' |
                           res$Model == '2-Factor (WM with IR)'), ]

res_wm.w.ir = res[-which(res$Model == '2-Factor (WM with AC)' |
                           res$Model == '2-Factor (IR with AC)'), ]


# Compute Satorra-Bentler Chi-square tests
res_ir.w.ac$cd = NA

for (i in 1:nrow(res_ir.w.ac)) {
  
  res_ir.w.ac$cd[i] = (res_ir.w.ac$numparams[i]*res_ir.w.ac$c0[i] - res_ir.w.ac$numparams[i + 1]*res_ir.w.ac$c0[i + 1]) /
    (res_ir.w.ac$numparams[i] - res_ir.w.ac$numparams[i + 1])  
  
  res_ir.w.ac$TRd[i] = (res_ir.w.ac$Chi_sq[i]*res_ir.w.ac$c0[i] - res_ir.w.ac$Chi_sq[i + 1]*res_ir.w.ac$c0[i + 1])/res_ir.w.ac$cd[i]
  
  res_ir.w.ac$df[i] = res_ir.w.ac$numparams[i] - res_ir.w.ac$numparams[i + 1]
  
}



res_wm.w.ac$cd = NA

for (i in 1:nrow(res_wm.w.ac)) {
  
  res_wm.w.ac$cd[i] = (res_wm.w.ac$numparams[i]*res_wm.w.ac$c0[i] - res_wm.w.ac$numparams[i + 1]*res_wm.w.ac$c0[i + 1]) /
    (res_wm.w.ac$numparams[i] - res_wm.w.ac$numparams[i + 1])  
  
  res_wm.w.ac$TRd[i] = (res_wm.w.ac$Chi_sq[i]*res_wm.w.ac$c0[i] - res_wm.w.ac$Chi_sq[i + 1]*res_wm.w.ac$c0[i + 1])/res_wm.w.ac$cd[i]
  
  res_wm.w.ac$df[i] = res_wm.w.ac$numparams[i] - res_wm.w.ac$numparams[i + 1]
}




res_wm.w.ir$cd = NA

for (i in 1:nrow(res_wm.w.ir)) {
  
  res_wm.w.ir$cd[i] = (res_wm.w.ir$numparams[i]*res_wm.w.ir$c0[i] - res_wm.w.ir$numparams[i + 1]*res_wm.w.ir$c0[i + 1]) /
    (res_wm.w.ir$numparams[i] - res_wm.w.ir$numparams[i + 1])  
  
  res_wm.w.ir$TRd[i] = (res_wm.w.ir$Chi_sq[i]*res_wm.w.ir$c0[i] - res_wm.w.ir$Chi_sq[i + 1]*res_wm.w.ir$c0[i + 1])/res_wm.w.ir$cd[i]
  
  res_wm.w.ir$df[i] = res_wm.w.ir$numparams[i] - res_wm.w.ir$numparams[i + 1]
  
}


# Compile formatted fit statistics in data.frame 
res2 = data.frame(Grade = rep(c('3rd Grade', '5th Grade', '7th Grade'), each = 20), Time = rep(1:4, each = 5),
                  Model = rep(c('1-Factor', '2-Factor (IR with AC)', '2-Factor (WM with AC)', '2-Factor (WM with IR)', '3-Factor'), each = 1),
                  Chi_sq = format(unlist(chi_sq), nsmall = 3), df = unlist(chi_df), pvalue = format(unlist(chi_sq_p), nsmall = 3), CFI = format(unlist(cfi), nsmall = 3), 
                  RMSEA = paste0(format(unlist(rmsea), nsmall = 3), ' (', format(unlist(rmsea.low), nsmall = 3), ', ', format(unlist(rmsea.high), nsmall = 3), ')'), AIC = format(unlist(aic), nsmall = 3), BIC = format(unlist(bic), nsmall = 3))


# Compile formatted Satorra-Bentler Chi-Square Test Results
LRTs = data.frame(Grade = rep(c('3rd Grade', '5th Grade', '7th Grade'),
                              each = 4), Time = rep(1:4, 3),
                  chi_1.v.2_ir.w.ac = paste0(round(res_ir.w.ac[res_ir.w.ac$Model == '1-Factor', 'TRd'], 3), ' (', res_ir.w.ac[res_ir.w.ac$Model == '1-Factor', 'df'], ')'),
                  p_1.v.2_ir.w.ac = round(pchisq(res_ir.w.ac[res_ir.w.ac$Model == '1-Factor', 'TRd'], 1, lower.tail = F), 3), 
                  chi_1.v.2_wm.w.ac = paste0(round(res_wm.w.ac[res_wm.w.ac$Model == '1-Factor', 'TRd'], 3), ' (', res_wm.w.ac[res_wm.w.ac$Model == '1-Factor', 'df'], ')'),
                  p_1.v.2_wm.w.ac = round(pchisq(res_wm.w.ac[res_wm.w.ac$Model == '1-Factor', 'TRd'], 1, lower.tail = F), 3), 
                  chi_1.v.2_wm.w.ir = paste0(round(res_wm.w.ir[res_wm.w.ir$Model == '1-Factor', 'TRd'], 3), ' (', res_wm.w.ir[res_wm.w.ir$Model == '1-Factor', 'df'], ')'),
                  p_1.v.2_wm.w.ir = round(pchisq(res_wm.w.ir[res_wm.w.ir$Model == '1-Factor', 'TRd'], 1, lower.tail = F), 3),
                  
                  chi_2_ir.w.ac.v.3 = paste0(round(res_ir.w.ac[res_ir.w.ac$Model == '2-Factor (IR with AC)', 'TRd'], 3), ' (', res_ir.w.ac[res_ir.w.ac$Model == '2-Factor (IR with AC)', 'df'], ')'),
                  p_2_ir.w.ac.v.3 = round(pchisq(res_ir.w.ac[res_ir.w.ac$Model == '2-Factor (IR with AC)', 'TRd'], 2, lower.tail = F), 3), 
                  chi_2_wm.w.ac.v.3 = paste0(round(res_wm.w.ac[res_wm.w.ac$Model == '2-Factor (WM with AC)', 'TRd'], 3), ' (', res_wm.w.ac[res_wm.w.ac$Model == '2-Factor (WM with AC)', 'df'], ')'),
                  p_2_wm.w.ac.v.3 = round(pchisq(res_wm.w.ac[res_wm.w.ac$Model == '2-Factor (WM with AC)', 'TRd'], 2, lower.tail = F), 3), 
                  chi_2_wm.w.ir.v.3 = paste0(round(res_wm.w.ir[res_wm.w.ir$Model == '2-Factor (WM with IR)', 'TRd'], 3), ' (', res_wm.w.ir[res_wm.w.ir$Model == '2-Factor (WM with IR)', 'df'], ')'),
                  p_2_wm.w.ir.v.3 = round(pchisq(res_wm.w.ir[res_wm.w.ir$Model == '2-Factor (WM with IR)', 'TRd'], 2, lower.tail = F), 3))
                  
                  

# Write results to .csv files
write.csv(res2, file = paste0('./Results/CFA_Models/FinalModels_CFA_ModelFit_', Sys.Date(), '.csv'), row.names = F)

write.csv(LRTs, file = paste0('./Results/CFA_Models/FinalModels_CFA_LRTs_', Sys.Date(), '.csv'), row.names = F)

