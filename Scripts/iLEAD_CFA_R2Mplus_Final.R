# R Script to Generate all Mplus Inputs Testing
# 1-, 2-, and 3-Factor Models of Executive Function
###################################################

## Updated 2021-11-19 by K. O'Laughlin to add annotations and edit user-specific file paths

# Set working directory where empty Mplus input files are stored
setwd("./Scripts/Mplus CFA Inputs")


# Loop to create all input files for 1 Factor Model
###################################################
for (g in c(3, 5, 7)) {
  
  for (times in 1:4){
    
    input_1Factor = readLines("input_1factor.txt")
    
    input_1Factor[13] = gsub("time", times, input_1Factor[13])
    input_1Factor[14] = gsub("time", times, input_1Factor[14])
    input_1Factor[21] = gsub("time", times, input_1Factor[21])
    input_1Factor[23] = gsub("time", times, input_1Factor[23])
    input_1Factor[32] = gsub("time", times, input_1Factor[32])
    input_1Factor[33] = gsub("time", times, input_1Factor[33])
    input_1Factor[37] = gsub("time", times, input_1Factor[37])
    input_1Factor[40] = gsub("time", times, input_1Factor[40])
    input_1Factor[44] = gsub("time", times, input_1Factor[44])
    input_1Factor[48] = gsub("time", times, input_1Factor[48])
    
    
    input_1Factor[16] = gsub("cohort", g, input_1Factor[16])
    
    write(input_1Factor, file = 
            paste("G", g, "_T", times, "_1F.inp", sep = ""))
    
  }
  
}
   


                                  
# Loop to create all input files for 2 Factor Model combining IR with AC
########################################################################
for (g in c(3, 5, 7)) {
  
  for (times in 1:4){
    
    input_2Factor = readLines("input_2factor_ir.w.ac.txt")
    
    input_2Factor[13] = gsub("time", times, input_2Factor[13])
    input_2Factor[14] = gsub("time", times, input_2Factor[14])
    input_2Factor[21] = gsub("time", times, input_2Factor[21])
    input_2Factor[23] = gsub("time", times, input_2Factor[23])
    input_2Factor[32] = gsub("time", times, input_2Factor[32])
    input_2Factor[33] = gsub("time", times, input_2Factor[33])
    input_2Factor[37] = gsub("time", times, input_2Factor[37])
    input_2Factor[40] = gsub("time", times, input_2Factor[40])
    input_2Factor[41] = gsub("time", times, input_2Factor[41])
    input_2Factor[44] = gsub("time", times, input_2Factor[44])
    input_2Factor[48] = gsub("time", times, input_2Factor[48])
    
    input_2Factor[16] = gsub("cohort", g, input_2Factor[16])
    
    write(input_2Factor, file = 
            paste("G", g, "_T", times, "_2F_ir.w.ac.inp", sep = ""))
    
  }
  
}



# Loop to create all input files for 2 Factor Model combining WM and AC
#######################################################################
for (g in c(3, 5, 7)) {
  
  for (times in 1:4){
    
    input_2Factor = readLines("input_2factor_wm.w.ac.txt")
    
    input_2Factor[13] = gsub("time", times, input_2Factor[13])
    input_2Factor[14] = gsub("time", times, input_2Factor[14])
    input_2Factor[21] = gsub("time", times, input_2Factor[21])
    input_2Factor[23] = gsub("time", times, input_2Factor[23])
    input_2Factor[32] = gsub("time", times, input_2Factor[32])
    input_2Factor[33] = gsub("time", times, input_2Factor[33])
    input_2Factor[37] = gsub("time", times, input_2Factor[37])
    input_2Factor[40] = gsub("time", times, input_2Factor[40])
    input_2Factor[41] = gsub("time", times, input_2Factor[41])
    input_2Factor[44] = gsub("time", times, input_2Factor[44])
    input_2Factor[48] = gsub("time", times, input_2Factor[48])
    
  
    input_2Factor[16] = gsub("cohort", g, input_2Factor[16])
    
    write(input_2Factor, file = 
            paste("G", g, "_T", times, "_2F_wm.w.ac.inp", sep = ""))
    
  }
  
}



# Loop to create all input files for 2 Factor Model combining WM and IR
#######################################################################
for (g in c(3, 5, 7)) {
  
  for (times in 1:4){
    
    input_2Factor = readLines("input_2factor_wm.w.ir.txt")
    
    input_2Factor[13] = gsub("time", times, input_2Factor[13])
    input_2Factor[14] = gsub("time", times, input_2Factor[14])
    input_2Factor[21] = gsub("time", times, input_2Factor[21])
    input_2Factor[23] = gsub("time", times, input_2Factor[23])
    input_2Factor[32] = gsub("time", times, input_2Factor[32])
    input_2Factor[33] = gsub("time", times, input_2Factor[33])
    input_2Factor[37] = gsub("time", times, input_2Factor[37])
    input_2Factor[40] = gsub("time", times, input_2Factor[40])
    input_2Factor[41] = gsub("time", times, input_2Factor[41])
    input_2Factor[44] = gsub("time", times, input_2Factor[44])
    input_2Factor[48] = gsub("time", times, input_2Factor[48])
    
  
    
    input_2Factor[16] = gsub("cohort", g, input_2Factor[16])
    
    write(input_2Factor, file = 
            paste("G", g, "_T", times, "_2F_wm.w.ir.inp", sep = ""))
    
  }
  
}




# Loop to create all input files for 3 Factor Model 
###################################################
for (g in c(3, 5, 7)) {
  
  for (times in 1:4){
    
    input_3Factor = readLines("input_3factor.txt")
    
    input_3Factor[13] = gsub("time", times, input_3Factor[13])
    input_3Factor[14] = gsub("time", times, input_3Factor[14])
    input_3Factor[21] = gsub("time", times, input_3Factor[21])
    input_3Factor[23] = gsub("time", times, input_3Factor[23])
    input_3Factor[32] = gsub("time", times, input_3Factor[32])
    input_3Factor[33] = gsub("time", times, input_3Factor[33])
    input_3Factor[34] = gsub("time", times, input_3Factor[34])
    input_3Factor[38] = gsub("time", times, input_3Factor[38])
    input_3Factor[41] = gsub("time", times, input_3Factor[41])
    input_3Factor[42] = gsub("time", times, input_3Factor[42])
    input_3Factor[43] = gsub("time", times, input_3Factor[43])
    input_3Factor[46] = gsub("time", times, input_3Factor[46])
    input_3Factor[50] = gsub("time", times, input_3Factor[50])
    
      
   input_3Factor[16] = gsub("cohort", g, input_3Factor[16])
    
    write(input_3Factor, file = 
            paste("G", g, "_T", times, "_3F.inp", sep = ""))
    
  }
  
}




# Conduct Analyses in Mplus
###########################

# Pull Mplus input files into current working directory 
inpgrep = grep(".\\.inp", dir())
inputfiles = dir()[inpgrep]


# Execute Mplus commands for all input files
sapply(inputfiles, function(x)
  system(paste('mplus.exe', x, sep = " "), wait = T))
