###Function to generate a correlation matrix with confidence intervals
#Code pulled from Github user abrowman: https://gist.github.com/abrowman/878f3047d54723b14672
####
cortable <- function(x, docname){ 
  library(Hmisc)
  library(psychometric)
  library(rtf)
  library(psych)
  print(corr.test(as.matrix(corlist)), short=FALSE)
  x <- as.matrix(x)
  R <- rcorr(x)$r
  p <- rcorr(x)$P
  #mystars <- ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ifelse(p < .1, "#", ""))))
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]  # trunctuate correlations to two decimal
  #Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  Rnew <- matrix(R, ncol=ncol(x))
  for (i in 1:length(rcorr(x)$r)) {
    ci <- round(CIr(r = corr.test(as.matrix(corlist))$r[i], n = corr.test(as.matrix(corlist))$n, level = .95), 2)
    Rnew[i] <- paste(Rnew[i], " [", ci[1], ", ", ci[2], "]", sep="")
  }
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  rtffile <- RTF(file=paste(docname, ".doc", sep=""), width=11, height=8.5, font.size=11)
  addTable(rtffile, cbind(rownames(Rnew), Rnew), col.justify="C", header.col.justify="C")
  #addText(rtffile, "# p < .10. * p < .05. ** p < .01. *** p < .001.")
  done(rtffile)
}
