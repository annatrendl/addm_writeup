#setwd("C:/Anna/ADDM_pure_c")
#rm(list=ls())

#Command prompt
#cd /d C:\Anna\addm_ML
#cd /d C:\Anna\addm_ML
#set PATH=C:\R-3.3.1\bin;C:\R-3.3.1\bin\x64;C:\Rtools\bin;C:\Rtools\mingw_64\bin; shortPath "C:\Program Files\R\R-3.3.1\bin\x64";C:\ProgramData\Oracle\Java\javapath;C:\WINDOWS\system32;C:\WINDOWS;C:\WINDOWS\System32\Wbem;shortPath "C:\Program Files (x86)\Microsoft Application Virtualization Client";C:\WINDOWS\System32\WindowsPowerShell\v1.0\;shortPath "C:\PROGRAM FILES\OPENOFFICE.ORG 3\URE\BIN"; shortPath "C:\PROGRAM FILES (X86)\OPENOFFICE.ORG 3\URE\BIN";C:\WINDOWS\System32\WindowsPowerShell\v1.0\;H:\Documents\miktex\miktex\bin\


#cd /d C:\Anna\grid_search
#set PATH=C:\R-3.3.2\bin;C:\R-3.3.2\bin\x64;C:\Rtools\bin;C:\Rtools\mingw_64\bin; shortPath "C:\R-3.3.2\bin\x64";C:\ProgramData\Oracle\Java\javapath;C:\WINDOWS\system32;C:\WINDOWS;C:\WINDOWS\System32\Wbem;shortPath "C:\Program Files (x86)\Microsoft Application Virtualization Client";C:\WINDOWS\System32\WindowsPowerShell\v1.0\;shortPath "C:\PROGRAM FILES\OPENOFFICE.ORG 3\URE\BIN"; shortPath "C:\PROGRAM FILES (X86)\OPENOFFICE.ORG 3\URE\BIN";C:\WINDOWS\System32\WindowsPowerShell\v1.0\;H:\Documents\miktex\miktex\bin\

#R CMD SHLIB Neils_ADDM_rewritten.c ziggurat_inline.c
#R CMD BATCH run_ADDM.R

setwd("C:/Anna/addm_writeup")

dyn.load("Neils_ADDM_rewritten.dll")


simulate.rewritten.ADDM <- function(item1, item2, item3, sigma, theta.unattended, speed.of.integration, fix1, fix2, prob, no.simulations, chosen, seed) {
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {abs(x - round(x)) < tol}
  
  #seed <- c(1,2,3,4)
  r <- c(item1, item2, item3)

  
  # Check all is legal
  if(!(is.vector(r) & length(r)>1 & is.numeric(r)))
    stop("r must be a vector of length at least 2")
  if(!(is.numeric(sigma) & length(sigma)==1))
    stop("sigma must be a scalar")
  if(!(is.numeric(theta.unattended) & length(theta.unattended)==1))
    stop("theta.unattended must be a scalar")
  if(!(is.numeric(speed.of.integration) & length(speed.of.integration)==1))
    stop("speed.of.integration must be a scalar")
  if(!all(is.wholenumber(fix1)))
    stop("fixations must be integers")
  if( any(fix1<1) | any(fix1>length(r)))
    stop("fixations must be in range 1...length(r)")
  if(!all(is.wholenumber(fix2)))
    stop("fixations must be integers")
  if( any(fix2<1) | any(fix2>length(r)))
    stop("fixations must be in range 1...length(r)")
  if(!(all(is.wholenumber(no.simulations)) & length(no.simulations)==1))
    stop("no.simulations must be an integer")
  if(!(all(is.wholenumber(seed)) & length(seed)==4))
    stop("seed must be a vector of integers length 4")
  
  
  fix1 <- as.integer(fix1 - 1)
  fix2 <- as.integer(fix2 - 1)# -1 to convert to numbering from 0 (so that we can use the number of the item to refer to their position)
  seed <- as.integer(seed)
  
  tally <- .Call("simulaterewrittenADDM", r, sigma, theta.unattended, speed.of.integration, fix1, fix2, prob, no.simulations, seed)
  
  names(tally) <- c(1:3, "Finished Early", "Not Finished")
 # print(tally)
  as.numeric(tally[chosen]/sum(tally))
 # print(tally)
}

simulate.rewritten.ADDM(item1 = 1, item2 = 6, item3 = 3, sigma=0.2, theta.unattended=0.3, speed.of.integration=0.055, fix1=c(1,1,1,2,2,3), fix2=c(1,1,1,2,2,3), prob = c(1,1,1), chosen = 3, no.simulations=100000, seed = c(1,2,3,4))

#simulate.rewritten.ADDM(r=c(4,3,5), sigma=0.2, theta.unattended=0.5, speed.of.integration=0.1, fix1=c(1,3,2,1,2,3,2), fix2=c(1,2,2,1,2,3,2), prob=c(1,0.45,0.12,1,1,0.45,0.55), no.simulations=15, chosen = 3, seed=c(0,0,0,0))

#system.time(result <- simulate.ADDM(r=c(7,3,5), sigma=0.1, theta.unattended=0.3, speed.of.integration=0.1, fixations=c(1,2,1,3,3,2,1), no.simulations=10000000, seed=c(0,0,0,0)))
#result

