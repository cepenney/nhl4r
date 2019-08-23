#### Preamble ####

# library("pracma")
# library("survival")
library("plyr")
# library("dplyr")
# library("MASS")
# library("ggplot2")
# library("grid")
# # library("eha")
# # library("aftgee")
# 
# # library("plm")
# # library("apsrtable")
# # library("car")
# library("gplots")
# # library("ivpack")
# library("foreign")
# library("stringr")
# library("scales")
# library("GGally")
# library("locfit")
library("lpSolve")
# library("adagio")
library("emplik")
library("optimx")
# setwd("~/R/optimization/pers")

dev.off(dev.list()["RStudioGD"])
rm(list=ls())

sample <- read.csv(file="fd112715.csv",header=T,sep=",")

f.obj <- sample$FPPG
# build the constraints matrix as follows:
sample$C <- 0
sample$C <- ifelse(sample$Position=="C",1,sample$C)
sample$LW <- 0
sample$LW <- ifelse(sample$Position=="LW",1,sample$LW)
sample$RW <- 0
sample$RW <- ifelse(sample$Position=="RW",1,sample$RW)
sample$D <- 0
sample$D <- ifelse(sample$Position=="D",1,sample$D)
sample$G <- 0
sample$G <- ifelse(sample$Position=="G",1,sample$G)
sample$FL <- 1

# Set the maximum salary:
sal <- 50000

c.con <- sample$C
lw.con <- sample$LW
rw.con <- sample$RW
d.con <- sample$D
g.con <- sample$G
fl.con <- sample$FL
sal.con <- sample$Salary
#                  1A    1B     2A     2B     3A     3B    4A    4B    5     6      7
constr_A <-rbind(c.con,c.con,lw.con,lw.con,rw.con,rw.con,d.con,d.con,g.con,fl.con,sal.con)
#          1A   1B   2A   2B   3A   3B   4A   4B   5A   6   7
dir_A <- c(">=","<=",">=","<=",">=","<=",">=","<=","=","=","<=")
rhs_A <- c(2   ,3   ,2   ,3   ,2   ,3   ,2   ,3   ,1  ,10 ,sal)
# # # # 

lp("max", f.obj, constr_A, dir_A, rhs_A, all.bin=T)
objval_A <- lp("max", f.obj, constr_A, dir_A, rhs_A, all.bin=T)$objval
soln_A <- lp("max", f.obj, constr_A, dir_A, rhs_A, all.bin=T)$solution
sample$soln_A <- soln_A
lineup_A <- sample[sample$soln_A==1,]

# Now to obtain some "adjacent" bundles
# store the objective function value from the last step (added above)
# simply need to add a constraint saying the new bundle has to be lower than the one just reported

# # for this method I need to use the objective as a constraint as well:
# obj.cons <- sample$FPPG
# 
# #                 1A    1B     2A     2B     3A     3B    4A    4B    5A    5B     6       7       8
# constr_alt <-rbind(c.con,c.con,lw.con,lw.con,rw.con,rw.con,d.con,d.con,g.con,g.con,fl.con,sal.con,obj.cons)
# #          1A   1B   2A   2B   3A   3B   4A   4B   5A   5B   6   7      8
# dir_alt <- c(">=","<=",">=","<=",">=","<=",">=","<=",">=","<=","=","<=","<=")
# rhs_alt <- c(2   ,3   ,2   ,3   ,2   ,3   ,2   ,3   ,1   ,2   ,10  ,50000,(objval_A-0.0001))
# lp("max", f.obj, constr_alt, dir_alt, rhs_alt, all.bin=T)
# objval_alt <- lp("max", f.obj, constr_alt, dir_alt, rhs_alt, all.bin=T)$objval
# soln_alt <- lp("max", f.obj, constr_alt, dir_alt, rhs_alt, all.bin=T)$solution
# sample$soln_alt <- soln_alt
# lineup_alt <- sample[sample$soln_alt==1,]

# # # Alternatively I can try just telling it not to take ALL the same players as before:
#
# Lineup B:
#                 1A    1B     2A     2B     3A     3B    4A    4B    5     6       7       8
constr_B <-rbind(c.con,c.con,lw.con,lw.con,rw.con,rw.con,d.con,d.con,g.con,fl.con,sal.con,soln_A)
#          1A   1B   2A   2B   3A   3B   4A   4B   5   6   7    8
dir_B <- c(">=","<=",">=","<=",">=","<=",">=","<=","=","=","<=","<=")
rhs_B <- c(2   ,3   ,2   ,3   ,2   ,3   ,2   ,3   ,1   ,10 ,sal ,9)
lp("max", f.obj, constr_B, dir_B, rhs_B, all.bin=T)
objval_B <- lp("max", f.obj, constr_B, dir_B, rhs_B, all.bin=T)$objval
soln_B <- lp("max", f.obj, constr_B, dir_B, rhs_B, all.bin=T)$solution
sample$soln_B <- soln_B
lineup_B <- sample[sample$soln_B==1,]

# It may end up being the same as the last lineup based on whether there are ties or not.
# In the end I'm probably best off using this method. Just keep adding constraints making sure
# that the new lineup has no more than 9 of the same players of ANY of the previous lineups.
# This guarantees maximization without repetition.

# Lineup C:
constr_C <-rbind(c.con,c.con,lw.con,lw.con,rw.con,rw.con,d.con,d.con,g.con,fl.con,sal.con,soln_A,soln_B)
#          1A   1B   2A   2B   3A   3B   4A   4B   5   6   7    8    9
dir_C <- c(">=","<=",">=","<=",">=","<=",">=","<=","=","=","<=","<=","<=")
rhs_C <- c(2   ,3   ,2   ,3   ,2   ,3   ,2   ,3   ,1   ,10 ,sal ,9   ,9)
lp("max", f.obj, constr_C, dir_C, rhs_C, all.bin=T)
objval_C <- lp("max", f.obj, constr_C, dir_C, rhs_C, all.bin=T)$objval
soln_C <- lp("max", f.obj, constr_C, dir_C, rhs_C, all.bin=T)$solution
sample$soln_C <- soln_C
lineup_C <- sample[sample$soln_C==1,]

# Lineup D:
constr_D <-rbind(c.con,c.con,lw.con,lw.con,rw.con,rw.con,d.con,d.con,g.con,fl.con,sal.con,soln_A,soln_B,soln_C)
#          1A   1B   2A   2B   3A   3B   4A   4B   5A   6   7    8    9    10
dir_D <- c(">=","<=",">=","<=",">=","<=",">=","<=",">=","=","<=","<=","<=","<=")
rhs_D <- c(2   ,3   ,2   ,3   ,2   ,3   ,2   ,3   ,1   ,10 ,sal ,9   ,9   ,9)
lp("max", f.obj, constr_D, dir_D, rhs_D, all.bin=T)
objval_D <- lp("max", f.obj, constr_D, dir_D, rhs_D, all.bin=T)$objval
soln_D <- lp("max", f.obj, constr_D, dir_D, rhs_D, all.bin=T)$solution
sample$soln_D <- soln_D
lineup_D <- sample[sample$soln_D==1,]

