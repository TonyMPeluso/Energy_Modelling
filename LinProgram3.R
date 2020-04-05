#pkgs.desired <- c("networkD3","lpSolve", "purrr");
#myRepoURL <- "file:////fld6filer/packagerepo-depotprogiciel/miniCRAN/";
#install.packages(
#  pkgs  = pkgs.desired,
#  repos = myRepoURL
#);
install.packages("networkD3")
install.packages("lpsolve")
install.packages("purrr")
library(lpSolve)
library(networkD3)
library(purrr)
options(max.print=999999)
nodes = as.data.frame(matrix(c(
  0, "Electricity Imports", "Supply",
  1, "Diesel Import", "Supply",
  2, "Wind Power", "Supply",
  3, "Diesel Power Gen.", "Transformation",
  4, "Storage", "Storage",
  5, "Diesel for Transportation", "Demand",
  6, "Electricity for Transportation", "Demand",
  7, "Electricity for Residential", "Demand"),
  byrow = TRUE, ncol = 3))
names(nodes) = c("NodeNo", "NodeName", "NodeGroup")
links = as.data.frame(matrix(c(
  0, 4, 1, #Elec. imp. to storage
  0, 7, 1, #Elec. imp. to elec. resid
  1, 3, 1, #Diesel imp. to diesel power gen
  1, 5, 1, #Diesel imp. to diesel transp
  2, 4, 1, #Wind to storage
  2, 7, 1, #Wind to elec. resid
  3, 4, 1, #Diesel gen. to storage
  3, 7, 1, #Diesel gen. to elec. resid
  4, 6, 1, #Storage elec. transp.
  4, 7, 1), #Storage elec. resid.
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "NodeName",
              fontSize= 20, nodeWidth = 30)
#
#Show graphs of time slices for electricity demands and wind availability
#
par(mfrow=c(2,2))
barplot(c(100000, 100000, 100000, 200000), main="Resid. Elec. Demand by Time Slice, kWh")
barplot(c(100000, 0, 0, 100000), main="Transp. Elec. Demand by Time Slice, kWh")
barplot(c(0, 80000, 80000, 0), main="Wind Availability by Time Slice, kWh")
#
#Defining variable names for linear program
#
VarNames <- as.list(c("ImpDieTra", "ImpDieGen", "ImpElResid", "ImpElStor", 
              "GenDie", "GenWin",  
              "StorOp", "StorEn",
              "AddImpEl", "AddGenDie", "AddGenWin", 
              "WithdrTraElec", "WithdrResidElec",  
              "TraElec", "TraDie", "ResidElec"))
length(VarNames)
#
#Defining header names for all 4 time slices
#
Head1 <- c(NA)
for (j in 1:4){
  for (i in 1:length(VarNames)){
  Head2 <- paste(VarNames[i], j, sep=".")
  Head1 <- c(Head1, Head2) 
    }
}
Head <- Head1[-c(1)]
Headnames <- Head1[-c(1)]
length(Headnames)
#
#Build up cost function cx for a single time slice cx1
#
cx1 <- matrix(c(rep(0,length(VarNames))), nrow=1, ncol=length(VarNames))
dimnames(cx1) = list(NULL, VarNames)
#
#Assigning costs for diesel to diesel and electricity imports to cx1
#
NumCol <- length(VarNames)*4
NumCol
#cx1[,c(1,2)] <- 0.233      #Cost of diesel imports
#dim(cx1)
cx1[,c("ImpDieTra", "ImpDieGen")] <- 0.185  #Cost of diesel imports
cx1[,c("ImpElResid", "ImpElStor")] <- 0.225 #cost of electricity imports
cx <- matrix(c(rep(cx1, 4)), ncol=NumCol)
dimnames(cx) = list(NULL,Headnames)
#cx
#
#Diesel balance: diesel imports are equal to sum of diesel generation
#(divided by thermodynamic efficiency) and diesel for transp. for each time slice
#
cBalDie1 <- c(rep(0, length(VarNames)))
cBalDie1[,c("ImpDieTra")] <- 1
cBalDie1[,c("ImpDieGen", "TraDie")] <- -1
BalDie.A <- matrix(
    c(cBalDie1, c(rep(0, length(VarNames)*3))),
    c(rep(0, length(VarNames)), cBalDie1, c(rep(0, length(VarNames)*2))),
    c(rep(0, length(VarNames)*2), cBalDie1, c(rep(0, length(VarNames))),
    c(rep(0, length(VarNames)*3), cBalDie1), ncol=length(VarNames)*4, byrow=TRUE)
BalDie.b <- matrix(c(rep(0,4)), ncol=1)
BalDie.sgn <- matrix(c(rep("=", 4)), ncol=1)
#
#Elec. balance: sum of elec. imports, diesel generation, wind  and 
#two withdrawalS from storage is equal to sum of three additions to storage and
#elec. demand for transp. and residential for each time slice
# 
cBalElec <- c(0, 1, 1, 1, 0, -1, -1, -1, 1, 1, 0, 1, 0, 1)
BalElec.A <- matrix(
  c(cBalElec, c(rep(0, 42)), 
    c(rep(0, 14)), cBalElec, c(rep(0, 28)),
    c(rep(0, 28)), cBalElec, c(rep(0, 14)),
    c(rep(0, 42)), cBalElec), ncol=56, byrow=TRUE)
BalElec.b <- matrix(c(rep(0,4)), ncol=1)
BalElec.sgn <- matrix(c(rep("=", 4)), ncol=1)
#
#Elec. demand for residential is met by elec. imports, wind power,
#diesel generation and withdrawal from storage for each time slice
#
cBalElRes <- c(0, 1, 1, 1, 0, 0, 0, 0, 0, 1 )
BalElRes.A <- matrix(
  c(cBalElecRes, c(rep(0, 42)), 
    c(rep(0, 14)), cBalElecRes, c(rep(0, 28)),
    c(rep(0, 28)), cBalElecRes, c(rep(0, 14)),
    c(rep(0, 42)), cBalElecRes), ncol=56, byrow=TRUE)
DemElRes.b <- matrix(c(rep(100000,3), 200000), ncol=1)
DemElRes.sgn <- matrix(c(rep(">", 4)), ncol=1)
#
#Elec. demand for transp. is met
#
DemElTra.A <- matrix(
  c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, c(rep(0, 33)), 
    c(rep(0, 11)), 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, c(rep(0, 22)),
    c(rep(0, 22)), 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, c(rep(0, 11)),
    c(rep(0, 33)), 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), ncol=44, byrow=TRUE)
DemElTra.b
DemElTra.sgn
#
#Electricity demand for transp. is sourced from withdrawal from storage
#
#
#Diesel demand for transportation is met
#
DemDieTr.A <- matrix(
          c(0, 0, 0, 0, 0, 0, 0, 1, c(rep(0, 24)), 
            c(rep(0, 8)), 0, 0, 0, 0, 0, 0, 0, 1, c(rep(0, 16)),
            c(rep(0, 16)), 0, 0, 0, 0, 0, 0, 0, 1, c(rep(0, 8)), 
            c(rep(0, 24)), 0, 0, 0, 0, 0, 0, 0, 1), ncol=32, byrow=TRUE)
DemTr.b <- matrix(c(rep(200000,4)), ncol=1)

DemTr.sgn <- matrix(c(rep(">", 4)), ncol=1)
#DemEl.sgn
#Diesel generating capacity is not exceeded
CapDie.A <- matrix(
          c(0, 1, 0, 0, 0, 0, 0, 0, c(rep(0, 24)), 
            c(rep(0,8)), 0, 1, 0, 0, 0, 0, 0, 0, c(rep(0, 16)), 
            c(rep(0, 16)), 0, 1, 0, 0, 0, 0, 0, 0, c(rep(0, 8)), 
            c(rep(0, 24)), 0, 1, 0, 0, 0, 0, 0, 0), ncol=32, byrow=TRUE)
#CapDie.A
CapDie.b <- matrix(c(rep(80000,4)), ncol=1)
CapDie.sgn <- matrix(c(rep("<", 4)), ncol=1)
#CapDie.sgn
#Wind availability is not exceeded
AvWin.A <- matrix(
          c(0, 0, 1, 0, 0, 0, 0, 0, c(rep(0, 24)),
            c(rep(0,8)), 0, 0, 1, 0, 0, 0, 0, 0, c(rep(0, 16)),
           c(rep(0, 16)), 0, 0, 1, 0, 0, 0, 0, 0, c(rep(0, 8)), 
           c(rep(0, 24)), 0, 0, 1, 0, 0, 0, 0, 0), ncol=32, byrow=TRUE)
#AvWin.A
AvWin.b <- matrix(c(0,rep(80000,2), 0), ncol=1)
AvWin.sgn <- matrix(c(rep("<", 4)), ncol=1)
#AvWin.sgn
#Storage capacity not exceeded
CapStor1.A <- matrix(
          c(0, 0, 0, 1, 0, 0, 0, 0, c(rep(0, 24)),
            c(rep(0,8)), 0, 0, 0, 1, 0, 0, 0, 0, c(rep(0, 16)),
            c(rep(0, 16)), 0, 0, 0, 1, 0, 0, 0, 0, c(rep(0, 8)), 
            c(rep(0, 24)), 0, 0, 0, 1, 0, 0, 0, 0), ncol=32, byrow=TRUE)
#CapStor1.A
CapStor1.b <- matrix(c(rep(100000,4)), ncol=1)
CapStor1.sgn <- matrix(c(rep("<", 4)), ncol=1)
CapStor2.A <- matrix(
            c(0, 0, 0, 0, 0, 1, 0, 0, c(rep(0, 24)),
              c(rep(0,8)), 0, 0, 0, 0, 0, 1, 0, 0, c(rep(0, 16)),
              c(rep(0, 16)), 0, 0, 0, 0, 0, 1, 0, 0, c(rep(0, 8)), 
              c(rep(0, 24)), 0, 0, 0, 0, 0, 1, 0, 0), ncol=32, byrow=TRUE)
#CapStor2.A
CapStor2.b <- CapStor1.b
CapStor2.sgn <- CapStor1.sgn
#Storage balance is maintained
BalStor.A <- matrix( 
            c(0, 0, 1, 1, -1, -1, 0, 0, c(rep(0, 24)),
              c(rep(0,8)), 0, 0, 1, 1, -1, -1, 0, 0, c(rep(0, 16)),
              c(rep(0, 16)), 0, 0, 1, 1, -1, -1, 0, 0, c(rep(0, 8)), 
              c(rep(0, 24)), 0, 0, 1, 1, -1, -1, 0, 0), ncol=32, byrow=TRUE)
#StorBal.A
StorBal.b <- matrix(c(rep(0,4)), ncol=1)
StorBal.sgn <- matrix(c(rep("=", 4)), ncol=1)
#Continuity of ending storage and next period's o storagepenins
StorCont.A <- matrix( 
            c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, c(rep(0, 16)), 
              c(rep(0,8)), 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, c(rep(0, 8)),
              c(rep(0,16)), 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 
              0, 0, 0, -1, 0, 0, 0, 0, c(rep(0, 16)), 0, 0, 0, 0, 0, 1, 0, 0), ncol=32, byrow=TRUE)
StorCont.A
StorCont.b <- matrix(c(rep(0,4)), ncol=1)
StorCont.sgn <- matrix(c(rep("=", 4)), ncol=1)
Ax <- rbind(DemEl.A, DemTr.A, CapDie.A, AvWin.A, CapStor1.A, CapStor2.A, BalStor.A, StorCont.A)
Ax
sgn <- rbind(DemEl.sgn, DemTr.sgn, CapDie.sgn, AvWin.sgn, CapStor1.sgn, CapStor2.sgn, BalStor.sgn, StorCont.sgn)
sgn
b <- rbind(DemEl.b, DemTr.b, CapDie.b, AvWin.b, CapStor1.b, CapStor2.b, BalStor.b, StorCont.b)
b
lp("min",cx, Ax, sgn, b)
# Variables final values
lp("min",cx, Ax, sgn, b)$solution
# Dual Values (first dual of the constraints and then dual of the variables)
lp("min",cx, Ax, sgn, b)$duals
              
