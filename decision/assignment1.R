rm(list=ls())

library(lpSolveAPI)
library(dplyr)
library(tidyr)

#funzioni custom
printSensitivityRHS <- function(model){
  options(scipen = 999)
  arg.rhs <- get.sensitivity.rhs(model)
  numRows <- length(arg.rhs$duals)
  symb <- c()
  for (i in c(1:numRows)) symb[i] <- paste("B", i, sep = "")
  
  rhs <- data.frame(rhs = symb, arg.rhs)
  
  rhs <- rhs %>%
    mutate(dualsfrom=replace(dualsfrom, dualsfrom < -1.0e4, "-inf")) %>%
    mutate(dualstill=replace(dualstill, dualstill > 1.0e4, "inf")) %>%
    unite(col = "Sensitivity",  
          dualsfrom, 
          rhs, 
          dualstill , 
          sep = " <= ", remove = FALSE) %>%
    select(c("rhs","Sensitivity"))
  colnames(rhs)[1] <- c('Rhs') 
  print(rhs)
}
printSensitivityObj <- function(model){
  options(scipen=999)
  arg.obj = get.sensitivity.obj(model)
  
  numRows <- length(arg.obj$objfrom)
  symb <- c() 
  for (i in c(1:numRows)) symb[i] <- paste("C", i, sep = "" ) 
  
  obj <- data.frame(Objs = symb, arg.obj)
  
  obj<-
    obj %>%
    mutate(objfrom=replace(objfrom, objfrom < -1.0e4, "-inf")) %>%
    mutate(objtill=replace(objtill, objtill > 1.0e4, "inf")) %>%
    unite(col = "Sensitivity",  
          objfrom, Objs, objtill , 
          sep = " <= ", remove = FALSE) %>%
    select(c("Objs","Sensitivity"))
  print(obj)
}

#modello a 12 variabili X_ij dove i è il cargo type e j il vagone
model <- make.lp(0,12)

#obiettivo massimizzazione
lp.control(model,sense='max')
#costruzione della funzione obiettivo con coefficienti profitti marginali 
objective <- rep(c(3500,2500,2000),4)
set.objfn(model, obj = objective)

#tutti i valori maggiori o uguali a zero
set.bounds(model,lower=rep(0,12))

#x11,x21,x31,x12,x22,x23,x31,x32,x33,x14,x24,x34

#vincoli di capacità vagone
add.constraint(model,
               xt=c(1,1,1),
               type="<=",rhs=10,
               indices=c(1:3))
add.constraint(model,
               xt=c(1,1,1),
               type="<=",rhs=8,
               indices=c(4:6))
add.constraint(model,
               xt=c(1,1,1),
               type="<=",rhs=12,
               indices=c(7:9))
add.constraint(model,
               xt=c(1,1,1),
               type="<=",rhs=6,
               indices=c(10:12))

#vincoli di capacità volume
add.constraint(model,
               xt=c(500,300,400),
               type="<=",rhs=5000,
               indices=c(1:3))
add.constraint(model,
               xt=c(500,300,400),
               type="<=",rhs=4000,
               indices=c(4:6))
add.constraint(model,
               xt=c(500,300,400),
               type="<=",rhs=8000,
               indices=c(7:9))
add.constraint(model,
               xt=c(500,300,400),
               type="<=",rhs=2500,
               indices=c(10:12))

#vinconibli di disponibilità cargo
add.constraint(model,
               xt=c(1,1,1,1),
               type="<=",rhs=20,
               indices=c(1,4,7,10))
add.constraint(model,
               xt=c(1,1,1,1),
               type="<=",rhs=10,
               indices=c(2,5,8,11))
add.constraint(model,
               xt=c(1,1,1,1),
               type="<=",rhs=18,
               indices=c(3,6,9,12))


write.lp(model,'model.lp')

#solving

solution <- solve(model)

get.objective(model)
get.variables(model)
get.constraints(model)
#10,8,12,6,5000,4000,8000,2500,20,10,18
#                     X    X          X

library(tidyverse)
tons=get.variables(model)
get.variables(model)
wagon=rep(c('Wagon 1','Wagon 2','Wagon 3','Wagon 4'),rep(3,4))
cargo=rep(c('Cargo 1','Cargo 2','Cargo 3'),4)

df=data.frame(tons,cargo,wagon)
df$cargo <- as.factor(df$cargo)
df$wagon <-as.factor(df$wagon)

df <- df%>%
  arrange(wagon, rev(cargo))

df <- df %>%
  group_by(wagon) %>%
  mutate(label_y = cumsum(tons) - 0.5 * tons)

ggplot(data = df, aes( x = wagon, y = tons, fill = cargo ) ) +    # print bar chart
  geom_bar( stat = 'identity')


prova=tons[which(tons==0),]<-NULL
#vincoli capacità vagone ok, vincoli volume vagone 2 su 4, vincoli disponilità 2 su 3 

#sensitivity
get.primal.solution(model) #obj value, constraint values,decision variables 

get.basis(model,nonbasic = F) #optimal basis

printSensitivityRHS <- function(model){
  options(scipen = 999)
  arg.rhs <- get.sensitivity.rhs(model)
  numRows <- length(arg.rhs$duals)
  symb <- c()
  for (i in c(1:numRows)) symb[i] <- paste("B", i, sep = "")
  
  rhs <- data.frame(rhs = symb, arg.rhs)
  
  rhs <- rhs %>%
    mutate(dualsfrom=replace(dualsfrom, dualsfrom < -1.0e4, "-inf")) %>%
    mutate(dualstill=replace(dualstill, dualstill > 1.0e4, "inf")) %>%
    unite(col = "Sensitivity",  
          dualsfrom, 
          rhs, 
          dualstill , 
          sep = " <= ", remove = FALSE) %>%
    select(c("rhs","Sensitivity"))
  colnames(rhs)[1] <- c('Rhs') 
  print(rhs)
}
printSensitivityObj <- function(model){
  options(scipen=999)
  arg.obj = get.sensitivity.obj(model)
  
  numRows <- length(arg.obj$objfrom)
  symb <- c() 
  for (i in c(1:numRows)) symb[i] <- paste("C", i, sep = "" ) 
  
  obj <- data.frame(Objs = symb, arg.obj)
  
  obj<-
    obj %>%
    mutate(objfrom=replace(objfrom, objfrom < -1.0e4, "-inf")) %>%
    mutate(objtill=replace(objtill, objtill > 1.0e4, "inf")) %>%
    unite(col = "Sensitivity",  
          objfrom, Objs, objtill , 
          sep = " <= ", remove = FALSE) %>%
    select(c("Objs","Sensitivity"))
  print(obj)
}

printSensitivityRHS(model)
printSensitivityObj(model)

get.dual.solution(model) #shadow price

#fare un check su shadow price
#
set.constr.value(model, constraints = 9, rhs = 22) #20

solve(model)
get.variables(model)
get.objective(model)
get.basis(model,nonbasic = F)

