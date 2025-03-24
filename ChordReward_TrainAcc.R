rm(list=ls(all=TRUE))
objects()

x= 0.89	
y= -0.08
z= 0.42

library(nlme)    #Non-linear mixed effects package
library(car)   #Regression package
library(lme4)    #Linear mixed effects package
library(ggplot2)   #Advanced plotting package
library(languageR)  ##Packages containing useful functions for language research 
library(lattice)  ##Plotting package -- need to install!!
#library(lmerTest)  


#MF_data = read.table("A:/Workfiles/Projects/Dopamine/Dopas/Dopa3/StatisticalAnalyses/NLME/ChordReward3Accnc.csv", header = TRUE, sep = ",")
MF_data = read.table("C:/Users/freedbergm/Desktop/Projects/Dopamine/Dopas/Dopa3/StatisticalAnalyses/NLME/ChordReward3Accnc.csv", header = TRUE, sep = ",")
attach(MF_data)
names(MF_data)

#session_rev <- session - 1
MF_data <- cbind(MF_data,session_rev)

options(contrasts=c("contr.sum", "contr.poly"))
#MF_data$chord<-as.factor(MF_data$chord)
MF_data$part<-as.factor(MF_data$Subject)
MF_data$reward<-as.factor(MF_data$Reward)

grouped_MF<-groupedData(ACC ~ session_rev| Subject/Reward, data=MF_data)

model1 <- nls(ACC ~ a+(b*exp(-c*session_rev)), start = c(a = x, b = y, c = z), trace = TRUE)

model5 <- nlsList(RT ~ a+(b*exp(-c*session_rev)), start = c(a = x, b = y, c = z), 
data = grouped_MF, control=list(maxiter=4000, tol = 1e-03, minFactor = 1/10000, warnOnly = FALSE))
summary(model5)

model8 <- nlme(ACC ~ a-(b*exp(-c*session_rev)), fixed=a+b+c~reward, random=pdDiag(a+b+c~1), 
start = c(a = x, b = y, c = z, 5.0, 5.0, 0.20), data = grouped_MF,  
control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

model9 <- nlme(ACC ~ a-(b*exp(-c*session_rev)), fixed=a+b+c~reward, random=pdDiag(b+c~1), 
start = c(a = x, b = y, c = z, 5.0, 5.0, 0.20), data = grouped_MF,  
control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

model10 <- nlme(ACC ~ a-(b*exp(-c*session_rev)), fixed=a+b+c~reward, random=pdDiag(a+b~1), 
start = c(a = x, b = y, c = z, 5.0, 5.0, 0.20), data = grouped_MF,  
control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

model11 <- nlme(ACC ~ a-(b*exp(-c*session_rev)), fixed=list(a+b~reward, c~1), random=pdDiag(a+b~1), 
start = c(a = x, b = y, c = z, rep(0.5,2)), data = grouped_MF,  
control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

model12 <- nlme(ACC ~ a-(b*exp(-c*session_rev)), fixed=list(b+c~reward, a~1), random=pdDiag(b+c~1), 
start = c(a = x, b = y, c = z, rep(0.5,2)), data = grouped_MF,  
control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

model13 <- nlme(ACC ~ a-(b*exp(-c*session_rev)), fixed=list(b+c~reward, a~1), random=pdDiag(a+b+c~1), 
start = c(a = x, b = y, c = z, rep(0.5,2)), data = grouped_MF,  
control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)


anova(model11, model10)

summary(model10)

g1 <- ggplot(data = grouped_MF, aes(x = session_rev, y = ACC, shape = reward))
g2 <- g1 + stat_summary(fun.data=mean_se, geom="pointrange")
g3 <- g2 + stat_summary(aes(y = fitted(model10), linetype = reward), fun.y = mean, geom="line")
g4 <- g3 + theme_bw() + scale_x_continuous(name = "Block (minus one)")
g5 <- g4 + scale_y_continuous(name = "ACC") 
print(g5)

coef(model99)