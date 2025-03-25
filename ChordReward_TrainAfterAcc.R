# Acc analysis for Training (Exp. 1) - Trials after rewarded and unrewarded feedback

rm(list=ls(all=TRUE))
objects()

# These are the starting parameters defined from line 33. 
x= 1.18	
y= 0.35
z= 0.05

library(nlme)    #Non-linear mixed effects package
library(car)   #Regression package
library(lme4)    #Linear mixed effects package
library(ggplot2)   #Advanced plotting package
library(languageR)  ##Packages containing useful functions for language research 
library(lattice)  ##Plotting package -- need to install!!
#library(lmerTest)  

MF_data = read.table("/Users/mvf327/Library/CloudStorage/Box-Box/Home/Data_Science/_github/Incidental_Rewarded_Learning/Exp_1/Training/ChordReward_TrainAfterAcc.csv", header = TRUE, sep = ",")
attach(MF_data)
names(MF_data)
summary(MF_data)


options(contrasts=c("contr.sum", "contr.poly"))
MF_data$part<-as.factor(MF_data$Subject)
MF_data$reward<-as.factor(MF_data$Reward)

grouped_MF<-groupedData(ACC ~ session_rev| Subject/Reward, data=MF_data)

############################## Initial Model for starting parameters ############################

init_model <- nls(ACC ~ a-(b*exp(-c*session_rev)), start = c(a = x, b = y, c = z), trace = TRUE)

############################## Contrasting models ####################################

# Fixed and random effects of asymptote, magnitude, and rate.
model_full <- nlme(ACC ~ a-(b*exp(-c*session_rev)), fixed=a+b+c~reward, random=pdDiag(a+b+c~1), 
                   start = c(a = x, b = y, c = z, 0.1,0.10, 1), data = grouped_MF,  
                   control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

# Fixed effects of asymptote, magnitude, and rate. Random effects of magnitude and rate.
comp_model_1 <- nlme(ACC ~ a-(b*exp(-c*session_rev)), fixed=a+b+c~reward, random=pdDiag(b+c~1), 
                     start = c(a = x, b = y, c = z, 0.1,0.1, 1), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(model_full, comp_model_1) # Keep full model (keep asymptote random effect)

# Fixed effects of asymptote, magnitude, and rate. Random effects of asymptote and magnitude.
comp_model_2 <- nlme(ACC ~ a-(b*exp(-c*session_rev)), fixed=a+b+c~reward, random=pdDiag(a+b~1), 
                     start = c(a = x, b = y, c = z, 0.1,0.3, 1), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(model_full, comp_model_2) # Switch to model 2 (Drop rate random effect)

# Fixed effects of asymptote, magnitude, and rate. Random effect of asymptote. 
comp_model_3 <- nlme(ACC ~ a-(b*exp(-c*session_rev)), fixed=a+b+c~reward, random=pdDiag(a~1), 
                     start = c(a = x, b = y, c = z, 0.3,0.2, 0.1), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(comp_model_2, comp_model_3) # Switch to model 3 (Drop magnitude random effect)

# Fixed effects of asymptote and magnitude. Random effect of asymptote.
comp_model_4 <- nlme(ACC ~ a-(b*exp(-c*session_rev)), fixed=list(a+b~reward, c~1), random=pdDiag(a~1), 
                     start = c(a = x, b = y, c = z, 0.05,0.10), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(comp_model_3, comp_model_4) # Switch to model 4 (Drop rate fixed effect)

# Fixed effect of asymptote. Random effect of asymptote.
comp_model_5 <- nlme(ACC ~ a-(b*exp(-c*session_rev)), fixed=list(a~reward, b+c~1), random=pdDiag(a~1), 
                     start = c(a = x, b = y, c = z, 0.05), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(comp_model_4, comp_model_5) # Switch to model 5 (Drop magnitude fixed effect)

summary(comp_model_5)

# Fixed effects:  list(a ~ reward, b + c ~ 1) 
# Value Std.Error  DF   t-value p-value
# a.(Intercept) 1.2499305 1.8845315 411 0.6632580  0.5075
# a.reward1     0.0142626 0.0087948  31 1.6217102  0.1150
# b             0.4118390 1.8733674 411 0.2198389  0.8261
# c             0.0428598 0.2268001 411 0.1889763  0.8502

g1 <- ggplot(data = grouped_MF, aes(x = session_rev, y = ACC, shape = reward))
g2 <- g1 + stat_summary(fun.data=mean_se, geom="pointrange")
g3 <- g2 + stat_summary(aes(y = fitted(comp_model_5), linetype = reward), fun.y = mean, geom="line")
g4 <- g3 + theme_bw() + scale_x_continuous(name = "Block (minus one)")
g5 <- g4 + scale_y_continuous(name = "ACC") 
print(g5)

# Retrieve coefficients

coef(comp_model_5)

# Specify vif.mer function below (Variance inflation factor)
vif.mer <- function (fit) {
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}
# Run vif.mer function 
vif.mer(comp_model_5)
