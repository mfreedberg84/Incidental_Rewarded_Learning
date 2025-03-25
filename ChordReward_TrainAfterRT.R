# RT analysis for Training (Exp. 1) - Trials after rewarded and unrewarded feedback

rm(list=ls(all=TRUE))
objects()

# These are the starting parameters defined from line 33. 
x= 1210	
y= 174
z= 0.56

library(nlme)    #Non-linear mixed effects package
library(car)   #Regression package
library(lme4)    #Linear mixed effects package
library(ggplot2)   #Advanced plotting package
library(languageR)  ##Packages containing useful functions for language research 
library(lattice)  ##Plotting package -- need to install!!
#library(lmerTest)  

MF_data = read.table("/Users/mvf327/Library/CloudStorage/Box-Box/Home/Data_Science/_github/Incidental_Rewarded_Learning/Exp_1/Training/ChordReward_TrainAfterRT.csv", header = TRUE, sep = ",")
attach(MF_data)
names(MF_data)
summary(MF_data)


options(contrasts =c("contr.sum", "contr.poly"))
MF_data$part<-as.factor(MF_data$Subject)
MF_data$reward<-as.factor(MF_data$Reward)

grouped_MF<-groupedData(RT ~ session_rev| Subject/Reward, data=MF_data)

############################## Initial Model for starting parameters ############################

init_mod <- nls(RT ~ a+(b*exp(-c*session_rev)), start = c(a = x, b = y, c = z), trace = TRUE)

############################## Contrasting models ####################################

# Fixed and random effects of asymptote, magnitude, and rate.
model_full <- nlme(RT ~ a+(b*exp(-c*session_rev)), fixed=a+b+c~reward, random=pdDiag(a+b+c~1), 
                   start = c(a = x, b = y, c = z, 0.5, 0.5, 0.20), data = grouped_MF,  
                   control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

# Fixed effects of asymptote, magnitude, and rate. Random effects of magnitude and rate.
comp_model_1 <- nlme(RT ~ a+(b*exp(-c*session_rev)), fixed=a+b+c~reward, random=pdDiag(b+c~1), 
                     start = c(a = x, b = y, c = z, 0.5, 0.5, 0.20), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(model_full, comp_model_1) # Keep full model (Keep asymptote random effect)

# Fixed effects of asymptote, magnitude, and rate. Random effects of asymptote and magnitude.
comp_model_2 <- nlme(RT ~ a+(b*exp(-c*session_rev)), fixed=a+b+c~reward, random=pdDiag(a+b~1), 
                     start = c(a = x, b = y, c = z, 0.7, 0.7, 1), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(model_full, comp_model_2) # Keep full model (Keep rate random effect)

# Fixed effects of asymptote, magnitude, and rate. Random effects of asymptote and rate.
comp_model_3 <- nlme(RT ~ a+(b*exp(-c*session_rev)), fixed=a+b+c~reward, random=pdDiag(a+c~1), 
                     start = c(a = x, b = y, c = z, 0.7,0.7,), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(model_full, comp_model_3) # Keep  full model (Keep magnitude random effect)

# Fixed effects of asymptote and magnitude. Random effects of asymptote,magnitude, and rate.
comp_model_4 <- nlme(RT ~ a+(b*exp(-c*session_rev)), fixed=list(a+b~reward, c~1), random=pdDiag(a+b+c~1), 
                     start = c(a = x, b = y, c = z, rep(5,2)), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(model_full, comp_model_4) # Keep full model (Keep rate fixed effect)

# Fixed effects of asymptote and rate. Random effects of asymptote, magnitude, and rate.
comp_model_5 <- nlme(RT ~ a+(b*exp(-c*session_rev)), fixed=list(a+c~reward, b~1), random=pdDiag(a+b+c~1), 
                     start = c(a = x, b = y, c = z, 0.5,0.2), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(model_full, comp_model_5) # Keep full model (Keep magnitude fixed effect)

# Fixed effects of magnitude and rate. Random effects of asymptote, magnitude, and rate.
comp_model_6 <- nlme(RT ~ a+(b*exp(-c*session_rev)), fixed=list(b+c~reward, a~1), random=pdDiag(a+b+c~1), 
                     start = c(a = x, b = y, c = z, 0.5,0.2), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(model_full, comp_model_6) # Keep full model (Keep asymptote fixed effect)

summary(model_full)

# Fixed effects:  a + b + c ~ reward 
# Value Std.Error  DF  t-value p-value
# a.(Intercept) 1189.3048 26.620957 443 44.67551  0.0000
# a.reward1      -17.8644  6.197114 443 -2.88270  0.0041 <-- Fixed effect of reward feedback on next trial asymptote
# b.(Intercept)  197.1975 22.955695 443  8.59035  0.0000
# b.reward1       11.0632 10.496515 443  1.05399  0.2925
# c.(Intercept)    0.5014  0.090306 443  5.55252  0.0000
# c.reward1       -0.0540  0.030631 443 -1.76420  0.0784

g1 <- ggplot(data = grouped_MF, aes(x = session_rev, y = RT, shape = reward))
g2 <- g1 + stat_summary(fun.data=mean_se, geom="pointrange")
g3 <- g2 + stat_summary(aes(y = fitted(model_full), linetype = reward), fun.y = mean, geom="line")
g4 <- g3 + theme_bw() + scale_x_continuous(name = "Block (minus one)")
g5 <- g4 + scale_y_continuous(name = "RT") 
print(g5)

# Retrieve coefficients

coef(model_full)

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
vif.mer(model_full)

