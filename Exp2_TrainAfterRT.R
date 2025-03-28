# RT analysis for Training (Exp. 2) - Trials after rewarded and unrewarded feedback

rm(list=ls(all=TRUE))
objects()

# These are the starting parameters defined from line 33. 
x= 1218	
y= -43.55
z= 0.37

library(nlme)    #Non-linear mixed effects package
library(car)   #Regression package
library(lme4)    #Linear mixed effects package
library(ggplot2)   #Advanced plotting package
library(languageR)  ##Packages containing useful functions for language research 
library(lattice)  ##Plotting package -- need to install!!
#library(lmerTest) 

df = read.table("~/Desktop/Exp2_TrainAfterRT.csv", header = TRUE, sep = ",")
attach(df)
names(df)
summary(df)


options(contrasts =c("contr.sum", "contr.poly"))
df$part<-as.factor(df$Subject)
df$reward<-as.factor(df$Reward)

grouped_MF<-groupedData(RT ~ session_rev| Subject/Reward, data=df)

############################## Initial Model for starting parameters ############################

init_mod <- nls(RT ~ a+(b*exp(-c*session_rev)), start = c(a = x, b = y, c = z), trace = TRUE)

############################## Contrasting models ####################################

# Fixed and random effects of asymptote, magnitude, and rate.
model_full <- nlme(RT ~ a+(b*exp(-c*session_rev)), fixed=a+b+c~reward, random=pdDiag(a+b+c~1), 
                   start = c(a = x, b = y, c = z, 5,1, 0.20), data = grouped_MF,  
                   control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

# Fixed effects of asymptote, magnitude, and rate. Random effects of magnitude and rate.
comp_model_1 <- nlme(RT ~ a+(b*exp(-c*session_rev)), fixed=a+b+c~reward, random=pdDiag(b+c~1), 
                     start = c(a = x, b = y, c = z, 5,1, 0.20), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(model_full, comp_model_1) # Keep full model (Keep asymptote random effect)

# Fixed effects of asymptote, magnitude, and rate. Random effects of asymptote and magnitude.
comp_model_2 <- nlme(RT ~ a+(b*exp(-c*session_rev)), fixed=a+b+c~reward, random=pdDiag(a+b~1), 
                     start = c(a = x, b = y, c = z, 5,1,0.01), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(model_full, comp_model_2) # Switch to model 2 (Drop rate random effect)

# Fixed effects of asymptote, magnitude, and rate. Random effect of asymptote.
comp_model_3 <- nlme(RT ~ a+(b*exp(-c*session_rev)), fixed=a+b+c~reward, random=pdDiag(a~1), 
                     start = c(a = x, b = y, c = z, 50,1,0.2), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(comp_model_2, comp_model_3) # Keep  model 2 (Keep magnitude random effect)

# Fixed effects of asymptote and magnitude. Random effects of asymptote and magnitude.
comp_model_4 <- nlme(RT ~ a+(b*exp(-c*session_rev)), fixed=list(a+b~reward, c~1), random=pdDiag(a+b~1), 
                     start = c(a = x, b = y, c = z, 5,1), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(comp_model_2, comp_model_4) # Keep model 2 (Keep rate fixed effect)

# Fixed effects of asymptote and rate. Random effects of asymptote and magnitude.
comp_model_5 <- nlme(RT ~ a+(b*exp(-c*session_rev)), fixed=list(a+c~reward, b~1), random=pdDiag(a+b~1), 
                     start = c(a = x, b = y, c = z, 0.00001,0.05), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(comp_model_2, comp_model_5) # Keep model 2 (Keep magnitude fixed effect)

# Fixed effects of magnitude and rate. Random effects of asymptote and magnitude.
comp_model_6 <- nlme(RT ~ a+(b*exp(-c*session_rev)), fixed=list(b+c~reward, a~1), random=pdDiag(a+b~1), 
                     start = c(a = x, b = y, c = z, 1,0.05), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(comp_model_2, comp_model_6) # Keep model 2 (Keep asymptote fixed effect)

summary(comp_model_2)

# Fixed effects:  a + b + c ~ reward 
# Value Std.Error  DF  t-value p-value
# a.(Intercept) 1211.9694  30.96333 639 39.14209  0.0000
# a.reward1       -9.5757   7.97352 639 -1.20094  0.2302
# b.(Intercept)  -34.1035  49.16774 639 -0.69362  0.4882
# b.reward1       13.6960  21.46033 639  0.63820  0.5236
# c.(Intercept)    0.6746   0.10913 639  6.18107  0.0000
# c.reward1        0.1910   0.07839 639  2.43682  0.0151 <-- Fixed effect of reward on rate

g1 <- ggplot(data = grouped_MF, aes(x = session_rev, y = RT, shape = reward))
g2 <- g1 + stat_summary(fun.data=mean_se, geom="pointrange")
g3 <- g2 + stat_summary(aes(y = fitted(comp_model_2), linetype = reward), fun.y = mean, geom="line")
g4 <- g3 + theme_bw() + scale_x_continuous(name = "Block (minus one)")
g5 <- g4 + scale_y_continuous(name = "RT") 
print(g5)

# Retrieve coefficients

coef(comp_model_2)

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
vif.mer(comp_model_2)

