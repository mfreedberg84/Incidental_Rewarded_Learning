# Acc analysis for Training (Exp. 1)

rm(list=ls(all=TRUE))
objects()

# These are the starting parameters defined from line 33. 
x= 0.88	
y= 0.08
z= 0.55

library(nlme)    #Non-linear mixed effects package
library(car)   #Regression package
library(lme4)    #Linear mixed effects package
library(ggplot2)   #Advanced plotting package
library(languageR)  ##Packages containing useful functions for language research 
library(lattice)  ##Plotting package -- need to install!!
#library(lmerTest)  

df = read.table("~/Desktop/Exp1_TrainAcc.csv", header = TRUE, sep = ",")
attach(df)
names(df)
summary(df)


options(contrasts=c("contr.sum", "contr.poly"))
df$part<-as.factor(df$Subject)
df$reward<-as.factor(df$Reward)

grouped_MF<-groupedData(ACC ~ session_rev| Subject/Reward, data=df)

############################## Initial Model for starting parameters ############################

init_model <- nls(ACC ~ a-(b*exp(-c*session_rev)), start = c(a = x, b = y, c = z), trace = TRUE)

############################## Contrasting models ####################################

# Fixed and random effects of asymptote, magnitude, and rate.
model_full <- nlme(ACC ~ a-(b*exp(-c*session_rev)), fixed=a+b+c~reward, random=pdDiag(a+b+c~1), 
                   start = c(a = x, b = y, c = z, 0.05,0.10, 1), data = grouped_MF,  
                   control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

# Fixed effects of asymptote, magnitude, and rate. Random effects of magnitude and rate.
comp_model_1 <- nlme(ACC ~ a-(b*exp(-c*session_rev)), fixed=a+b+c~reward, random=pdDiag(b+c~1), 
                     start = c(a = x, b = y, c = z, 0.1,0.3, 1), data = grouped_MF,  
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

anova(comp_model_3, comp_model_4) # Keep model 3 (Keep rate fixed effect)

# Fixed effects of asymptote and rate. Random effects of asymptote and magnitude.
comp_model_5 <- nlme(ACC ~ a-(b*exp(-c*session_rev)), fixed=list(a+c~reward, b~1), random=pdDiag(a+b~1), 
                     start = c(a = x, b = y, c = z, 0.05,1), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(comp_model_3, comp_model_5) # Keep model 3 (Keep magnitude fixed effect)

# Fixed effects of magnitude and rate. Random effects of asymptote and magnitude.
comp_model_6 <- nlme(ACC ~ a-(b*exp(-c*session_rev)), fixed=list(b+c~reward, a~1), random=pdDiag(a+b~1), 
                     start = c(a = x, b = y, c = z, 0.10,1), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(comp_model_3, comp_model_6) # Switch to model 6 (Drop asymptote fixed effect)

summary(comp_model_6)

# Fixed effects:  list(b + c ~ reward, a ~ 1) 
# Value  Std.Error  DF  t-value p-value
# b.(Intercept) 0.0798076 0.01399313 444  5.70334  0.0000
# b.reward1     0.0238398 0.01268189 444  1.87983  0.0608 <-- Trend effect of reward on magnitude
# c.(Intercept) 0.7376421 0.23442261 444  3.14663  0.0018
# c.reward1     0.0861823 0.18925142 444  0.45539  0.6491
# a             0.8766119 0.01175191 444 74.59311  0.0000


g1 <- ggplot(data = grouped_MF, aes(x = session_rev, y = ACC, shape = reward))
g2 <- g1 + stat_summary(fun.data=mean_se, geom="pointrange")
g3 <- g2 + stat_summary(aes(y = fitted(comp_model_6), linetype = reward), fun.y = mean, geom="line")
g4 <- g3 + theme_bw() + scale_x_continuous(name = "Block (minus one)")
g5 <- g4 + scale_y_continuous(name = "ACC") 
print(g5)

# Retrieve coefficients

coef(comp_model_6)

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
vif.mer(comp_model_6)