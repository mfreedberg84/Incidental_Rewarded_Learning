# RT analysis for Training (Exp. 2)

rm(list=ls(all=TRUE))
objects()

# These are the starting parameters defined from line 33. 
x= 1224.64	
y= -64.43
z= 0.55

library(nlme)    #Non-linear mixed effects package
library(car)   #Regression package
library(lme4)    #Linear mixed effects package
library(ggplot2)   #Advanced plotting package
library(languageR)  ##Packages containing useful functions for language research 
library(lattice)  ##Plotting package -- need to install!!
#library(lmerTest)  

df = read.table("~/Desktop/Exp2_TrainRT.csv", header = TRUE, sep = ",")
attach(df)
names(df)
summary(df)


options(contrasts=c("contr.sum", "contr.poly"))
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
                     start = c(a = x, b = y, c = z, 5.0, 1, 0.20), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(model_full, comp_model_1) # Switch to model 1 (Drop asymptote random effect)

# Fixed effects of asymptote, magnitude, and rate. Random effects of asymptote and magnitude.
comp_model_2 <- nlme(RT ~ a+(b*exp(-c*session_rev)), fixed=a+b+c~reward, random=pdDiag(b~1), 
                     start = c(a = x, b = y, c = z, 5.0, 1, 0.20), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(comp_model_1, comp_model_2) # Switch to model 2 (Drop rate random effect)

# Fixed effects of asymptote and magnitude. Random effect magnitude.
comp_model_3 <- nlme(RT ~ a+(b*exp(-c*session_rev)), fixed=list(a+b~reward, c~1), random=pdDiag(b~1), 
                     start = c(a = x, b = y, c = z, 5,1), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(comp_model_2, comp_model_3) # Switch to model 3 (Drop rate fixed effect)

# Fixed effect of asymptote. Random effect of magnitude.
comp_model_4 <- nlme(RT ~ a+(b*exp(-c*session_rev)), fixed=list(a~reward, b+c~1), random=pdDiag(b~1), 
                     start = c(a = x, b = y, c = z, 0.1), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(comp_model_3, comp_model_4) # Keep model 3 (Keep magnitude fixed effect)

# Fixed effect of magnitude. Random effect magnitude.
comp_model_5 <- nlme(RT ~ a+(b*exp(-c*session_rev)), fixed=list(b~reward, a+c~1), random=pdDiag(b~1), 
                     start = c(a = x, b = y, c = z, 0.1), data = grouped_MF,  
                     control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
                                  pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(comp_model_3, comp_model_5) # Switch to model 5 (Drop  asymptote fixed effect)

summary(comp_model_5)

# Fixed effects:  list(b ~ reward, a + c ~ 1) 
# Value Std.Error  DF    t-value p-value
# b.(Intercept) -1816.9120  7550.019 638 -0.2406500  0.8099
# b.reward1        21.2249     8.489 638  2.5003241  0.0127 <-- Fixed effect of reward on magnitude
# a              2995.8613  7549.148 638  0.3968476  0.6916
# c                 0.0037     0.016 638  0.2389966  0.8112

g1 <- ggplot(data = grouped_MF, aes(x = session_rev, y = RT, shape = reward))
g2 <- g1 + stat_summary(fun.data=mean_se, geom="pointrange")
g3 <- g2 + stat_summary(aes(y = fitted(comp_model_5), linetype = reward), fun.y = mean, geom="line")
g4 <- g3 + theme_bw() + scale_x_continuous(name = "Block (minus one)")
g5 <- g4 + scale_y_continuous(name = "RT") 
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

