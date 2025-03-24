# RT analysis for Training (Exp. 1)

rm(list=ls(all=TRUE))
objects()

# These are the starting parameters defined from line 33. 
x= 1214	
y= 169
z= 0.56

library(nlme)    #Non-linear mixed effects package
library(car)   #Regression package
library(lme4)    #Linear mixed effects package
library(ggplot2)   #Advanced plotting package
library(languageR)  ##Packages containing useful functions for language research 
library(lattice)  ##Plotting package -- need to install!!
#library(lmerTest)  

MF_data = read.table("/Users/mvf327/Library/CloudStorage/Box-Box/Home/Data_Science/_github/Incidental_Rewarded_Learning/Exp_1/ChordReward_TrainRT.csv", header = TRUE, sep = ",")
attach(MF_data)
names(MF_data)
summary(MF_data)


options(contrasts=c("contr.sum", "contr.poly"))
MF_data$part<-as.factor(MF_data$Subject)
MF_data$reward<-as.factor(MF_data$Reward)

grouped_MF<-groupedData(RT ~ session_rev| Subject/Reward, data=MF_data)

############################## Initial Model for starting parameters ############################

init_mod <- nls(RT ~ a+(b*exp(-c*session_rev)), start = c(a = x, b = y, c = z), trace = TRUE)

############################## Contrasting models ####################################

model_full <- nlme(RT ~ a+(b*exp(-c*session_rev)), fixed=a+b+c~reward, random=pdDiag(a+b+c~1), 
start = c(a = x, b = y, c = z, 5.0, 5.0, 0.20), data = grouped_MF,  
control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

comp_model_1 <- nlme(RT ~ a+(b*exp(-c*session_rev)), fixed=a+b+c~reward, random=pdDiag(b+c~1), 
start = c(a = x, b = y, c = z, 5.0, 5.0, 0.20), data = grouped_MF,  
control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(model_full, comp_model_1) # Keep full model

comp_model_2 <- nlme(RT ~ a+(b*exp(-c*session_rev)), fixed=a+b+c~reward, random=pdDiag(a+b~1), 
start = c(a = x, b = y, c = z, 5.0, 5.0, 0.20), data = grouped_MF,  
control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)
# Model fail

comp_model_3 <- nlme(RT ~ a+(b*exp(-c*session_rev)), fixed=list(a+b~reward, c~1), random=pdDiag(a+b~1), 
start = c(a = x, b = y, c = z, rep(0.5,2)), data = grouped_MF,  
control=list(maxIter=100, pnlsMaxIter = 7, msMaxIter = 100, minScale = .00001, 
pnlsTol = .1, returnObject = TRUE, msVerbose = TRUE), verbose = TRUE)

anova(model_full, comp_model_3) # Go with cop 3

summary(comp_model_3)

# Fixed effects:  list(a + b ~ reward, c ~ 1) 
# Value Std.Error  DF  t-value p-value
# a.(Intercept) 1215.2081 26.212662 444 46.35958  0.0000
# a.reward1       13.1571  5.381505 444  2.44487  0.0149 <-- Lower asymptote for rewarded chords
# b.(Intercept)  167.6279 20.267132 444  8.27092  0.0000
# b.reward1      -22.8410 10.924688 444 -2.09077  0.0371 <-- Greater magnitude for rewarded chords
# c                0.5516  0.085813 444  6.42793  0.0000

g1 <- ggplot(data = grouped_MF, aes(x = session_rev, y = RT, shape = reward))
g2 <- g1 + stat_summary(fun.data=mean_se, geom="pointrange")
g3 <- g2 + stat_summary(aes(y = fitted(comp_model_3), linetype = reward), fun.y = mean, geom="line")
g4 <- g3 + theme_bw() + scale_x_continuous(name = "Block (minus one)")
g5 <- g4 + scale_y_continuous(name = "RT") 
print(g5)

# Retrieve coefficients

coef(model8)

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
vif.mer(comp_model_3)

