# 1)

# a)

url <- "https://ww2.amstat.org/publications/jse/datasets/fruitfly.dat.txt"
data <- read.table(url)
data <- data[,c(-1,-6)] # remove id and sleep
names(data) <- c("partners","type","longevity","thorax")
pairs(data)

# b) Make a scatterplot of longevity versus thorax, using colors for the number of females and different plotting symbols for the different types of females

head(data)

# to not repeat the name of the dataset anymore
attach(data)

# define colours for the number of females
col.partn <- 1*(partners==0) + 2*(partners==1) + 3*(partners==8)

# define symbols for the different types of females
sym.partn <- 1*(type==0) + 2*(type==1) + 3*(type==9)

plot(thorax, longevity, col=col.partn, pch = sym.partn,
     ylim = range(longevity), xlim = range(thorax))

legend("topleft", c("1 pregnant", "8 pregnant", "1 virgin", "8 virgin",
                    # pch = female type
                    "0 partners"), pch = c(1, 2, 1, 2, 3), col = c(2, 2, 3, 3, 1))

# c) 

# col = 1, pch = 3 => 0 partners
par(mfrow = c(3, 1))
plot(thorax[partners == 0], longevity[partners == 0], pch = 3, col = 1,
     main = "0 partners", ylim = range(longevity), xlim = range(thorax))

plot(thorax[partners == 1], longevity[partners == 1], pch = sym.partn[partners == 1],
     col = 2, main = "1 partner")
legend("topleft", c("pregnant", "virgin"), pch = c(1, 2), col = 2)

plot(thorax[partners == 8], longevity[partners == 8], pch = sym.partn[partners == 8],
     col = 3, main = "8 partners")
legend("topleft", c("pregnant", "virgin"), pch = c(1, 2), col = 3)

# male fruitflies with pregnant females tend to live longer than those with virgin 
# females. The difference in lifespan is larger when the no of partners = 8 vs when
# the number of partners = 1 => we have interaction between no of partners
# and type of partner.

# d)

# create dummy variables for the different groups
zero_p <- 1 * (partners == 0)
one_p_v <- 1 * (partners == 1) * (type == 1)
one_p_p <- 1 * (partners == 1) * (type == 0)
eight_p_v <- 1 * (partners == 8) * (type == 1)
eight_p_p <- 1 * (partners == 8) * (type == 0)

par(mfrow=c(1,1))
boxplot(thorax[zero_p == 1], thorax[one_p_v == 1], thorax[one_p_p == 1],
        thorax[eight_p_v == 1], thorax[eight_p_p == 1])

# we want to test if thorax length is significantly different between at least 2 of the groups
full_model <- lm(thorax ~ one_p_p + one_p_v + eight_p_v + eight_p_p) # full model
model_with_intercept <- lm(thorax ~ 1) # model with intercept only

anova(model_with_intercept, full_model) # there is no significant difference between the groups in terms of thorax

# e)
# Test the effect of type of female on longevity for the two groups with 1 female
lm1 <- lm(longevity[partners == 1] ~ factor(type[partners == 1]))
lm2 <- lm(longevity[partners == 1] ~ factor(type[partners == 1]) + thorax[partners==1])
summary(lm1)
summary(lm2)

# f)

part <- as.factor(partners)
typ <- as.factor(type)
model <- lm(longevity ~ part + typ + part * typ + thorax)
summary(model)

# g)

# we first run the full model (which includes interaction between type and partners)
full_model <- lm(longevity ~ thorax + one_p_p + one_p_v + eight_p_v + eight_p_p)
no_interaction_model <- lm(longevity ~ thorax + I(one_p_p + one_p_v) + I(one_p_p + eight_p_p) + I(eight_p_v - one_p_p))

# the partial F-test shows that the interaction between type and partners is significant
anova(no_interaction_model, full_model)

#####################################


# 2.

# a)

url <- "https://raw.githubusercontent.com/jawj/coffeestats/master/lifeexp.dat"
data <- read.table(url, sep="\t", header=T, row.names=1)
data <- data[,c("LifeExp","People.per.TV","People.per.Dr")]

attach(data)
pairs(data)

hist(LifeExp, breaks = 50)
hist(People.per.Dr,breaks = 50)
hist(People.per.TV, breaks = 50)


data[order(LifeExp, decreasing = TRUE),][1:3,]
data[order(People.per.Dr, decreasing = TRUE),][1:3,]
data[order(People.per.TV, decreasing = TRUE),][1:3,]

# b)

new_data <- data[complete.cases(data),]
l2tv <- log2(new_data$People.per.TV)
l2dr <- log2(new_data$People.per.Dr)

model <- lm(LifeExp ~ l2tv + l2dr, data = new_data)

summary(model)

# c) no we can't. this is an observational study and we cannot imply any causality

# 2 countries / observations with highest Cook distance

plot(model, which = 4) # NK, Sudan

# d)

# build a 95% CI for LifeExp in a country with 50 people per tv and 3000 people per doctor

new_country <- data.frame(l2tv = log2(50), l2dr = log2(3000))

predict(model, newdata = new_country, interval = "confidence")

# build a 95% prediction interval for the same data

predict(model, newdata = new_country, interval = "prediction")

# e)

plot(model, which = 1) # TA plot => not met, not constant variance
plot(model, which = 2) # Normal plot => Not gaussian errors
# scale - location plot: indications of heteroscedasticity (= non-equal error variance)
plot(model, which = 3) # scale-location
plot(model, which = 4)
plot(model, which = 5) # residuals vs leverage

# f) exclude the obsv with the highest Cook distance

which(hatvalues(model) > 0.4) # pt ca de la 0.5 incolo trebuie exclus

rownames(new_data)[c(17, 30)]

# excluding observations from the model

new_model <- lm(new_data$LifeExp ~ l2tv + l2dr, subset = -c(17, 30))
summary(new_model)

plot(new_model, which = 5)

predict(new_model, newdata = new_country, interval = "confidence")

# 3)

# 1.

## Reading the dataset
url <- "http://stat.ethz.ch/Teaching/Datasets/mortality.csv"
mortality <- read.csv(url,header = TRUE)
mortality <- mortality[,-1]

##Create pairs plot using the splom() function of the lattice package
library(lattice)
splom(~mortality[, c("Mortality", "Pop", "HC", "NOx", "SO2")],pscales=0,cex=0.5)

par(mfrow=c(1, 2))
mortal_full <- lm(Mortality ~ ., data = mortality)

# TA plot
plot(fitted(mortal_full), resid(mortal_full))
plot(fitted(mortal_full), resid(mortal_full))
abline(h=0, lty = 2)

qqnorm(resid(mortal_full), xlab = "Standard Normal Quantiles",
       ylab = "Empirical Quantiles")
qqline(resid(mortal_full))

qqnorm(resid(mortal_full))
qqline(resid(mortal_full))


## Transform the data
mortality_old <- mortality
mortality[,"logPop"] <- log(mortality[,"Pop"])
mortality[,"logHC"] <- log(mortality[,"HC"])
mortality[,"logNOx"] <- log(mortality[,"NOx"])
mortality[,"logSO2"] <- log(mortality[,"SO2"])

# delete cols from dataset
col_num <- which(names(mortality) %in% c("Pop","HC","NOx","SO2"))
mortality <- mortality[,-col_num]

# we look again at the model assumptions with the newly transformed data
model2 <- lm(Mortality ~ ., data = mortality)

# TA plot
plot(fitted(model2), resid(model2))
abline(h = 0, lty = 2)

# Normal plot
qqnorm(resid(model2))
qqline(resid(model2))

# stepwise variable selection

# backward selection => we need to start from the full model. it stops when AIC cannot decrease anymore
full_mod <- lm(Mortality ~ ., data = mortality)
summary(full_mod)

step(full_mod, direction = "backward")

step(full_model, direction = "backward")

# forward selection => we need to start with an empty model
empty_model <- lm(Mortality ~ 1, data = mortality)

step(empty_model, direction = "forward", scope = list(upper = full_mod, lower = empty_model))

# using Mallow's Cp
library(leaps)

mortal_allsubsets <- regsubsets(Mortality ~ ., data = mortality, nvmax = 9)
source("ftp://stat.ethz.ch/Teaching/maechler/CompStat/cp-plot.R")
p.regsubsets(mortal_allsubsets)

# p = no of variables included in the smaller model

