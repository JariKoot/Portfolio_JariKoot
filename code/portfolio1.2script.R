# R code for the manuscript entitled:
# "Changes in health care workers' anxiety during two time points of the COVID-19 pandemic: Evidence from a longitudinal study"
# López Steinmetz LC, Herrera CR, Fong SB & Godoy JC.
# Corresponding author: L. Cecilia lópez Steinemtz, Ph.D. E-mail: cecilialopezsteinmetz@unc.edu.ar

####################################################################################
####################################################################################
############################### METHODS ############################################
####################################################################################
####################################################################################

####################################################################################
############################ Data analysis #########################################

# Load the dataset
#dateData<-read.table("clipboard",header=TRUE, encoding = "Latin 1", dec=",", sep="\t")
library(readxl)
dateData <- read_excel("dataset.xlsx")


# To test skewness and kurtosis.
# Criteria: range of acceptable values -1 to +1 for Skewness and -3 to +3 for Kurtosis (Brown, 2006).
# Reference: Brown TA. Confirmatory factor analysis for applied research. New York: Guilford Press; 2006.
library(moments)
# ANXIETY STATE
# Time 1:
skewness(dateData$ASTAIE)
# skewness ANXIETY STATE = 0.03374558
kurtosis(dateData$ASTAIE)
# kurtosis ANXIETY STATE = 2.701858
# Time 2 (Follow-up):
skewness(dateData$BSTAIE)
# skewness ANXIETY STATE = -0.0009689078
kurtosis(dateData$BSTAIE)
# kurtosis ANXIETY STATE = 2.358516

####################################################################################
####################################################################################
############################### RESULTS ############################################
####################################################################################
####################################################################################

####################################################################################
############################ Participants ##########################################

# First measurement 339 participants.
# Second measurement 305 participants > Attrition: 10.03%.

# To test if anxiety levels differed between participants who only answer during the first measurement and those who answered in both measurements:
# Load the sheet named as "TwoMeasuresOrOne" from the Excel file:
twoorone <-
  read.table(
    "clipboard",
    header = TRUE,
    encoding = "Latin 1",
    dec = ",",
    sep = "\t"
  )
twoorone <- read_excel("dataset.xlsx")
# To test differences:
t.test(
  x = twoorone$ASTAIE[twoorone$facetofaceassist == "no"],
  y = twoorone$ASTAIE[twoorone$facetofaceassist == "yes"],
  paired = FALSE,
  alternative = "two.sided",
  mu = 0,
  conf.level = 0.95
)
# Welch Two Sample t-test
# data:  twoorone$ASTAIE[twoorone$twomeasures == "no"] and twoorone$ASTAIE[twoorone$twomeasures == "yes"]
# t = -1.6126, df = 39.819, p-value = 0.1147
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -7.8208009  0.8798173
# sample estimates:
#   mean of x mean of y
# 28.50000  31.97049


###### Note:
# In this work, we analyzed only the sample of 305 healthcare workers that completed the online survey for the two-repeated measures.


# DESCRIPTION OF THE SAMPLE:
# TYPE OF HEALTH SETTING >>> Categories: yes = Face-to-face patients' assistance; no = Remote patients' assistance
summary(dateData$facetofaceassist)
# no yes
# 75 230
prop.table(table(dateData$facetofaceassist)) * 100
#       no      yes
# 24.59016 75.40984

# AGE
mean(dateData$edad)
# mean = 41.25902
sd(dateData$edad)
# standard deviation = 9.374612


### TABLE 1: Characteristics of the sample:
# AGE >>> Categories: OLDER = >= 40 years old; YOUNGER = < 40 years old
percentage <- dateData
percentage$edad[percentage$edad >= "40"] <- "older"
percentage$edad[percentage$edad < "40"] <- "younger"
table(percentage$edad)
# older younger
# 155     150
prop.table(table(percentage$edad)) * 100
#   older  younger
# 50.81967 49.18033

# SEX (biological sex)
table(dateData$sexo_text)
# men    women
#  63    242
prop.table(table(dateData$sexo_text)) * 100
# men      women
# 20.65574 79.34426

# AREA >>> ba = metropolitan area (province of Buenos Aires); jujuy = inside the country area (province of Jujuy)
table(dateData$provincia_text)
# ba jujuy
# 137   168
prop.table(table(dateData$provincia_text)) * 100
# ba       jujuy
# 44.91803 55.08197

# MENTAL DISORDER HISTORY >>> no = absence of mental disorder history; si = presence of mental disorder history
table(dateData$trastorno.mental_text)
# no  si
# 262  43
prop.table(table(dateData$trastorno.mental_text)) * 100
# no       si
# 85.90164 14.09836

# COVID-19 CONTAGION (during the follow-up) >>> Categories:
# no = I was not infected with the COVID-19;
# no se sin sintomas = I don't know if I was infected with the COVID-19 and I have no symptoms of the disease;
# no se pero sintomas = I don't know if I was infected with the COVID-19, but I have symptoms of the disease;
# si = Yes, I got sick with the COVID-19.
dateData$covid_text <-
  factor(dateData$covid_text ,
         levels = c("no", "no se sin sintomas", "no se pero sintomas", "si")) # to order cateogries
table(dateData$covid_text)
# no    no se sin sintomas      no se pero sintomas    si
# 138           99              20                     48
prop.table(table(dateData$covid_text)) * 100
# no         no se sin sintomas       no se pero sintomas    si
# 45.245902       32.459016            6.557377             15.737705


####################################################################################
############################ Anxiety levels ########################################

# DIFFERENCES IN PROPORTIONS:
# First measurement: High anxiety: 171 cases
# Follow-up: High anxiety: 200 cases
# N = 305
x <- c(171, 200)
n <- c(305, 305)
prop.test(x, n, conf.level = 0.95, correct = FALSE)
# 2-sample test for equality of proportions without continuity correction
# data:  x out of n
# X-squared = 5.7857, df = 1, p-value = 0.01616
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#  -0.1721900 -0.0179739
# sample estimates:
#  prop 1    prop 2
# 0.5606557 0.6557377

# CORRELATION BETWEEN THE ANXIETY LEVELS IN THE TWO MEASUREMENTS:
cor.test(
  dateData$ASTAIE,
  dateData$BSTAIE,
  exact = F,
  method = "pearson",
  conf.level = 0.95
)
# t = 9.813, df = 303, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.4008862 0.5718523
# sample estimates:
#  cor
# 0.4910838

# FIGURE 1:
library(ggplot2)
# Figure 1 (color version)
# GROUPED SCATTERPLOT: Metropolitan region and inside the country region
scatteranxbothcol <-
  ggplot(dateData, aes(ASTAIE, BSTAIE, colour = Region))
scatteranxbothcol + geom_point() + geom_smooth(method = "lm", aes(fill = Region), alpha = 0.5) + labs(x = "Anxiety - First measurement", y = "Anxiety - Follow-up") + scale_color_manual(values =
                                                                                                                                                                                           c('blueviolet', 'mediumblue')) + scale_fill_manual(values = c('blue', 'deepskyblue')) # here we have a straight line

# Figure 1 (black & white version)
# GROUPED SCATTERPLOT: Metropolitan region and inside the country region
scatteranxbothbw <-
  ggplot(dateData, aes(ASTAIE, BSTAIE, colour = Region))
scatteranxbothbw + geom_point() + geom_smooth(method = "lm", aes(fill = Region), alpha = 0.7) + labs(x = "Anxiety - First measurement", y = "Anxiety - Follow-up") + scale_color_manual(values =
                                                                                                                                                                                          c('black', 'gray52')) + scale_fill_manual(values = c('gray73', 'black')) # here we have a straight line


# Mean levels of anxiety in each measurement:
# Time 1
mean(dateData$ASTAIE)
# media = 31.97049
sd(dateData$ASTAIE)
# standard deviation = 11.28412

# Time 2
mean(dateData$BSTAIE)
# media = 34.40656
sd(dateData$BSTAIE)
# standard deviation = 12.96726

# Differences in anxiety in the entire sample during time 1 and the follow-up:
t.test(
  x = dateData$ASTAIE,
  y = dateData$BSTAIE,
  alternative = "two.sided",
  paired = TRUE,
  mu = 0,
  conf.level = 0.95
)
# t = -3.4534, df = 304, p-value = 0.0006321
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -3.824158 -1.047973
# sample estimates:
#  mean of the differences
# -2.436066


### TABLE 2: Mean levels of anxiety in each measurement, grouped by  age, sex, region, mental disorder history, and COVID-19 contagion:
## ANXIETY. TIME 1
# AGE
tapply(percentage$ASTAIE, factor(percentage$edad), mean)
#  older  younger
# 29.71613 34.30000
tapply(percentage$ASTAIE, factor(percentage$edad), sd)
#  older  younger
# 11.43517 10.67189

# SEX
tapply(dateData$ASTAIE, factor(dateData$sexo_text), mean)
# hombre    mujer
# 29.0000 32.7438
tapply(dateData$ASTAIE, factor(dateData$sexo_text), sd)
# hombre    mujer
# 11.63837 11.08438

# AREA
tapply(dateData$ASTAIE, factor(dateData$provincia_text), mean)
#    ba    jujuy
# 33.63504 30.61310
tapply(dateData$ASTAIE, factor(dateData$provincia_text), sd)
#    ba    jujuy
# 10.24256 11.92588

# TYPE OF HEALTH SETTING: Face-to-face patients' assistance:
tapply(dateData$ASTAIE, factor(dateData$facetofaceassist), mean)
#     no      yes
# 29.41333 32.80435
tapply(dateData$ASTAIE, factor(dateData$facetofaceassist), sd)
#     no      yes
# 11.22749 11.20086

# MENTAL DISORDER HISTORY
tapply(dateData$ASTAIE,
       factor(dateData$trastorno.mental_text),
       mean)
#    no       si
# 31.23664 36.44186
tapply(dateData$ASTAIE, factor(dateData$trastorno.mental_text), sd)
#    no       si
# 11.497681  8.734993

# COVID-19 CONTAGION
tapply(dateData$ASTAIE, factor(dateData$covid_text), mean)
#  no            no se sin sintomas     no se pero sintomas     si
# 31.36232            32.52525            35.60000            31.06250

tapply(dateData$ASTAIE, factor(dateData$covid_text), sd)
#  no            no se sin sintomas     no se pero sintomas     si
# 11.82657            11.10373            10.30278            10.37880

## ANXIETY. TIME 2
# AGE
tapply(percentage$BSTAIE, factor(percentage$edad), mean)
#  older  younger
# 32.65806 36.21333
tapply(percentage$BSTAIE, factor(percentage$edad), sd)
#   older  younger
# 13.79697 11.82625

# SEX
tapply(dateData$BSTAIE, factor(dateData$sexo_text), mean)
# hombre    mujer
# 31.90476 35.05785
tapply(dateData$BSTAIE, factor(dateData$sexo_text), sd)
# hombre    mujer
# 13.60824 12.74375

# AREA
tapply(dateData$BSTAIE, factor(dateData$provincia_text), mean)
#    ba    jujuy
# 35.64234 33.39881
tapply(dateData$BSTAIE, factor(dateData$provincia_text), sd)
#    ba    jujuy
# 9.738294 15.052857

# TYPE OF HEALTH SETTING: Face-to-face patients' assistance:
tapply(dateData$BSTAIE, factor(dateData$facetofaceassist), mean)
#     no      yes
# 33.01333 34.86087
tapply(dateData$BSTAIE, factor(dateData$facetofaceassist), sd)
#     no      yes
# 13.14009 12.90671

# MENTAL DISORDER HISTORY
tapply(dateData$BSTAIE,
       factor(dateData$trastorno.mental_text),
       mean)
#    no       si
# 33.46947 40.11628
tapply(dateData$BSTAIE, factor(dateData$trastorno.mental_text), sd)
#    no       si
# 12.98028 11.45428

# COVID-19 CONTAGION
tapply(dateData$BSTAIE, factor(dateData$covid_text), mean)
#  no            no se sin sintomas     no se pero sintomas     si
#  32.83333            36.63636            43.05000            30.72917

tapply(dateData$BSTAIE, factor(dateData$covid_text), sd)
#  no            no se sin sintomas     no se pero sintomas     si
#  13.31076            11.79541            13.10072            12.17905



# DIFFERENCES IN ANXIETY BY REGIONS:
# Time 1:
t.test(
  x = dateData$ASTAIE[dateData$provincia_text == "jujuy"],
  y = dateData$ASTAIE[dateData$provincia_text == "ba"],
  alternative = "two.sided",
  mu = 0,
  conf.level = 0.95
)
# t = -2.3799, df = 302.17, p-value = 0.01794
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -5.5206818 -0.5232007
# sample estimates:
#  mean of x mean of y
# 30.61310  33.63504

# Time 2:
t.test(
  x = dateData$BSTAIE[dateData$provincia_text == "jujuy"],
  y = dateData$BSTAIE[dateData$provincia_text == "ba"],
  alternative = "two.sided",
  paired = FALSE,
  mu = 0,
  conf.level = 0.95
)
# t = -1.5704, df = 288.95, p-value = 0.1174
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -5.0553527  0.5683002
# sample estimates:
#  mean of x mean of y
# 33.39881  35.64234


####################################################################################
###################### Within-person changes in anxiety ############################

# We have followed the steps indicated in Field et al. (2012) for carrying out these analyses.
# Reference: Field, A., Miles, J., Field, Z., 2012. Discovering statistics using R. SAGE, London.

#### TO ASSESS THE NEED FOR A MULTILEVEL MODEL WITH RANDOM EFFECTS BY REGION

library(nlme) # nlme = non linear mixed effects

# TIME 2
interceptOnly <-
  gls(BSTAIE ~ 1, data = dateData, method = "ML") # only the intercept
randomInterceptOnly <-
  lme(
    BSTAIE ~ 1,
    data = dateData,
    random = ~ 1 | provincia,
    method = "ML"
  ) # Region as a random effect
anova(interceptOnly, randomInterceptOnly) # to compare models
#                    Model df      AIC      BIC    logLik   Test    L.Ratio p-value
# interceptOnly           1  2 2431.632 2439.073 -1213.816
# randomInterceptOnly     2  3 2433.618 2444.779 -1213.809 1 vs 2 0.01416325  0.9053
# p-value is not significant. So, it is not important that we model the variability in intercepts because when we do the fit of our model is not significantly improved.
remove(interceptOnly, randomInterceptOnly)


# TIME 1
# AREA: ANXIETY STATE
interceptOnly <-
  gls(ASTAIE ~ 1, data = dateData, method = "ML") # only the intercept
randomInterceptOnly <-
  lme(
    ASTAIE ~ 1,
    data = dateData,
    random = ~ 1 | provincia,
    method = "ML"
  ) # Region as a random effect
anova(interceptOnly, randomInterceptOnly) # to compare models
#                    Model df      AIC      BIC    logLik   Test    L.Ratio p-value
# interceptOnly           1  2 2346.823 2354.263 -1171.411
# randomInterceptOnly     2  3 2347.380 2358.541 -1170.690 1 vs 2 1.442911  0.2297
# p-value is not significant. So, it is not important that we model the variability in intercepts because when we do the fit of our model is not significantly improved.
remove(interceptOnly, randomInterceptOnly)


# FIGURE 2:
# Scatterplot with the 2D density estimation. ANXIETY IN THE FIRST MEASUREMENT AND THE FOLLOW-UP:
# Figure 2 (color version):
ans <- ggplot(dateData, aes(x = ASTAIE, y = BSTAIE)) +
  geom_point()
ans + stat_density_2d(
  aes(fill = ..level..),
  alpha = 0.8,
  geom = "polygon",
  contour_var = "ndensity"
) +
  scale_fill_gradient(low = "blue", high = "red") + labs(x = "Anxiety - First measurement", y = "Anxiety - Follow-up") + theme(axis.text =
                                                                                                                                 element_text(size = 12),
                                                                                                                               axis.title = element_text(size = 12)) # para q esté en negrita: axis.title=element_text(size=12,face="bold")

# Figure 2 (black & white version):
ansbw <- ggplot(dateData, aes(x = ASTAIE, y = BSTAIE)) +
  geom_point()
ansbw + stat_density_2d(
  aes(fill = ..level..),
  alpha = 0.9,
  geom = "polygon",
  contour_var = "ndensity"
) +
  scale_fill_gradient(low = "gray54", high = "black") + labs(x = "Anxiety - First measurement", y = "Anxiety - Follow-up") + theme(axis.text =
                                                                                                                                     element_text(size = 12),
                                                                                                                                   axis.title = element_text(size = 12))


##### MIXED EFFECTS MODELLING

# ANXIETY (outcome): SCORES IN TIMES 1 AND 2 (within), AGE (between), SEX (between), PROVINCE, (between) COVID (between) y MENTAL DISORDER HISTORY (between)

# Preparing the data
# To select variables from the dataframe:
ans <-
  subset(
    dateData,
    select = c(
      participante,
      ASTAIE,
      BSTAIE,
      edad,
      sexo,
      provincia,
      covid,
      trastorno.mental
    )
  )

# Dummy code categories of age:
ans$edad[ans$edad >= "40"] <- "1"
ans$edad[ans$edad < "40" & ans$edad != "1"] <- "0"

ans$edad <- as.integer(ans$edad)
is.integer(ans$edad)

# To convert the format of the data frame into the long format:
library(reshape)
hcwData <-
  melt(
    ans,
    id = c(
      "participante",
      "edad",
      "sexo",
      "provincia",
      "covid",
      "trastorno.mental"
    ),
    measured = c("ASTAIE", "BSTAIE")
  )

# To rename variables:
names(hcwData) <-
  c(
    "participante",
    "edad",
    "sexo",
    "provincia",
    "covid",
    "trastorno.mental",
    "variable",
    "scores"
  ) # "variable" refers to the (repeated-measures: time 1 and time 2) variable STAIE

# This creates a variable period in the dataframe hcwData:
hcwData$period <-
  gl(2, 305, labels = c("first", "second")) # We created 2 sets of 305 scores, the labels option then specifies the names to attach to these 2 sets, which correspond to the levels of "period" (time 1 and time 2).

# To make it clearer that there are two observations for each participant we have sorted the data by participant by executing:
hcwData <- hcwData[order(hcwData$participante), ]

# Descriptive statistics:
library(pastecs)
by(hcwData$scores, list(hcwData$period), stat.desc, basic = FALSE)
mean(hcwData$scores)
library("plotrix")
std.error(hcwData$scores)

by(hcwData$scores,
   list(hcwData$period, hcwData$edad),
   stat.desc,
   basic = FALSE)
by(hcwData$scores, list(hcwData$edad), stat.desc, basic = FALSE)

by(hcwData$scores,
   list(hcwData$period, hcwData$sexo),
   stat.desc,
   basic = FALSE)
by(hcwData$scores, list(hcwData$sexo), stat.desc, basic = FALSE)

by(hcwData$scores,
   list(hcwData$period, hcwData$provincia),
   stat.desc,
   basic = FALSE)
by(hcwData$scores, list(hcwData$provincia), stat.desc, basic = FALSE)

by(hcwData$scores,
   list(hcwData$period, hcwData$covid),
   stat.desc,
   basic = FALSE)
by(hcwData$scores, list(hcwData$covid), stat.desc, basic = FALSE)

by(hcwData$scores,
   list(hcwData$period, hcwData$trastorno.mental),
   stat.desc,
   basic = FALSE)
by(hcwData$scores,
   list(hcwData$trastorno.mental),
   stat.desc,
   basic = FALSE)


####### Mixed designs as a GLM
hcwData$covid <- as.factor(hcwData$covid)
is.factor(hcwData$covid)

# Setting contrasts
covNOvscovNSnosint <-
  c(0, 1, 0, 0) # this compares the covid "no" (or baseline) with the covid "no se pero no tengo sintomas"
covNOvscovNSsisint <-
  c(0, 0, 1, 0) # this compares the covid "no"  (or baseline) with the covid "no se pero tengo sintomas"
covNOvscovSI <-
  c(0, 0, 0, 1) # this compares the covid "no"  (or baseline) with the covid "si"
contrasts(hcwData$covid) <-
  cbind(covNOvscovNSnosint, covNOvscovNSsisint, covNOvscovSI)
hcwData$covid # To check we setted the contrasts correctly

# Building the model
library(nlme)

# Baseline model (model 1)
baseline <-
  lme(
    scores ~ 1,
    random = ~ 1 | participante / period,
    data = hcwData,
    method = "ML"
  )

# To see the overall effect of each main effect and interaction we added them to the model one at a time:
periodM <- update(baseline, . ~ . + period)

edadM <- update(periodM, . ~ . + edad)

sexoM <- update(edadM, . ~ . + sexo)

provinciaM <- update(sexoM, . ~ . + provincia)

mentalM <- update(provinciaM, . ~ . + trastorno.mental)

covidM <- update(mentalM, . ~ . + covid)

# To compare additive models
anova(baseline, periodM, edadM, sexoM, provinciaM, mentalM, covidM)

# To see interactions:
period_edad <- update(covidM, . ~ . + period:edad)

period_sexo <- update(period_edad, . ~ . + period:sexo)

period_provincia <- update(period_sexo, . ~ . + period:provincia)

period_mental <-
  update(period_provincia, . ~ . + period:trastorno.mental)

period_covid <- update(period_mental, . ~ . + period:covid)

edad_sexo <- update(period_covid, . ~ . + edad:sexo)

edad_provincia <- update(edad_sexo, . ~ . + edad:provincia)

edad_mental <- update(edad_provincia, . ~ . + edad:trastorno.mental)

edad_covid <- update(edad_mental, . ~ . + edad:covid)

sexo_provincia <- update(edad_covid, . ~ . + sexo:provincia)

sexo_mental <- update(sexo_provincia, . ~ . + sexo:trastorno.mental)

sexo_covid <- update(sexo_mental, . ~ . + sexo:covid)

provincia_mental <-
  update(sexo_covid, . ~ . + provincia:trastorno.mental)

provincia_covid <- update(provincia_mental, . ~ . + provincia:covid)

mental_covid <- update(provincia_covid, . ~ . + trastorno.mental:covid)

# To comare all the models:
anova(
  baseline,
  periodM,
  edadM,
  sexoM,
  provinciaM,
  mentalM,
  covidM,
  period_edad,
  period_sexo,
  period_provincia,
  period_mental,
  period_covid,
  edad_sexo,
  edad_provincia,
  edad_mental,
  edad_covid,
  sexo_provincia,
  sexo_mental,
  sexo_covid,
  provincia_mental,
  provincia_covid,
  mental_covid
)
#                  Model df      AIC      BIC    logLik     Test   L.Ratio p-value
# baseline             1  4 4713.742 4731.396 -2352.871
# periodM              2  5 4704.005 4726.072 -2347.003   1 vs 2 11.736684  0.0006
# edadM                3  6 4694.245 4720.726 -2341.123   2 vs 3 11.759620  0.0006 #
# sexoM                4  7 4691.557 4722.451 -2338.778   3 vs 4  4.688486  0.0304 #
# provinciaM           5  8 4691.466 4726.774 -2337.733   4 vs 5  2.090859  0.1482
# mentalM              6  9 4677.700 4717.421 -2329.850   5 vs 6 15.765968  0.0001
# covidM               7 12 4671.931 4724.893 -2323.965   6 vs 7 11.769144  0.0082
# period_edad          8 13 4673.397 4730.772 -2323.699   7 vs 8  0.533640  0.4651
# period_sexo          9 14 4675.311 4737.100 -2323.656   8 vs 9  0.086046  0.7693
# period_provincia    10 15 4677.154 4743.356 -2323.577  9 vs 10  0.157070  0.6919
# period_mental       11 16 4678.716 4749.331 -2323.358 10 vs 11  0.438326  0.5079
# period_covid        12 19 4675.647 4759.503 -2318.824 11 vs 12  9.068459  0.0284 #
# edad_sexo           13 20 4676.760 4765.029 -2318.380 12 vs 13  0.887514  0.3462
# edad_provincia      14 21 4678.738 4771.420 -2318.369 13 vs 14  0.022364  0.8811
# edad_mental         15 22 4680.734 4777.831 -2318.367 14 vs 15  0.003136  0.9553
# edad_covid          16 25 4683.876 4794.212 -2316.938 15 vs 16  2.858972  0.4139
# sexo_provincia      17 26 4685.843 4800.593 -2316.921 16 vs 17  0.032888  0.8561
# sexo_mental         18 27 4684.351 4803.515 -2315.176 17 vs 18  3.491335  0.0617
# sexo_covid          19 30 4687.962 4820.366 -2313.981 18 vs 19  2.389330  0.4956
# provincia_mental    20 31 4689.373 4826.191 -2313.687 19 vs 20  0.588482  0.4430
# provincia_covid     21 34 4682.504 4832.562 -2307.252 20 vs 21 12.869464  0.0049 #
# mental_covid        22 37 4678.469 4841.767 -2302.234 21 vs 22 10.035230  0.0183 ###

# A summary of the best fitted model:
options(max.print = 999999)
summary(mental_covid)
# Here is a fragment of the summary:
# Linear mixed-effects model fit by maximum likelihood
# Data: hcwData
# AIC      BIC    logLik
# 4678.469 4841.767 -2302.234

# Random effects:
#   Formula: ~1 | participante
#           (Intercept)
# StdDev:    6.916121

# Formula: ~1 | period %in% participante
#            (Intercept) Residual
# StdDev:    7.940202  3.17491

# Fixed effects: scores ~ period + edad + sexo + provincia + trastorno.mental + covid + period:edad + period:sexo + period:provincia + period:trastorno.mental + period:covid + edad:sexo + edad:provincia + edad:trastorno.mental + edad:covid + sexo:provincia + sexo:trastorno.mental + sexo:covid + provincia:trastorno.mental + provincia:covid + trastorno.mental:covid
#                                               Value Std.Error  DF   t-value p-value
# (Intercept)                               29.549598  2.877817 297 10.268059  0.0000
# periodsecond                               1.265941  2.081359 297  0.608228  0.5435
# edad                                      -8.465631  3.027239 279 -2.796486  0.0055 #
# sexo                                       1.701659  3.025459 279  0.562446  0.5743
# provincia                                  6.181582  3.299123 279  1.873705  0.0620
# trastorno.mental                          15.992085  4.439281 279  3.602405  0.0004 # do not interpret this because the interaction is meaningful
# covidcovNOvscovNSnosint                   -1.404204  3.662827 279 -0.383366  0.7017
# covidcovNOvscovNSsisint                    5.502461  5.525994 279  0.995741  0.3202
# covidcovNOvscovSI                         -1.659051  4.772252 279 -0.347645  0.7284
# periodsecond:edad                          1.292017  1.470243 297  0.878778  0.3802
# periodsecond:sexo                         -0.389681  1.779041 297 -0.219040  0.8268
# periodsecond:provincia                    -0.758081  1.485681 297 -0.510258  0.6102
# periodsecond:trastorno.mental              0.966883  2.068445 297  0.467444  0.6405
# periodsecond:covidcovNOvscovNSnosint       2.901687  1.658597 297  1.749483  0.0812
# periodsecond:covidcovNOvscovNSsisint       5.855315  2.995395 297  1.954772  0.0515 #
# periodsecond:covidcovNOvscovSI            -1.899251  2.096596 297 -0.905873  0.3657
# edad:sexo                                  4.085016  2.871339 279  1.422687  0.1559
# edad:provincia                            -0.514617  2.375081 279 -0.216673  0.8286
# edad:trastorno.mental                     -0.493400  3.418309 279 -0.144340  0.8853
# edad:covidcovNOvscovNSnosint               1.332512  2.591630 279  0.514160  0.6075
# edad:covidcovNOvscovNSsisint               8.419141  4.658244 279  1.807364  0.0718
# edad:covidcovNOvscovSI                     2.956172  3.526895 279  0.838180  0.4026
# sexo:provincia                            -1.580906  2.889681 279 -0.547087  0.5848
# sexo:trastorno.mental                     -6.968513  3.690921 279 -1.888015  0.0601
# sexo:covidcovNOvscovNSnosint               5.343912  3.377901 279  1.582021  0.1148
# sexo:covidcovNOvscovNSsisint               0.891854  5.091415 279  0.175168  0.8611
# sexo:covidcovNOvscovSI                    -0.452126  3.930878 279 -0.115019  0.9085
# provincia:trastorno.mental                -2.116576  3.597017 279 -0.588425  0.5567
# provincia:covidcovNOvscovNSnosint         -5.933353  2.589470 279 -2.291339  0.0227 #
# provincia:covidcovNOvscovNSsisint        -10.239705  5.305359 279 -1.930068  0.0546 #
# provincia:covidcovNOvscovSI                3.898008  3.700361 279  1.053413  0.2931
# trastorno.mental:covidcovNOvscovNSnosint  -4.769315  3.597725 279 -1.325647  0.1860
# trastorno.mental:covidcovNOvscovNSsisint -19.451285  6.520736 279 -2.982989  0.0031 #
# trastorno.mental:covidcovNOvscovSI        -1.263923  5.866510 279 -0.215447  0.8296

# To see 95% confidence intervals:
intervals(mental_covid, which = "fixed")
# Approximate 95% confidence intervals
# Fixed effects:
#                                                lower      est.     upper
# (Intercept)                               24.0461987  29.5495977 35.05299668
# periodsecond                              -2.7143494   1.2659412  5.24623176
# edad                                     -14.2563054  -8.4656309 -2.67495640
# sexo                                      -4.0856110   1.7016588  7.48892857
# provincia                                 -0.1291674   6.1815819 12.49233130
# trastorno.mental                           7.5003774  15.9920850 24.48379254
# covidcovNOvscovNSnosint                   -8.4106662  -1.4042039  5.60225848
# covidcovNOvscovNSsisint                   -5.0679737   5.5024614 16.07289647
# covidcovNOvscovSI                        -10.7876856  -1.6590506  7.46958430
# periodsecond:edad                         -1.5196041   1.2920168  4.10363780
# periodsecond:sexo                         -3.7918333  -0.3896809  3.01247161
# periodsecond:provincia                    -3.5992256  -0.7580812  2.08306310
# periodsecond:trastorno.mental             -2.9887112   0.9668831  4.92247743
# periodsecond:covidcovNOvscovNSnosint      -0.2701340   2.9016867  6.07350734
# periodsecond:covidcovNOvscovNSsisint       0.1270654   5.8553148 11.58356429
# periodsecond:covidcovNOvscovSI            -5.9086796  -1.8992505  2.11017859
# edad:sexo                                 -1.4074447   4.0850161  9.57747692
# edad:provincia                            -5.0578062  -0.5146169  4.02857245
# edad:trastorno.mental                     -7.0321357  -0.4934002  6.04533519
# edad:covidcovNOvscovNSnosint              -3.6249046   1.3325119  6.28992834
# edad:covidcovNOvscovNSsisint              -0.4914125   8.4191411 17.32969474
# edad:covidcovNOvscovSI                    -3.7902740   2.9561715  9.70261702
# sexo:provincia                            -7.1084527  -1.5809065  3.94663979
# sexo:trastorno.mental                    -14.0287159  -6.9685128  0.09169039
# sexo:covidcovNOvscovNSnosint              -1.1175292   5.3439119 11.80535310
# sexo:covidcovNOvscovNSsisint              -8.8472930   0.8918536 10.63100027
# sexo:covidcovNOvscovSI                    -7.9713321  -0.4521262  7.06707963
# provincia:trastorno.mental                -8.9971531  -2.1165758  4.76400154
# provincia:covidcovNOvscovNSnosint        -10.8866380  -5.9333529 -0.98006785
# provincia:covidcovNOvscovNSsisint        -20.3880958 -10.2397047 -0.09131352
# provincia:covidcovNOvscovSI               -3.1802517   3.8980076 10.97626701
# trastorno.mental:covidcovNOvscovNSnosint -11.6512472  -4.7693147  2.11261788
# trastorno.mental:covidcovNOvscovNSsisint -31.9245195 -19.4512854 -6.97805126
# trastorno.mental:covidcovNOvscovSI       -12.4857157  -1.2639231  9.95786958
# attr(,"label")
# [1] "Fixed effects:"


# To calculate effect sizes:
library(DSUR.noof)

# We got effect sizes of meaningful predictors by executing: rcontrast(t, df)

# edad
rcontrast(-2.796486, 279)
# [1] "r =  0.165123042015866"

# trastorno.mental
rcontrast(3.602405, 279)
# [1] "r =  0.210822993888399"

# periodsecond:covidcovNOvscovNSsisint
rcontrast(1.954772, 297)
# [1] "r =  0.112704676784711"

# provincia:covidcovNOvscovNSnosint
rcontrast(-2.291339, 279)
# [1] "r =  0.135906094166818"

# provincia:covidcovNOvscovNSsisint
rcontrast(-1.930068, 279)
# [1] "r =  0.11478638510497"

# trastorno.mental:covidcovNOvscovNSsisint
rcontrast(-2.982989, 279)
# [1] "r =  0.175805379278513"

####################################################################################
####################################################################################
################################## END #############################################
####################################################################################
#####################################################################