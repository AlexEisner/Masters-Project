#read in the file
accident <- read.csv(file.choose(),header=T)  

#make sure the variables are all the right format
str(accident)
accident$ACCIDENT_TYPE <-as.factor(accident$ACCIDENT_TYPE)
accident$DAY_OF_WEEK <-as.factor(accident$DAY_OF_WEEK)
accident$LIGHT_CONDITION <-as.factor(accident$LIGHT_CONDITION)
accident$ROAD_GEOMETRY <-as.factor(accident$ROAD_GEOMETRY)
accident$SPEED_ZONE <-as.factor(accident$SPEED_ZONE)
accident$ROAD_SURFACE_TYPE.A <-as.factor(accident$ROAD_SURFACE_TYPE.A)
accident$ROAD_SURFACE_TYPE.B <-as.factor(accident$ROAD_SURFACE_TYPE.B)
accident$VEHICLE_TYPE.A_RECODED <-as.factor(accident$VEHICLE_TYPE.A_RECODED)
accident$VEHICLE_TYPE.B_RECODED <-as.factor(accident$VEHICLE_TYPE.B_RECODED)
accident$TRAFFIC_CONTROL.A <-as.factor(accident$TRAFFIC_CONTROL.A)
accident$TRAFFIC_CONTROL.B <-as.factor(accident$TRAFFIC_CONTROL.B)
accident$DRIVER_INTENT.A <-as.factor(accident$DRIVER_INTENT.A)
accident$VEHICLE_MOVEMENT.A <-as.factor(accident$VEHICLE_MOVEMENT.A)
accident$VEHICLE_MOVEMENT.B <-as.factor(accident$VEHICLE_MOVEMENT.B)
accident$ROAD_USER_TYPE.A. <-as.factor(accident$ROAD_USER_TYPE.A.)
accident$ROAD_USER_TYPE.B. <-as.factor(accident$ROAD_USER_TYPE.B.)
accident$DRIVER_INTENT.B <-as.factor(accident$DRIVER_INTENT.B)

# make a subset that has only complete cases
accident_complete <- accident[complete.cases(accident), ]
str(accident_complete)

#install.packages("nnet")
library(nnet)

## stepwise regression
# make Minor Injury Crashes the reference category
accident_complete$FSI_OR_MINORINJURY <- relevel(accident_complete$FSI_OR_MINORINJURY, ref = "Minor Injury Crash")

# make the reference categories different for each level
accident_complete$SPEED_ZONE <- relevel(accident_complete$SPEED_ZONE, ref = "60")

str(accident_complete)


#### stepwise selection, only including the predictor variables included by the lasso
full.model <- glm(accident_complete$FSI_OR_MINORINJURY ~ accident_complete$ACCIDENT_TYPE + 
                    accident_complete$DAY_OF_WEEK +
                    accident_complete$LIGHT_CONDITION +
                    accident_complete$ROAD_GEOMETRY +
                    accident_complete$SPEED_ZONE + 
                    accident_complete$Deg.Urban.Name +
                    accident_complete$Postcode.No +
                    accident_complete$SURFACE_DRY + 
                    accident_complete$SURFACE_WET +
                    accident_complete$ATMOSPHERIC_CLEAR +
                    accident_complete$ATMOSPHERIC_RAINING +
                    accident_complete$ATMOSPHERIC_RAINING +
                    accident_complete$ATMOSPHERIC_FOG +
                    accident_complete$ATMOSPHERIC_STRONG.WIND +
                    accident_complete$TRAFFIC_CONTROL.A +
                    accident_complete$DRIVER_INTENT.A +
                    accident_complete$VEHICLE_MOVEMENT.A +
                    accident_complete$VEHICLE_COLOUR_1.A +
                    accident_complete$VEHICLE_YEAR_MANUF.B +
                    accident_complete$ROAD_SURFACE_TYPE.B +
                    accident_complete$TRAFFIC_CONTROL.B +
                    accident_complete$DRIVER_INTENT.B +
                    accident_complete$VEHICLE_MOVEMENT.B +
                    accident_complete$VEHICLE_COLOUR_1.B +
                    accident_complete$SEX.A. +
                    accident_complete$AGE.A. +
                    accident_complete$ROAD_USER_TYPE.A. +
                    accident_complete$SEX.B. +
                    accident_complete$AGE.B. +
                    accident_complete$ROAD_USER_TYPE.B. +
                    accident_complete$DCA_CAT +
                    accident_complete$VEHICLE_TYPE.A_RECODED +
                    accident_complete$VEHICLE_TYPE.B_RECODED, data = accident_complete, family = binomial)
coef(full.model)
summary(full.model)

# stepwise variable selection
library(MASS)
library(magrittr)
step.model <- full.model %>% stepAIC(trace = FALSE, direction = c("both"))
coef(step.model)

summary(step.model)

#calculate the odds ratios with confidence intervals
Oddsratios = exp(cbind(coef(step.model), confint(step.model, level = 0.95)))
Oddsratios


### area under the ROC curve
prob=predict(step.model,type=c("response"))
accident_complete$prob=prob
library(pROC)
g <- roc(accident_complete$FSI_OR_MINORINJURY ~ prob, data = accident_complete)
plot(g)    
auc(g)








