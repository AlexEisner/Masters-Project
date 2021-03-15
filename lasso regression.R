
install.packages("glmnet")
library(glmnet)

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

#relevel the reference cat to "yes"
accident$ATMOSPHERIC_CLEAR <- relevel(accident$ATMOSPHERIC_CLEAR, ref = "Yes")
accident$ATMOSPHERIC_RAINING <- relevel(accident$ATMOSPHERIC_RAINING, ref = "Yes")
accident$ATMOSPHERIC_SNOWING <- relevel(accident$ATMOSPHERIC_SNOWING, ref = "Yes")
accident$ATMOSPHERIC_FOG <- relevel(accident$ATMOSPHERIC_FOG, ref = "Yes")
accident$ATMOSPHERIC_SMOKE <- relevel(accident$ATMOSPHERIC_SMOKE, ref = "Yes")
accident$ATMOSPHERIC_DUST <- relevel(accident$ATMOSPHERIC_DUST, ref = "Yes")
accident$ATMOSPHERIC_STRONG.WIND <- relevel(accident$ATMOSPHERIC_STRONG.WIND, ref = "Yes")

accident$SURFACE_DRY <- relevel(accident$SURFACE_DRY, ref = "Yes")
accident$SURFACE_WET <- relevel(accident$SURFACE_WET, ref = "Yes")
accident$SURFACE_MUDDY <- relevel(accident$SURFACE_MUDDY, ref = "Yes")
accident$SURFACE_SNOWY <- relevel(accident$SURFACE_SNOWY, ref = "Yes")
accident$SURFACE_ICY <- relevel(accident$SURFACE_ICY, ref = "Yes")



# make a subset that has only complete cases
accident_complete <- accident[complete.cases(accident), ]
str(accident_complete)


# the LASSO regression
x=model.matrix(FSI_OR_MINORINJURY~.,accident_complete)[,-1]
y=accident_complete$FSI_OR_MINORINJURY
grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(x,y,alpha=1,lambda=grid, family= "binomial")
plot(lasso.mod)

# create the training data 
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
set.seed(1)

# the rest of the analysis
cv.out=cv.glmnet(x[train,], y[train], alpha=1, family = "binomial")
plot(cv.out)

bestlam=cv.out$lambda.min
bestlam

lasso.pred=predict(lasso.mod, s=bestlam, newx=x[test,])

out=glmnet(x,y,alpha=1, family = "binomial")
lasso.coef=predict(out, type="coefficients", s=bestlam)[1:241,]
lasso.coef

table(accident_complete$FSI_OR_MINORINJURY)











