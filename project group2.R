

```{r}

getwd()
dat<-read.csv(file.choose())

head(dat)
str(dat)
colnames(dat)<-c("obs","x1","x2","x3","x4","x5","x6","y","x7","x8","x9")
head(dat)
str(dat)
dat$x1<-as.numeric(dat$x1)
dat$x2<-as.factor(dat$x2)
dat$x3<-as.factor(dat$x3)
dat$x4<-as.factor(dat$x4)
dat$x5<-as.factor(dat$x5)
dat$x6<-as.factor(dat$x6)
dat$y<-as.numeric(dat$y)
dat$x7<-as.factor(dat$x7)
dat$x8<-as.factor(dat$x8)
dat$x9<-as.factor(dat$x9)
str(dat)

# After reading the data into R, we viewed the structure of each of the variables provided
#and treated it by converting each of them based on the variable type.
#y which denotes our response indicates Ordered to complete minutes was converted to numerical variable
# x1 which denotes the patients age was converted to numerical variable,
# x2 which denotes the radiology technician was converted to categorical variable
# x3 which denotes the catalog code was converted to categorical variable
# x4 which denotes the in rad.room with the base variable of " 1" (if performed in radiology room) when converted to a categorical variable
# x5 which denotes the patient type with a base variable of "IP-in patient" with respect to the other variables  when converted to a categorical variable
# x6 which denotes the priority with a base variable of Routine with respect to STAT when converted to a categorical variable
# x7 which denotes the Lock.at.Exam.Complete converted was converted a categorical variable
# x8 which denotes the Exam completed bucket with a base variable of time "12am-8am" with respect to other variable when converted to categorical
# x9 which denotes the Exam room was converted to a categorical variable

#Fitting the linear model with all the predictor variables regressed on the response(y)
model<-lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dat)
plot(model)
vif(model)
#the Vif of the model was  implemented but could not process an output because of the categorical variables present

#After fitting the linear model, we went on to check the model adequacy by observing the plots
#Checking the Residual vs fitted plot, we observed that a pattern exist in the distribution of the data across the graph
#Indicating the constant variance assumption is questionable
#Checking for the Normal Q-Q plot, we observed we do not have a fairly straight line
#Indicating the Normality is questionable
#The square root of  standardized residual vs fitted values also indicates a pattern, nullyfying out assumption for normality
# The residuals vs leverage plot also indicates we have a pattern, nullyfing our assumption for normality
summary(model)
#The overall p value for the model is 2.2e-16 indicating it is highly significant
#the multiple and Adjusted R values are 0.2561 and 0.252 respectively, which implies the model is not accurate enough

#From the results above , we conclude to use a power transformation (Box-cox) because
#The plots obtained were questionable  

#Applying the box-cox transformation
library(MASS)
b<-boxcox(model)
lambda<-b$x
liklihood<-b$y
which.max(liklihood)
#the liklihood vaue is 45
lambda[45]
#tHE Lambda value is  -0.2222222
dat$newy<-log(dat$y)
#Using a log transform because the value of lambda is close to Zero, from the plot
head(dat)
#Fitting a new model with the new values of y obtained after performing box cox transformation.
model2<-lm(newy~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dat)
plot(model2)
#Observing the model adequacy plots
#Checking the Residual vs fitted plot, we observed that the pattern no longer exist in the distribution of the data across the graph
#Indicating we have a constant variance(scatter plot) across the data distribution
#Checking for the Normal Q-Q plot, we observed we have a fairly straight line
#Indicating , Normality exist
#The square root of  standardized residual vs fitted values also indicates constant variance assumption is satisfied
#The residuals vs leverage plot also indicates indicates constant variance assumption is satisfied
summary(model2)
#The overall p-value for the model is 2.2e-16 indicating it is highly significant.
#the multiple and Adjusted R values are 0.5809 and 0.5787 respectively,
#implying a better r2 and adj. R2 values than the first model

#Checking for Outliers,leverage,and and influencial points
#Checking the Residual vs fitted plot, we observed that we have data points (69,238,85) which where outliers
#From the Normal Q-Q plot we had data points (69,238,85) which were outliers
#The square root of  standardized residual vs fitted values outliers with data points (69,238,85)

#In order to obtain the best fitting model, the step wise procedures(forward, backward and both)were executed

#Implementing the forward step wise regression model

model3<-lm(newy~1,data=dat)
formula(model3)
step(model3,scope~x1+x2+x3+x4+x5+x6+x7+x8+x9,direction = "forward")

#the forward step wise regression gave us the model newy~x6+x9+x8+x3+x2+x7+x5+x1
#arranging the variables in order of ascending AIC values and dropping the variable x4 completely

#Dropping predictor variables based on the AIC (from highest to lowest)

summary(lm(newy~x6+x9+x8+x3+x2+x7+x5+x1,data = dat))
# dropping x1 based on AIC obtained from forward step wise and level of significance
summary(lm(newy~x6+x9+x8+x3+x2+x7+x5,data = dat))
# dropping x5 based on AIC obtained from forward step wise and level of significance
summary(lm(newy~x6+x9+x8+x3+x2+x7,data = dat))
# dropping x7 based on AIC obtained from forward step wise and level of significance
summary(lm(newy~x6+x9+x8+x3+x2,data = dat))
# dropping x2 based on AIC obtained from forward step wise and level of significance
summary(lm(newy~x6+x9+x8+x3,data = dat))
# dropping x3 based on AIC obtained from forward step wise and level of significance
summary(lm(newy~x6+x9+x8,data = dat))
# dropping x8 based on AIC obtained from forward step wise and level of significance
summary(lm(newy~x6+x9,data = dat))
#From the forward stepwise regression, we propose to use two model
fullmodel1<-lm(newy~x6+x9+x8,data=dat)
summary(fullmodel1)
#Our first proposed model(newy~x6+x9+x8), has an overall p value of < 2.2e-16
#R2 and adj. R2 of 0.537 and 0.5368 respectively.

fullmodel2<-lm(newy~x6+x9,data=dat)
summary(fullmodel2)
#Our second proposed model(newy~x6+x9), has an overall p value of < 2.2e-16
#R2 and adj. R2 of 0.5011 and 0.501 respectively.


#Implementing the backward step wise regression model


model<-lm(newy~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dat)
formula(model)
step(model,direction = "backward")

#the backward step wise regression gave us the model newy ~ x1+x5+x7+x2+x8+x3+x9+x6
#arranging the variables in order of ascending AIC values and dropping the variable x4 completely

#Dropping predictor variables based on the AIC

summary(lm(newy ~ x1+x5+x7+x2+x8+x3+x9+x6, data = dat))
# dropping x6 based on AIC obtained from forward step wise and level of significance
summary(lm(newy ~ x1+x5+x7+x2+x8+x3+x9, data = dat))
# dropping x9 based on AIC obtained from forward step wise and level of significance
summary(lm(newy ~ x1+x5+x7+x2+x8+x3, data = dat))
# dropping x3 based on AIC obtained from forward step wise and level of significance
summary(lm(newy ~ x1+x5+x7+x2+x8, data = dat))
# dropping x8 based on AIC obtained from forward step wise and level of significance
summary(lm(newy ~ x1+x5+x7+x2, data = dat))
# dropping x2 based on AIC obtained from forward step wise and level of significance
summary(lm( newy ~ x1+x5+x7,data = dat))
# dropping x7 based on AIC obtained from forward step wise and level of significance
summary(lm(newy ~ x1+x5,data = dat))

#From the backward stepwise regression, we propose to use the  model below
fullmodel3<-lm(newy~x1+x5,data=dat)
summary(fullmodel3)

#the proposed model from backward step wise (newy~x1+x5), has an overall p value of < 2.2e-16
#R2 and adj. R2 of 0.1396 and 0.1395 respectively.


#Implementing the both step wise regression model

model<-lm(newy~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dat)
formula(model)
step(model,scope~x1+x2+x3+x4+x5+x6+x7+x8+x9,direction = "both")
#the both step wise regression gave us the model newy ~ x1+x5+x7+x2+x8+x3+x9+x6
#arranging the variables in order of ascending AIC values and dropping the variable x4 completely

#Dropping predictor variables based on the AIC

summary(lm(newy ~ x1+x5+x7+x2+x8+x3+x9+x6, data = dat))
# dropping x6 based on AIC obtained from forward step wise and level of significance
summary(lm(newy ~ x1+x5+x7+x2+x8+x3+x9, data = dat))
# dropping x9 based on AIC obtained from forward step wise and level of significance
summary(lm(newy ~ x1+x5+x7+x2+x8+x3, data = dat))
# dropping x3 based on AIC obtained from forward step wise and level of significance
summary(lm(newy ~ x1+x5+x7+x2+x8, data = dat))
# dropping x8 based on AIC obtained from forward step wise and level of significance
summary(lm(newy ~ x1+x5+x7+x2, data = dat))
# dropping x2 based on AIC obtained from forward step wise and level of significance
summary(lm( newy ~ x1+x5+x7,data = dat))
# dropping x7 based on AIC obtained from forward step wise and level of significance
summary(lm(newy ~ x1+x5,data = dat))

# we observe the final model from the both step wise is the same as the model fom the backward step wise

#Implementing the olsrr function
#in order to obtain a more accurate model, the olsrr function was implemented because it considers major criteria such as the AIC and adjusted R-square
#and other parameters simultaneously

#Note,
#The olsrr function was implemented on the initial model 1 which contains all predictor variables given
#which were (x1+x2+x3+x4+x5+x6+x7+x8+x9)
#The olsrr could not process an output for the large number of data set

#Also
#The dredge function was implemented on the initial model 1 which contains all predictor variables given
#which were (x1+x2+x3+x4+x5+x6+x7+x8+x9)
#The dredge could not process an output for the large number of data set


#Running olsrr with this predictor variables gotten from the step-wise models
library(olsrr)
fullmodel5<-lm(newy~x6+x9+x8,data=dat,na.action = "na.fail")
summary.fit1<-ols_step_best_subset(fullmodel3)

#from the olsrr output we observed that the predictor variable x6 has significant AIC and a moderate R-square and adjusted R-square values
#so we regress the response newy on x6
fullmodel4<-lm(newy~x6,data=dat)
summary(fullmodel4)
#from the olsrr method, x6 seems to be the most significant with an overall p value of 2.2e-16
#with R2 and adj. R2 of 0.4239 and 0.4239  respectively.

#Reaching a conclusion , for our proposed model we have
#fullmodel1<-lm(newy~x6,data=dat) (proposed model 1)
#fullmodel2<-lm(newy~x6+x9,data=dat) (proposed model 2)
#fullmodel3<-lm(newy~x6+x9+x8,data=dat) (proposed model 3)
#fullmodel4<-lm(newy~x1+x5,data=dat) (proposed model 4)

#RECOMMENDATION and CONCLUSION

#After analyzing the data, it was observed that one particular predictor variable (x6 which denotes priority)
#recurred and has major influence on the hospitals delivery time of radiology results

#So if the hospital was to consider just one predictor variable, they should consider x6 which denotes priority
#This is because the data shows that the hospital is giving far more priority to STAT patient than routine
#from the overall result of analysis done, the variable x6 most importantly contributes to the uptick in patients' length of stay
#and for hospital to solve this problem,the hospital needs to find a balance between prioritizing the routine and STATS patient

#If the hospital was to consider two predictor variable, they should go with the second model
#This model shows that the priority and exam rooms are the significant variables contributing to the problem
#A process can be generated to handle each case based on priority
#Example, for all STAT cases, portable X-ray machines can be taken to the patient's room by radiology technician
#While all routine cases can be handled at the radiology room

#If the hospital was to consider three predictor variable, they should go with the third model
#This model shows that the priority, exam rooms and exam completed bucket are the significant variables
#Since the time period taken to complete the exam will determine how many of the patients will be attended to over a certain period
#So the hospital should consider dividing the longer shifts into two and hire technicians for these shifts

#There is also one model that can be considered which is the fourth model
#This model shows that the patient age and patient type can be causing the patients to stay long
#Because older patients tends to stay longer in the hospital increasing the uptick length of stay at the hospital
#Also, there are many patients in (IP,OPEC,OPOBS,OPSRG), which also translates to an increase in the uptick length of stay at the hospital
#the hospital should look in to discharging patients after their recovery process to
#Help reduce their uptick length of stay at their hospital

```