library(readxl)
library(caTools)
library(ggplot2)
data=read_xlsx("Salary_Data.xlsx")
data
# checking for NA's
is.na(data)

# split the data
split=sample.split(Y=data$Salary,SplitRatio = 2/3)
split
train_set=subset(x=data,split==TRUE)
test_set=subset(x=data,split==FALSE)

# fitting to model
model=lm(formula=Salary~YearsExperience,data=train_set)

#check model stats
summary(model)
Y_pred=predict(object=model,newdata = test_set)

#do visualization
ggplot()+geom_point(aes(x=train_set$YearsExperience,y=train_set$Salary),color="green")+
  geom_line(aes(x=train_set$YearsExperience,y=predict(object=model,newdata = train_set)),color='red')
