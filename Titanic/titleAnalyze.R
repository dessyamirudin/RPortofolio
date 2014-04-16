#analyzing title
options(digits=2)
require(Hmisc)
bystats(df.train$Age,df.train$Title,
fun=function(x)c(Mean=mean(x),Median=median(x)))

titles.na.train<-c("Dr","Master","Mrs","Miss","Mr")

df.train$Age<-imputeMedian(df.train$Age,df.train$Title,titles.na.train)

