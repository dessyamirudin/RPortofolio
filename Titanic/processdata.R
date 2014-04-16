train.data.file<-"train.csv"
test.data.file<-"test.csv"
missing.types<-c("NA","")
#train.column.types<-c('integer', #passenger ID
#			    'factor', #survived
#			    'factor', #Pclass
#			    'character',#Name
#			    'factor', #Sex
#			    'numeric', #Age
#			    'integer', #SibSp
#			    'integer', #Parch
#			    'character', #Ticket
#			    'numeric', #Fare
#			    'character', #cabin
#			    'factor', #embarked
#)

#trainRaw<-readData(train.data.file,train.column.types,missing.types)
trainRaw<-readData(train.data.file,missing.types)
df.train<-trainRaw
#testRaw<-readData(test.data.file,train.column.types,missing.types)
testRaw<-readData(test.data.file,missing.types)
df.test<-testRaw

windows()
require(Amelia)
missmap(df.train, main="Titanic Training Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)

windows()
barplot(table(df.train$Survived),
	names.arg=c("Perished","Survived"),
	main="Survived - passenger fate",col="black")

windows()
barplot(table(df.train$Pclass),
	names.arg=c("first","second","third"),
	main="Pclass (passenger travelling class)",col="firebrick")

windows()
barplot(table(df.train$Sex),main="Sex (gender)",col="darkviolet")

windows()
hist(df.train$Age,main="Age",xlab=NULL,col="brown")

windows()
barplot(table(df.train$SibSp),main="SibSp (Sibling + spouse aboard)",
	col="darkblue")

windows()
barplot(table(df.train$Parch),main="Parent and kid aboard)",col="gray50")

windows()
hist(df.train$Fare,main="Fare",xlab=NULL, col="darkgreen")

windows()
barplot(table(df.train$Embarked),
	names.arg=c("Cherbourg","Queenstown","Southhampton"),
	main="Embarked (port embarkation)",col="sienna")

windows()
mosaicplot(df.train$Pclass ~ df.train$Survived,
	main="Passenger Fate by Travelling Class",shade=FALSE,
	color=TRUE,xlab="pclass",ylab="Survived")

windows()
mosaicplot(df.train$Sex ~ df.train$Survived,
	main="Passenger Fate by Sex",shade=FALSE,
	color=TRUE,xlab="Sex",ylab="Survived")

windows()
boxplot(df.train$Age~df.train$Survived,
	main="passenger fate by age",
	xlab="Survived",ylab="Age")

windows()
mosaicplot(df.train$Embarked ~ df.train$Survived,
	main="Passenger fate by embarkation", shade=FALSE,
	color=TRUE,xlab="Embarked", ylab="Survived")

windows()
require(seriation)
require(corrgram)
require(plyr)
corrgram.data<-df.train
#assign new type of data
corrgram.data$Survived<-as.numeric(corrgram.data$Survived)
corrgram.data$Pclass<-as.numeric(corrgram.data$Pclass)
corrgram.data$Embarked<-revalue(corrgram.data$Embarked,
					c("C"=1,"Q"=2,"S"=3))

#generate correlogram
corrgram.vars<-c("Survived","Pclass","Sex","Age","SibSp","Parch","Fare","Embarked")
corrgram(corrgram.data[,corrgram.vars], order=FALSE,
	lower.panel=panel.ellipse,upper.panel=panel.pie,
	text.panel=panel.txt,main="Titanic Training Data")

windows()
boxplot(df.train$Age ~ df.train$Pclass)

###creating a summary of age based on class
#tableAge<-df.train[complete.cases(df.train$Age),]
#tapply(tableAge$Age,list(tableAge$Pclass),mean)
	#OR
#aggregate(tableAge$Age,list(tableAge$Pclass),FUN=mean)




