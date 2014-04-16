readData_inet<-function(path.name,file.name,column.types,missing.types){
read.csv(url(paste(path.name,file.name,sep="")),
colClasses=column.types,
na.strings=missing.types)
}

#myversion
readData_fal<-function(file.name,column.types,missing.types){
read.csv(file.name,colClasses=column.types,na.strings=missing.types)
}

readData<-function(file.name,missing.types){
read.csv(file.name,na.strings=missing.types)
}

#function to extract honorific (title) from the name feature
getTitle_inet<-function(data){
	title.dot.start<-regexpr("\\,[A-Z]{1,20}.",data$Name,TRUE)
	title.comma.end<-title.dot.start
				+attr(title.dot.start,"match.length")-1
	data$Title<-substr(data$Name,title.dot.start+2,title.comma.end-1)
	return(data$Title)
}

getTitle<-function(data){
	title.dot.start<-regexpr("\\,",data$Name,TRUE)
	title.comma.end<-regexpr("\\.",data$Name,TRUE)
	data$Title<-substr(data$Name,title.dot.start+2,title.comma.end-1)
	return(data$Title)
}

imputeMedian<-function(impute.var,filter.var,var.levels){
for(v in var.levels){
	impute.var[which(filter.var==v)]<-impute(impute.var[which(filter.var==v)])
}
return (impute.var)
}

changeTitles <- function(data, old.titles, new.title) {
  for (honorific in old.titles) {
    data$Title[ which( data$Title == honorific)] <- new.title
  }
  return (data$Title)
}
