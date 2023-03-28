install.packages("stringr")  
library("stringr")

library(readxl)
DATA_FILM <- read.csv(file="E:/2EMS/SAE 4-01/DATAPROPRES.csv",sep =";",dec=",",na.strings = "" )
ncol<-length(DATA_FILM)
nlign<-length(DATA_FILM$ï..Title)

for(j in 1:ncol){
  for(i in 1:nlign){
    if(is.na(DATA_FILM[i,j])==TRUE){
      DATA_FILM[i,j]<-mean(DATA_FILM[,j])
    }
  }
}

modele<-lm(budgres,DATA_FILM$Income)


budg<-DATA_FILM$Budget
budgres<-as.integer(budg)
budgres<-na.omit(budgres)
budgres
mean(budgres)
length(DATA_FILM$Genre)
boxplot(budgres ~ DATA_FILM$Genre)

incom<-DATA_FILM$Income
incom<-str_replace_all(incom, "[^[:alnum:]]", "")
incomres<-as.integer(incom)

plot(incomres)



plot(DATA_FILM$Year,mean(DATA_FILM$Rating))

ratio<-c()

for(i in 1:length(DATA_FILM)){
  ratio[i]<-incomres[i]/budgres[i]
}

plot(DATA_FILM$Title,ratio)

length(ratio)
length(DATA_FILM$Title)


genre<-DATA_FILM$Genre

Eff<-table(genre)
Nval<-length(genre)
Freq<-Eff/Nval

table(genre)

lieu<-DATA_FILM$Filming_location
table(lieu)
barplot(Freq)


#Etude du genre

Genre2<-str_split(c(genre),",")
Genre2

