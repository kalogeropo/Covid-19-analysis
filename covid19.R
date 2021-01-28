install.packages("tidyr")
library(tidyr)
library(dplyr)

#1 2
confirmed <- read.csv('C:/Users/nrk_pavilion/Desktop/ddcmd/emp/r projec/confirmed.csv')
deaths <- read.csv("C:/Users/nrk_pavilion/Desktop/ddcmd/emp/r projec/deaths.csv")

#2 4 
confirmed <- confirmed %>% gather(key="date", value="confirmed", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(confirmed=sum(confirmed))
deaths <- deaths %>% gather(key="date", value="deaths", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(deaths=sum(deaths))

#3
names(confirmed)[names(confirmed) == "Country.Region"] <- "Country"
names(deaths)[names(deaths) == "Country.Region"] <- "Country"


#5
confirmed$date<-confirmed$date %>% sub("X","",.) %>% as.Date("%m.%d.%y")


deaths$date<-deaths$date %>% sub("X","",.) %>% as.Date("%m.%d.%y")


#7
country <- full_join(confirmed, deaths)
#country$date<-format(country$date,"%m.%d.%y")
country <- country %>% group_by(Country) %>% mutate( days = date - first(date) + 1)

#8
world <- country %>% group_by(date) %>% summarize(confirmed=sum(confirmed), deaths=sum(deaths)) %>% mutate(days = date - first(date) + 1)

#9
country_Temp<- country %>% group_by(Country,date) %>%arrange(Country,days)
#10
temp<-country_Temp$confirmed
test<-lag(temp)
confirmed.ind<-temp-test
confirmed.ind[is.na(confirmed.ind)]<-0

country_Temp<-cbind(country_Temp,confirmed.ind)


temp<-country_Temp$deaths
test<-lag(temp)
deaths.inc<-temp-test
deaths.inc[is.na(deaths.inc)]<-0

country_Temp<-cbind(country_Temp,deaths.inc)



names(country_Temp)[names(country_Temp) == "...6"] <- "confirmed.ind"
names(country_Temp)[names(country_Temp) == "...7"] <- "deaths.inc"

#---------------------------------------------------------------------------------------------------------#
#world_further research



temp<-world$confirmed
test<-lag(temp)
confirmed.ind<-temp-test
confirmed.ind[is.na(confirmed.ind)]<-0

world<-cbind(world,confirmed.ind)


temp<-world$deaths
test<-lag(temp)
deaths.inc<-temp-test
deaths.inc[is.na(deaths.inc)]<-0

world<-cbind(world,deaths.inc)



names(world)[names(world) == "...6"] <- "confirmed.ind"
names(world)[names(world) == "...7"] <- "deaths.inc"


install.packages("ggplot2")
library(ggplot2)
#GREEEEECE

Greece <- country_Temp %>% filter(Country=="Greece")
summary(Greece)


ggplot(Greece, aes(x=date, y=confirmed)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Cases in Greece", x= "Date", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Greece, aes(x=date, y=deaths)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Deaths in Greece", x= "Date", y= "Daily deaths") +
  theme(plot.title = element_text(hjust = 0.5))

#fixing erroneous values
Greece$confirmed.ind[1]<-0
Greece$deaths.inc[1]<-0

ggplot(Greece, aes(x=date, y=confirmed.ind)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Cases in Greece PER day", x= "Date", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Greece, aes(x=date, y=deaths.inc)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Deaths in Greece PER day", x= "Date", y= "Daily deaths") +
  theme(plot.title = element_text(hjust = 0.5))

#china

China <- country_Temp %>% filter(Country=="China")
#summary(Greece)


ggplot(China, aes(x=date, y=confirmed)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Cases in China", x= "Date", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(China, aes(x=date, y=deaths)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Deaths in China", x= "Date", y= "Daily deaths") +
  theme(plot.title = element_text(hjust = 0.5))

#fixing erroneous values
China$confirmed.ind[1]<-0
China$deaths.inc[1]<-0

ggplot(China, aes(x=date, y=confirmed.ind)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Cases in China PER day", x= "Date", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(China, aes(x=date, y=deaths.inc)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Deaths in China PER day", x= "Date", y= "Daily deaths") +
  theme(plot.title = element_text(hjust = 0.5))

#usa
#russia
# new zealand

#seasonal approach

Greece_1_3<-Greece[1:90,]

ggplot(Greece_1_3, aes(x=date, y=confirmed)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Cases in Greece at first 3 covid-19 months", x= "Date", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Greece_1_3, aes(x=date, y=deaths)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Deaths in Greece at first 3 covid-19 months", x= "Date", y= "Daily deaths") +
  theme(plot.title = element_text(hjust = 0.5))

#fixing erroneous values
Greece_1_3$confirmed.ind[1]<-0
Greece_1_3$deaths.inc[1]<-0

ggplot(Greece_1_3, aes(x=date, y=confirmed.ind)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Cases in Greece PER day at first 3 covid-19 months", x= "Date", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Greece_1_3, aes(x=date, y=deaths.inc)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Deaths in Greece PER day at first 3 covid-19 months", x= "Date", y= "Daily deaths") +
  theme(plot.title = element_text(hjust = 0.5))

Greece_7<-Greece[194:314,]

ggplot(Greece_7, aes(x=date, y=confirmed)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Cases in Greece at last 3 covid-19 months", x= "Date", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Greece_7, aes(x=date, y=deaths)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Deaths in Greece at last 3 covid-19 months", x= "Date", y= "Daily deaths") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Greece_7, aes(x=date, y=confirmed.ind)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Cases in Greece PER day at last 3 covid-19 months", x= "Date", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Greece_7, aes(x=date, y=deaths.inc)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Deaths in Greece PER day at last 3 covid-19 months", x= "Date", y= "Daily deaths") +
  theme(plot.title = element_text(hjust = 0.5))

#World
ggplot(world, aes(x=date, y=confirmed)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Global Covid-19 Confirmed Cases", x= "Date", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(world, aes(x=date, y=deaths)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Global Covid-19 Confirmed Deaths", x= "Date", y= "Daily deaths") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(world, aes(x=date, y=confirmed.ind)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Global Covid-19 Confirmed Cases per day", x= "Date", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(world, aes(x=date, y=deaths.inc)) + geom_bar(stat="identity", width=0.1) +
  theme_gray() +
  labs(title = "Global Covid-19 Confirmed Deaths", x= "Date", y= "Daily deaths") +
  theme(plot.title = element_text(hjust = 0.5))

#Greece further research
x<- list(29,31, 30, 31, 30, 31, 31, 30, 31, 30)
test1<-Greece
Greece_pie_index<-10
for (i in x){
Greece_pie_index <- Greece_pie_index  + i
print(Greece_pie_index)
test1<-rbind(test1,c(test1[Greece_pie_index,]))
}
Gr_pie<-subset(test1[315:324,],select=-c(deaths.inc,confirmed.ind))

temp<-Gr_pie$confirmed
test<-lag(temp)
confirmed.dif<-temp-test
confirmed.dif[is.na(confirmed.dif)]<-4

Gr_pie<-cbind(Gr_pie,confirmed.dif)


temp<-Gr_pie$deaths
test<-lag(temp)
deaths.dif<-temp-test
deaths.dif[is.na(deaths.dif)]<-0

Gr_pie<-cbind(Gr_pie,deaths.dif)



names(Gr_pie)[names(Gr_pie) == "...6"] <- "confirmed.dif"
names(Gr_pie)[names(Gr_pie) == "...7"] <- "deaths.dif"

install.packages("lubridate")
library(lubridate)

months<-month(Gr_pie$date)
ggplot(Gr_pie, aes(x=months, y=deaths.dif)) + geom_bar(stat="identity", width=0.1) +
  theme_gray() +
  labs(title = "Greece total deaths per month", x= "Month", y= "Daily deaths") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(Gr_pie, aes(x=months, y=confirmed.dif)) + geom_bar(stat="identity", width=0.10) +
  theme_gray() +
  labs(title = "Greece total confirmed cases per month", x= "Month", y= "Daily deaths") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Gr_pie,aes(x=months,y=confirmed.dif,fill=confirmed.dif))+geom_bar(width = 0.1, stat = "identity")  +
  labs(title = "Greece total confirmed cases per month", x= "Month", y= "Daily deaths") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(Gr_pie,aes(x=months,y=deaths.dif,fill=deaths.dif))+geom_bar(width = 0.1, stat = "identity")  +
  labs(title = "Greece total deaths per month", x= "Month", y= "Daily deaths") +
  theme(plot.title = element_text(hjust = 0.5))

#pies
pal<-c("#FF0000","#000000","#999999", "#E69F00", "#56B4E9","#80FF00","#00FF80","#00FF00","#FFFF00","#FF8000")
perc<-confirmed.dif/sum(confirmed.dif)*100
ggplot(Gr_pie,aes(x="",y=perc,fill=months))+
  geom_bar(width = 1, stat = "identity",color="white") +
  coord_polar("y",start=0)# +
  #scale_fill_brewer(palette = "Dark2") +
  #scale_fill_manual(values = pal)+theme_void()


#per 3 months charts :Greece_1_3,Greece_7,Greece_4_6

Greece_2_4_p<-Gr_pie[1:3,]
months2<- month(Greece_2_4_p$date)

Greece_5_8_p<- Gr_pie[4:7,]
months5<- month(Greece_5_8_p$date)

Greece_8_p<-Gr_pie[8:10,]
months<- month(Greece_8_p$date)


ggplot(Greece_5_8_p,aes(x="",y=confirmed.dif/sum(confirmed.dif)*100
,fill=months5))+
  geom_bar(width = 1, stat = "identity",color="white") +
  coord_polar("y",start=0)

percentage<-Greece_8_p$confirmed.dif/sum(Greece_8_p$confirmed.dif)*100
ggplot(Greece_8_p,aes(x="",y=percentage ,fill=months))+
  geom_bar(width = 1, stat = "identity",color="white") +
  coord_polar("y",start=0)+
  labs(title = "Greece total deaths per month",x="Percentage") +
  theme(plot.title = element_text(hjust = 0.5))












countryselection<-Greece
list1<-c("US", "Italy", "China", "France", "United Kingdom", "Germany")
list_balk<-c("Albania","Bosnia and Herzegovina","Bulgaria","Kosovo","Italy","Croatia","Romania","Serbia","Slovenia","Turkey","Montenegro","North Macedonia")
i<-315
balkconfirmed<-data.frame(cname=character(),min=integer(),max=integer(),mean_value=double(),variance=double())
balkconfirmed<-rbind(balkconfirmed, c("Greece",min(Greece$confirmed.ind),max(Greece$confirmed.ind),mean(Greece$confirmed.ind),var(Greece$confirmed.ind)))


balkdeaths<-data.frame(cname=character(),min=integer(),max=integer(),mean_value=double(),variance=double())
balkdeaths<-rbind(balkdeaths, c("Greece",min(Greece$deaths.inc),max(Greece$deaths.inc),mean(Greece$deaths.inc),var(Greece$deaths.inc)))
for (x in list_balk){
temp<- country_Temp %>% filter(Country==x)

balkconfirmed<-rbind(balkconfirmed, c(x,min(temp$confirmed.ind),max(temp$confirmed.ind),mean(temp$confirmed.ind),var(temp$confirmed.ind)))
balkdeaths<-rbind(balkdeaths, c(x,min(temp$deaths.inc),max(temp$deaths.inc),mean(temp$deaths.inc),var(temp$deaths.inc)))
countryselection <- rbind(countryselection,country_Temp %>% filter(Country==x))
countryselection$confirmed.ind[i]<-0
countryselection$deaths.inc[i]<-0

i<-i+314
}
ggplot(countryselection, aes(x=days, y=confirmed, colour=Country)) + geom_line(size=1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Cases by Country", x= "Days", y= "Daily confirmed cases (log scale)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(trans="log10")

balk <- countryselection %>% group_by(date) %>% summarize(confirmed=sum(confirmed), deaths=sum(deaths)) %>% mutate(days = date - first(date) + 1)

ggplot(balk, aes(x=date, y=confirmed)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Cases in Balkanian Countries", x= "Date", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(countryselection,aes(x=date, y=confirmed,group=Country,color=Country))+ geom_line()+theme_bw() +
  labs(title = "Covid-19 Confirmed Cases in Balkanian Countries", x= "Date", y= "Daily confirmed cases")

ggplot(countryselection,aes(x=date, y=confirmed.ind,group=Country,color=Country))+ geom_line()+theme_bw() +
  labs(title = "Covid-19 Confirmed Cases in Balkanian  Countries per day", x= "Date", y= "Daily confirmed cases")  

ggplot(countryselection,aes(x=date, y=deaths.inc,group=Country,color=Country))+ geom_line()+theme_bw() +
  labs(title = "Covid-19 Deaths in Balkanian  Countries per day", x= "Date", y= "Daily confirmed cases")  


ggplot(countryselection,aes(x=date, y=deaths,group=Country,color=Country))+ geom_line()+theme_bw() +
  labs(title = "Covid-19 deaths in Balkanian Countries", x= "Date", y= "Daily confirmed cases")


countryselection<-Greece
list_neighbour<-c("Albania","Bulgaria","Italy","Egypt","Turkey","North Macedonia")
i<-315
for (x in list_neighbour){
  countryselection <- rbind(countryselection,country_Temp %>% filter(Country==x))
  countryselection$confirmed.ind[i]<-0
  countryselection$deaths.inc[i]<-0
  i<-i+314
}


ggplot(countryselection,aes(x=date, y=confirmed.ind,group=Country,color=Country))+ geom_line()+theme_bw() +
  labs(title = "Covid-19 Confirmed Cases in Greece's neighbour Countries per day", x= "Date", y= "Daily confirmed cases")
  
ggplot(countryselection,aes(x=date, y=confirmed,group=Country,color=Country))+ geom_line()+theme_bw() +
  labs(title = "Covid-19 Confirmed Cases in Greece's neighbour Countries", x= "Date", y= "Daily confirmed cases")

ggplot(countryselection,aes(x=date, y=deaths.inc,group=Country,color=Country))+ geom_line()+theme_bw() +
  labs(title = "Covid-19 Deaths in Greece's neighbour Countries per day", x= "Date", y= "Daily confirmed cases")  

  
  ggplot(countryselection,aes(x=date, y=deaths,group=Country,color=Country))+ geom_line()+theme_bw() +
  labs(title = "Covid-19 deaths in Greece's neighbour Countries", x= "Date", y= "Daily confirmed cases")
  
  
  tour_countries1<-c("Belgium","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia")
  temp<-country_Temp %>% filter(Country=="Austria")
  countryselection<-temp[194:313,]
  for (x in tour_countries1){
    print(x)
    temp<-country_Temp %>% filter(Country==x)    
    countryselection <- rbind(countryselection,temp[194:313,])

  }
  
  ggplot(countryselection,aes(x=date, y=confirmed.ind,group=Country,color=Country))+ geom_line()+theme_bw() +
    labs(title = "Confirmed cases per day on actively touristical imports", x= "Date", y= "Daily confirmed cases")
  
  
  tour_countries2<-c("France","Germany","Hungary","Iceland","Ireland","Liechtenstein")
  
  temp<-country_Temp %>% filter(Country=="Finland")
  countryselection<-temp[194:313,]
  for (x in tour_countries2){
    print(x)
    temp<-country_Temp %>% filter(Country==x)    
    countryselection <- rbind(countryselection,temp[194:313,])
    
  }
  ggplot(countryselection,aes(x=date, y=confirmed.ind,group=Country,color=Country))+ geom_line()+theme_bw() +
    labs(title = "Confirmed cases per day on actively touristical imports", x= "Date", y= "Daily confirmed cases")
  
  
  tour_countries3<-c("Luxembourg","Malta","Netherlands","Norway","Poland","Portugal")
 
  
  temp<-country_Temp %>% filter(Country=="Lithuania")
  countryselection<-temp[194:313,]
  for (x in tour_countries3){
    print(x)
    temp<-country_Temp %>% filter(Country==x)    
    countryselection <- rbind(countryselection,temp[194:313,])
    
  }
  ggplot(countryselection,aes(x=date, y=confirmed.ind,group=Country,color=Country))+ geom_line()+theme_bw() +
    labs(title = "Confirmed cases per day on actively touristical imports", x= "Date", y= "Daily confirmed cases")
  
  tour_countries4<-c("Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
  
  temp<-country_Temp %>% filter(Country=="Romania")
  countryselection<-temp[194:313,]
  for (x in tour_countries4){
    print(x)
    temp<-country_Temp %>% filter(Country==x)    
    countryselection <- rbind(countryselection,temp[194:313,])
    
  }
  ggplot(countryselection,aes(x=date, y=confirmed.ind,group=Country,color=Country))+ geom_line()+theme_bw() +
    labs(title = "Confirmed cases per day on actively touristical imports", x= "Date", y= "Daily confirmed cases")
  
  
  uniq<-unique(country_Temp[,1])
  
  balkconfirmed<-data.frame(cname=character(),min=integer(),max=integer(),mean_value=double(),variance=double())
  balkconfirmed<-rbind(balkconfirmed, c("Greece",min(Greece$confirmed.ind),max(Greece$confirmed.ind),mean(Greece$confirmed.ind),var(Greece$confirmed.ind)))
  
  
  balkdeaths<-data.frame(cname=character(),min=integer(),max=integer(),mean_value=double(),variance=double())
  balkdeaths<-rbind(balkdeaths, c("Greece",min(Greece$deaths.inc),max(Greece$deaths.inc),mean(Greece$deaths.inc),var(Greece$deaths.inc)))
  uniq_l<-unlist(uniq)
  for (x in uniq_l){
    temp<- country_Temp %>% filter(Country==x)
    balkconfirmed<-rbind(balkconfirmed, c(x,min(temp$confirmed.ind),max(temp$confirmed.ind),mean(temp$confirmed.ind),var(temp$confirmed.ind)))
    balkdeaths<-rbind(balkdeaths, c(x,min(temp$deaths.inc),max(temp$deaths.inc),mean(temp$deaths.inc),var(temp$deaths.inc)))

    
    i<-i+314
  }
  install.packages("gridExtra")
  library(gridExtra)
  library(grid)
  
  colnames(balkconfirmed) <- c("Country","min","max","mean","variance")
  
  
  png("all_con.png", height = 50*nrow(balkdeaths), width = 200*ncol(balkdeaths))
  grid.newpage()
  grid.table(balkconfirmed)
  dev.off()
  
  
  colnames(balkdeaths) <- c("Country","min","max","mean","variance")
  
  png("all_death.png", height = 50*nrow(balkdeaths), width = 200*ncol(balkdeaths))
  grid.table(balkdeaths)
  dev.off()