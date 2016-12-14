rm(list = ls())
install.packages("plotly")
#graphs
library(haven)
library(ggplot2)
library(MASS)
library(plotly)

#load data set from Stata
dset.crime<-read_dta("stata files/zcbp90.dta")

#data where race is turned into a factor variable
dset.race<- within(dset.crime, {
  race<- factor(race, levels = 0:4, labels = c("white", "black", "hispanic", "minority", "integrated"))
})

#data whre race remains a continuous variable
dset.norace<- dset.crime
#create new data set for predicted value graph (interactions)
newdset.race<- data.frame(religious_organizations= rep(0:51, each=5),
                          race=rep(c(0:4),52),
                          disadvantage = mean(dset.race$disadvantage),
                          residential_instability =mean(dset.race$residential_instability),
                          race_het =mean(dset.race$race_het))
#create new data set for predicted value graph (average effect across races)
newdset.norace<- data.frame(religious_organizations= rep(0:51),
                            T_PCBLK=mean(dset.norace$T_PCBLK),
                            T_PCHISP=mean(dset.norace$T_PCHISP),
                            T_PCASAN=mean(dset.norace$T_PCASAN),
                            T_PCOTH=mean(dset.norace$T_PCOTH),
                            disadvantage = mean(dset.crime$disadvantage),
                            residential_instability =mean(dset.crime$residential_instability),
                            race_het =mean(dset.norace$race_het))
#turn race into a factor variable
newdset.race <- within(newdset.race, {
  race<- factor(race, levels = 0:4, labels = c("white", "black", "hispanic", "minority", "integrated"))
})
#negative binomial model for race
race.model<-glm.nb(robberies~ disadvantage+ residential_instability+ 
                     race_het + religious_organizations:race, 
                   data=dset.race)
#model for no race
norace.model<-glm.nb(robberies~ disadvantage+ residential_instability+ 
                       race_het + religious_organizations+ T_PCBLK +
                       T_PCHISP+ T_PCASAN+ T_PCOTH, 
                     data=dset.norace)
#create column for predicted values
newdset.race$predicted_robberies<- predict(race.model, newdata = newdset.race, type = "response")

#create column for predicted values (no race)
newdset.norace$predicted_robberies<- predict(norace.model, newdata = newdset.norace, type = "response")

d<-ggplot() + 
  geom_line(data = newdset.race, aes(x=religious_organizations, 
                                     y = predicted_robberies, color=race)) +
  geom_line(data = newdset.norace, aes(x=religious_organizations,
                                       y = predicted_robberies), size=1.5)+
  xlab('Number of Religious Organization') + ylab('Number of Robberies')


d


d<- ggplot()+
  geom_line(data = newdset.norace, aes(x=religious_organizations,
                                         y = predicted_robberies,  
                                       scale_size_manual(values = 2)))

d
ggplotly(d)

#create plot
d <- ggplot(newdset.race, aes(religious_organizations, predicted_robberies, color=race))+
  geom_line() +xlab('Number of Religious Organization') + ylab('Number of Robberies') +
  

#create plot
d <- ggplot(newdset.race, aes(religious_organizations, predicted_robberies, color=race))+
  geom_line() +xlab('Number of Religious Organization') + ylab('Number of Robberies') +

d
ggplotly(d)
dset.norace$T_pcbl