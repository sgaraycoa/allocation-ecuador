###make commands out of each formula (dhondt and webster)
###so that they can be called by the shiny app

load("./nat_seats.rds")
load("./prov_seats.rds")
load("./prov_seats_agg.rds")

library(tidyr)
library(dplyr)

##calculate d'hondt
#rank parties by V/1+s
#where V is the number of votes and s is the number of seats already obtained
#this is calculated iteratively

partyvotes<- newdata_prov %>%
    group_by(op_codigo, add=T) %>% #group by political party
    summarise(v1=sum(votos_candidato), distrito=first(distrito),
              op_nombre=first(op_nombre))
partyvotes$s<-0
partyvotes$s<-ifelse(partyvotes$v1==max(partyvotes$v1), partyvotes$s + 1, partyvotes$s)
partyvotes$v<-partyvotes$v1

seats <- 15 #this "national" district has 15 seats
for(i in 2:seats){
    partyvotes$v<-partyvotes$v1 / (partyvotes$s + 1)
    partyvotes$s<-ifelse(partyvotes$v==max(partyvotes$v), partyvotes$s + 1, partyvotes$s)
}


###############################
#NOTE: Ecuador does not use Dhondt for the national district,
###ONLY for the provincial districts.
##Instead it used the Webster Method for the national district
###################


################
#Webster Method
######################
#Divide total number of votes for a party by total number of seats
###Round up (if >0.5) for each party and that is the number of seats per party.
quotient<-sum(partyvotes$v1)/seats
partyvotes$w<- round(partyvotes$v1 / quotient)

