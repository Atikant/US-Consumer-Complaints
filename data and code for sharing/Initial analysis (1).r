# Project sem 2 basic analysis

complaints<-read.csv("C:/Users/Atikant Negi/Downloads/Project_Sem2/complaints.csv")
head(complaints)
dim(complaints)
attributes(complaints)
library(tidyr)
library(dplyr)

#Types why the product was closed
Company.response<-complaints%>%
  group_by(Company.response.to.consumer)%>%
  summarise("count"=n())
#### 1 ""                                      2
#### 2 "Closed"                            17611
#### 3 "Closed with explanation"         1623241
#### 4 "Closed with monetary relief"       97065
#### 5 "Closed with non-monetary relief"  221742
#### 6 "Closed with relief"                 5304
#### 7 "Closed without relief"             17868
#### 8 "In progress"                       16772
#### 9 "Untimely response"                  8169


# with monetary relief
monetary_relief<-complaints%>%
  filter(complaints$Company.response.to.consumer=="Closed with monetary relief")%>%
  select(Product,Sub.product ,Issue ,Company,State,Timely.response.)
write.csv(monetary_relief, file = "monetaryRelief.csv")

#without manetary relief
monetary_relief_others<-complaints%>%
  filter(complaints$Company.response.to.consumer!="Closed with monetary relief")%>%
  select(Product,Sub.product ,Issue ,Company,State,Timely.response.)
write.csv(monetary_relief_others, file = "monetaryReliefOthers.csv")


#monetary relief group by company
mr_by_comp<-monetary_relief%>%
  group_by(Company)%>%
  summarise("count"=n())%>%
  arrange(desc(count))
####BANK OF AMERICA, NATIONAL ASSOCIATION -- 14011

#monetary relief others group by company
mro_by_comp<-monetary_relief_others%>%
  group_by(Company)%>%
  summarise("count"=n())%>%
  arrange(desc(count))
####1 EQUIFAX, INC.                     -- 241797


#monetary relief others group by Product
mro_by_prod<-monetary_relief_others%>%
  group_by(Product)%>%
  summarise("count"=n())%>%
  arrange(desc(count))
####  1 Credit reporting, credit repair services, or other personal consumer reports 664976
#### Debt collection                                                              337426


#monetary relief others group by Product
mr_by_comp<-monetary_relief%>%
  group_by(Product)%>%
  summarise("count"=n())%>%
  arrange(desc(count))
#### 1 Credit card                                                                  18433
#### 2 Bank account or service                                                      18200


#monetary relief others group by State
mro_by_State<-monetary_relief_others%>%
  group_by(State)%>%
  summarise("count"=n())%>%
  arrange(desc(count))
####   1 CA    249018
####   2 FL    210342
####   3 TX    176747                                                        
####   4 NY    127387
####   5 GA    107934

#monetary relief others group by State
mr_by_State<-monetary_relief%>%
  group_by(State)%>%
  summarise("count"=n())%>%
    arrange(desc(count))
####  1 CA    13908
####  2 NY     8340
####  3 FL     8208
####  4 TX     6415
####  5 NJ     4268

#timely response
Timely_monetary_relief<-monetary_relief%>%
  group_by(Timely.response.)%>%
  summarise("count"=n())
#### 1 No                1251
#### 2 Yes              95814

#timely response
Timely_monetary_relief_others<-monetary_relief_others%>%
  group_by(Timely.response.)%>%
  summarise("count"=n())
#### 1 No                 38903
#### 2 Yes              1871806

#### trend for the no. of complaints

