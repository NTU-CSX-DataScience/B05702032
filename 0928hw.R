library(dplyr)
library(customer)
data <- read.csv("customer.csv", header=T, sep=",")
#1. 年紀與花費習慣的關係#
mutate(data,cardspent+card2spent)
data <-data%>%
  mutate(spendtotal = cardspent+card2spent)

data$spendtotal<- cut(data$spendtotal, seq(0,2000,400))
require(ggplot2)
qplot(x=age,data=data,geom = "histogram",main = "histogram of spendtotal",xlab="age(years-old)",binwidth=10,fill=spendtotal
)


#2. 教育程度與收入是否成正比 #
canvas<-ggplot(data=data)+
  geom_point(aes(x=edcat,y=income,color=income))+
  geom_smooth(aes(x=edcat,y=income))+
  labs(title="Scatter of Education-Income",x="edcat",y="income")+
  theme_bw()

#3.年紀和地區與養寵物的關係 #
data <-data%>%
  mutate(pettotal=pets_cats+pets_dogs+pets_birds+pets_small+pets_saltfish+pets_freshfish)

data2<-select(data,pettotal,region,age)%>%
  filter(pettotal>=1)
require("lattice")
xyplot(x=region~age,data=data2,group=pettotal,auto.key=list(space="top",columns=12,title="pettotal",cex.title=1))




