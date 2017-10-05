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
data3<-select(data,edcat,income)
boxplot(formula=income~edcat,data=data3,
        xlab="edcat",
        ylab="income",
        color="lightblue")
          

#3.年紀和地區的關係 #

data5<-data%>%select(region,age)
require(ggplot2)


canvas<-ggplot(data=data4)
canvas+geom_histogram(aes(x=age,fill=region))+facet_grid(.~region)+theme_bw()

