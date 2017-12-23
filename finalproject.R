library(ggplot2)
library(readr)
Dividendyield <- read_csv("C:/Users/Daniel Chiang/Desktop/finalproject/Dividendyield1.csv")
EPS <- read_csv("C:/Users/Daniel Chiang/Desktop/finalproject/EPS1.csv")
Revenue <- read_csv("C:/Users/Daniel Chiang/Desktop/finalproject/Revenue1.csv")
ROA <- read_csv("C:/Users/Daniel Chiang/Desktop/finalproject/ROA1.csv")
ROE <- read_csv("C:/Users/Daniel Chiang/Desktop/finalproject/ROE1.csv")

#每月營收
barplot(Revenue$`2330`,main="Revenue",xlab="month", ylab="revenue",col="light blue")

#單季每股盈餘
barplot(EPS$`2330`,main="EPS",xlab="quarter", ylab="EPS",col="light blue")

#單季ROA
ggplot(ROA, aes(x = ROA$quarter, y = ROA$`2330`)) +geom_line()+geom_point()

#單季ROE
ggplot(ROE, aes(x = ROE$quarter, y =ROE$`2330`)) +geom_line()+geom_point() 

#單季現金股利殖利率
ggplot(Dividendyield, aes(x = Dividendyield$month, y = Dividendyield$`2330`)) +geom_line()+geom_point() 
