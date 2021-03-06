library(readr)
library(ggplot2)
library(plotly)
wage <- read_csv("C:/Users/Daniel Chiang/Desktop/career/wage.csv")

#薪資分佈圖
ggplot(data = wage)+geom_density(aes(x=wage),fill="grey50" ,title(main="薪資分佈(經常性薪資)",sub="D1：第1十分位數 20,768元D2：第2十分位數 25,539元D3：第3十分位數 28,130元D4：第4十分位數 30,840元D5：第5十分位數(中位數) 33,502元D6：第6十分位數 36,942元D7：第7十分位數 41,560元D8：第8十分位數 48,390元D9：第9十分位數 61,437元"))
#各行業薪資
  #礦業及土石採取業
Mining_and_earthwork_industry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Mining and earthwork industry.csv")

year1 <- Mining_and_earthwork_industry$year
Recurrentsalary1<- Mining_and_earthwork_industry$`Recurrent salary`
NonRecurrentsalary1 <- Mining_and_earthwork_industry$`Non-Recurrentsalary`
data1 <- data.frame(year1,Recurrentsalary1,NonRecurrentsalary1)
p1 <- plot_ly(data1, x = ~year1, y = ~Recurrentsalary1, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary1, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')

#砂、石及黏土採取業
Sandstoneandclayindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Sandstoneandclayindustry.csv")

year2 <- Sandstoneandclayindustry$year
Recurrentsalary2<- Sandstoneandclayindustry$`Recurrent salary`
NonRecurrentsalary2 <- Sandstoneandclayindustry$`Non-Recurrentsalary`
data2 <- data.frame(year2,Recurrentsalary2,NonRecurrentsalary2)
p2 <- plot_ly(data2, x = ~year2, y = ~Recurrentsalary2, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary2, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#食品製造業
Foodmanufacturingindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Foodmanufacturingindustry.csv")

year3 <- Foodmanufacturingindustry$year
Recurrentsalary3<- Foodmanufacturingindustry$`Recurrent salary`
NonRecurrentsalary3 <- Foodmanufacturingindustry$`Non-Recurrentsalary`
data3 <- data.frame(year3,Recurrentsalary3,NonRecurrentsalary3)
p3 <- plot_ly(data3, x = ~year3, y = ~Recurrentsalary3, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary3, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#飲料及菸草製造業
Beverageandtobaccomanufacturingindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Beverageandtobaccomanufacturingindustry.csv")

year4 <- Beverageandtobaccomanufacturingindustry$year
Recurrentsalary4<- Beverageandtobaccomanufacturingindustry$`Recurrent salary`
NonRecurrentsalary4<- Beverageandtobaccomanufacturingindustry$`Non-Recurrentsalary`
data4 <- data.frame(year4,Recurrentsalary4,NonRecurrentsalary4)
p4 <- plot_ly(data4, x = ~year4, y = ~Recurrentsalary4, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary4, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#紡織業
Textileindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Textileindustry.csv")

year5 <- Textileindustry$year
Recurrentsalary5<- Textileindustry$`Recurrent salary`
NonRecurrentsalary5<- Textileindustry$`Non-Recurrentsalary`
data4 <- data.frame(year5,Recurrentsalary5,NonRecurrentsalary5)
p5 <- plot_ly(data5, x = ~year5, y = ~Recurrentsalary5, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary5, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#成衣及服飾品製造業
Clothingandapparelmanufacturingindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Clothingandapparelmanufacturingindustry.csv")

year6 <- Clothingandapparelmanufacturingindustry$year
Recurrentsalary6<- Clothingandapparelmanufacturingindustry$`Recurrent salary`
NonRecurrentsalary6<- Clothingandapparelmanufacturingindustry$`Non-Recurrentsalary`
data4 <- data.frame(year6,Recurrentsalary6,NonRecurrentsalary6)
p6 <- plot_ly(data6, x = ~year6, y = ~Recurrentsalary6, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary6, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#皮革、毛皮及其製品製造業
Leatherfurproductsmanufacturingindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Leatherfurproductsmanufacturingindustry.csv")

year7 <- Leatherfurproductsmanufacturingindustry$year
Recurrentsalary7<- Leatherfurproductsmanufacturingindustry$`Recurrent salary`
NonRecurrentsalary7<- Leatherfurproductsmanufacturingindustry$`Non-Recurrentsalary`
data7 <- data.frame(year7,Recurrentsalary7,NonRecurrentsalary7)
p7 <- plot_ly(data7, x = ~year7, y = ~Recurrentsalary7, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary7, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#木竹製品製造業
Woodandbambooproductsmanufacturingindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Woodandbambooproductsmanufacturingindustry.csv")

year8 <- Woodandbambooproductsmanufacturingindustry$year
Recurrentsalary8<- Woodandbambooproductsmanufacturingindustry$`Recurrent salary`
NonRecurrentsalary8<- Woodandbambooproductsmanufacturingindustry$`Non-Recurrentsalary`
data8 <- data.frame(year8,Recurrentsalary8,NonRecurrentsalary8)
p8 <- plot_ly(data8, x = ~year8, y = ~Recurrentsalary8, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary8, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#紙漿、紙及紙製品製造業
Paperandpaperproductsmanufacturing <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Paperandpaperproductsmanufacturing.csv")

year9 <- Paperandpaperproductsmanufacturing$year
Recurrentsalary9<- Paperandpaperproductsmanufacturing$`Recurrent salary`
NonRecurrentsalary9<- Paperandpaperproductsmanufacturing$`Non-Recurrentsalary`
data9 <- data.frame(year9,Recurrentsalary9,NonRecurrentsalary9)
p9 <- plot_ly(data9, x = ~year9, y = ~Recurrentsalary9, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary9, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#印刷及資料儲存媒體複製業
Printinganddatastoragemediacopyindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Printinganddatastoragemediacopyindustry.csv")

year10 <- Printinganddatastoragemediacopyindustry$year
Recurrentsalary10<- Printinganddatastoragemediacopyindustry$`Recurrent salary`
NonRecurrentsalary10<- Printinganddatastoragemediacopyindustry$`Non-Recurrentsalary`
data10 <- data.frame(year10,Recurrentsalary10,NonRecurrentsalary10)
p10 <- plot_ly(data10, x = ~year10, y = ~Recurrentsalary10, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary10, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#石油及煤製品製造業
Oilandcoalproductsmanufacturingindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Oilandcoalproductsmanufacturingindustry.csv")

year11 <- Oilandcoalproductsmanufacturingindustry$year
Recurrentsalary11<- Oilandcoalproductsmanufacturingindustry$`Recurrent salary`
NonRecurrentsalary11<- Oilandcoalproductsmanufacturingindustry$`Non-Recurrentsalary`
data11 <- data.frame(year11,Recurrentsalary11,NonRecurrentsalary11)
p11 <- plot_ly(data11, x = ~year11, y = ~Recurrentsalary11, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary11, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#化學材料製造業
ChemicalMaterialsManufacturing <- read_csv("C:/Users/Daniel Chiang/Desktop/career/ChemicalMaterialsManufacturing.csv")

year12 <- ChemicalMaterialsManufacturing$year
Recurrentsalary12<- ChemicalMaterialsManufacturing$`Recurrent salary`
NonRecurrentsalary12<- ChemicalMaterialsManufacturing$`Non-Recurrentsalary`
data12 <- data.frame(year12,Recurrentsalary12,NonRecurrentsalary12)
p12 <- plot_ly(data12, x = ~year12, y = ~Recurrentsalary12, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary12, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#藥品及醫用化學製品製造業
Pharmaceuticalandmedicalchemicalsmanufacturing <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Pharmaceuticalandmedicalchemicalsmanufacturing.csv")

year13 <- Pharmaceuticalandmedicalchemicalsmanufacturing $year
Recurrentsalary13<- Pharmaceuticalandmedicalchemicalsmanufacturing $`Recurrent salary`
NonRecurrentsalary13<- Pharmaceuticalandmedicalchemicalsmanufacturing $`Non-Recurrentsalary`
data13 <- data.frame(year13,Recurrentsalary13,NonRecurrentsalary13)
p13 <- plot_ly(data13, x = ~year13, y = ~Recurrentsalary13, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary13, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#塑膠製品製造業
Plasticproductsmanufacturingindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Plasticproductsmanufacturingindustry.csv")

year14 <- Plasticproductsmanufacturingindustry $year
Recurrentsalary14<- Plasticproductsmanufacturingindustry $`Recurrent salary`
NonRecurrentsalary14<- Plasticproductsmanufacturingindustry $`Non-Recurrentsalary`
data14 <- data.frame(year14,Recurrentsalary14,NonRecurrentsalary14)
p14 <- plot_ly(data14, x = ~year14, y = ~Recurrentsalary14, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary14, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#基本金屬製造業
Basicmetalmanufacturing <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Basicmetalmanufacturing.csv")

year15 <- Basicmetalmanufacturing $year
Recurrentsalary15<- Basicmetalmanufacturing $`Recurrent salary`
NonRecurrentsalary15<- Basicmetalmanufacturing $`Non-Recurrentsalary`
data15 <- data.frame(year15,Recurrentsalary15,NonRecurrentsalary15)
p15 <- plot_ly(data15, x = ~year15, y = ~Recurrentsalary15, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary15, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#金屬製品製造業
Metalproductsmanufacturingindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Metalproductsmanufacturingindustry.csv")

year16 <- Metalproductsmanufacturingindustry $year
Recurrentsalary16<- Metalproductsmanufacturingindustry $`Recurrent salary`
NonRecurrentsalary16<- Metalproductsmanufacturingindustry $`Non-Recurrentsalary`
data16 <- data.frame(year16,Recurrentsalary16,NonRecurrentsalary16)
p16 <- plot_ly(data16, x = ~year16, y = ~Recurrentsalary16, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary16, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#電腦、電子產品及光學製品製造業
Computers_electronic_products_and_optical_products_manufacturing <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Computers, electronic products and optical products manufacturing.csv")

year17 <- Computers_electronic_products_and_optical_products_manufacturing $year
Recurrentsalary17<- Computers_electronic_products_and_optical_products_manufacturing $`Recurrent salary`
NonRecurrentsalary17<- Computers_electronic_products_and_optical_products_manufacturing $`Non-Recurrentsalary`
data17 <- data.frame(year17,Recurrentsalary17,NonRecurrentsalary17)
p17 <- plot_ly(data17, x = ~year17, y = ~Recurrentsalary17, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary17, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#電力設備製造業
Electricalequipmentmanufacturingindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Electricalequipmentmanufacturingindustry.csv")

year18 <- Electricalequipmentmanufacturingindustry $year
Recurrentsalary18<- Electricalequipmentmanufacturingindustry $`Recurrent salary`
NonRecurrentsalary18<- Electricalequipmentmanufacturingindustry $`Non-Recurrentsalary`
data18 <- data.frame(year18,Recurrentsalary18,NonRecurrentsalary18)
p18 <- plot_ly(data18, x = ~year18, y = ~Recurrentsalary18, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary18, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#汽車及其零件製造業
Caranditspartsmanufacturingindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Caranditspartsmanufacturingindustry.csv")

year19 <- Caranditspartsmanufacturingindustry $year
Recurrentsalary19<- Caranditspartsmanufacturingindustry $`Recurrent salary`
NonRecurrentsalary19<- Caranditspartsmanufacturingindustry $`Non-Recurrentsalary`
data19 <- data.frame(year19,Recurrentsalary19,NonRecurrentsalary19)
p19 <- plot_ly(data19, x = ~year19, y = ~Recurrentsalary19, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary19, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#其他運輸工具及其零件製造業
Othertransportandpartsmanufacturing <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Othertransportandpartsmanufacturing.csv")

year20 <- Othertransportandpartsmanufacturing $year
Recurrentsalary20<- Othertransportandpartsmanufacturing $`Recurrent salary`
NonRecurrentsalary20<- Othertransportandpartsmanufacturing $`Non-Recurrentsalary`
data20 <- data.frame(year20,Recurrentsalary20,NonRecurrentsalary20)
p20 <- plot_ly(data20, x = ~year20, y = ~Recurrentsalary20, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary20, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#家具製造業
Furnituremanufacturing <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Furnituremanufacturing.csv")

year21 <- Furnituremanufacturing $year
Recurrentsalary21<- Furnituremanufacturing $`Recurrent salary`
NonRecurrentsalary21<- Furnituremanufacturing $`Non-Recurrentsalary`
data21 <- data.frame(year21,Recurrentsalary21,NonRecurrentsalary21)
p21 <- plot_ly(data21, x = ~year21, y = ~Recurrentsalary21, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary21, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#其他製造業
Othermanufacturing <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Othermanufacturing.csv")

year21 <- Furnituremanufacturing $year
Recurrentsalary21<- Furnituremanufacturing $`Recurrent salary`
NonRecurrentsalary21<- Furnituremanufacturing $`Non-Recurrentsalary`
data21 <- data.frame(year21,Recurrentsalary21,NonRecurrentsalary21)
p21 <- plot_ly(data21, x = ~year21, y = ~Recurrentsalary21, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary21, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#產業用機械設備維修及安裝業
Industrialmachineryandequipmentrepairandinstallationindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Industrialmachineryandequipmentrepairandinstallationindustry.csv")

year22 <- Industrialmachineryandequipmentrepairandinstallationindustry $year
Recurrentsalary22<- Industrialmachineryandequipmentrepairandinstallationindustry $`Recurrent salary`
NonRecurrentsalary22<- Industrialmachineryandequipmentrepairandinstallationindustry $`Non-Recurrentsalary`
data22 <- data.frame(year22,Recurrentsalary22,NonRecurrentsalary22)
p22 <- plot_ly(data22, x = ~year22, y = ~Recurrentsalary22, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary22, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#電力供應業
Electricitysupplyindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Electricitysupplyindustry.csv")

year23 <- Electricitysupplyindustry $year
Recurrentsalary23<- Electricitysupplyindustry $`Recurrent salary`
NonRecurrentsalary23<- Electricitysupplyindustry $`Non-Recurrentsalary`
data23 <- data.frame(year23,Recurrentsalary23,NonRecurrentsalary23)
p23 <- plot_ly(data23, x = ~year23, y = ~Recurrentsalary23, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary23, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#氣體燃料供應業
Gasfuelsupplyindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Gasfuelsupplyindustry.csv")

year24 <- Gasfuelsupplyindustry $year
Recurrentsalary24<- Gasfuelsupplyindustry $`Recurrent salary`
NonRecurrentsalary24<- Gasfuelsupplyindustry $`Non-Recurrentsalary`
data24 <- data.frame(year24,Recurrentsalary24,NonRecurrentsalary24)
p24 <- plot_ly(data24, x = ~year24, y = ~Recurrentsalary24, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary24, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#用水供應業
Watersupplyindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Watersupplyindustry.csv")

year25 <- Watersupplyindustry $year
Recurrentsalary25<- Watersupplyindustry $`Recurrent salary`
NonRecurrentsalary25<- Watersupplyindustry $`Non-Recurrentsalary`
data25 <- data.frame(year25,Recurrentsalary25,NonRecurrentsalary25)
p25 <- plot_ly(data25, x = ~year25, y = ~Recurrentsalary25, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary25, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#廢（污）水處理業
Wastewatertreatmentindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Wastewatertreatmentindustry.csv")

year26 <- Wastewatertreatmentindustry $year
Recurrentsalary26<- Wastewatertreatmentindustry $`Recurrent salary`
NonRecurrentsalary26<- Wastewatertreatmentindustry $`Non-Recurrentsalary`
data26 <- data.frame(year26,Recurrentsalary26,NonRecurrentsalary26)
p26 <- plot_ly(data26, x = ~year26, y = ~Recurrentsalary26, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary26, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#廢棄物清除業
Wasteremovalindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Wasteremovalindustry.csv")

year27 <- Wasteremovalindustry $year
Recurrentsalary27<- Wasteremovalindustry $`Recurrent salary`
NonRecurrentsalary27<- Wasteremovalindustry $`Non-Recurrentsalary`
data27 <- data.frame(year27,Recurrentsalary27,NonRecurrentsalary27)
p27 <- plot_ly(data27, x = ~year27, y = ~Recurrentsalary27, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary27, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#廢棄物處理業
Wastetreatmentindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Wastetreatmentindustry.csv")

year28 <- Wastetreatmentindustry $year
Recurrentsalary28<- Wastetreatmentindustry $`Recurrent salary`
NonRecurrentsalary28<- Wastetreatmentindustry $`Non-Recurrentsalary`
data28 <- data.frame(year28,Recurrentsalary28,NonRecurrentsalary28)
p28 <- plot_ly(data28, x = ~year28, y = ~Recurrentsalary28, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary28, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#污染整治業
Pollutionremediationindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Pollutionremediationindustry.csv")

year29 <- Pollutionremediationindustry $year
Recurrentsalary29<- Pollutionremediationindustry $`Recurrent salary`
NonRecurrentsalary29<- Pollutionremediationindustry $`Non-Recurrentsalary`
data29 <- data.frame(year29,Recurrentsalary29,NonRecurrentsalary29)
p29 <- plot_ly(data29, x = ~year29, y = ~Recurrentsalary29, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary29, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#建築工程業
ConstructionIndustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/ConstructionIndustry.csv")

year30 <- ConstructionIndustry $year
Recurrentsalary30 <- ConstructionIndustry $`Recurrent salary`
NonRecurrentsalary30 <- ConstructionIndustry $`Non-Recurrentsalary`
data30  <- data.frame(year30 ,Recurrentsalary30 ,NonRecurrentsalary30 )
p30  <- plot_ly(data30, x = ~year30, y = ~Recurrentsalary30, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary30 , name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#土木工程業
Civilengineeringindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Civilengineeringindustry.csv")

year31 <- Civilengineeringindustry $year
Recurrentsalary31<- Civilengineeringindustry $`Recurrent salary`
NonRecurrentsalary31<- Civilengineeringindustry $`Non-Recurrentsalary`
data31 <- data.frame(year31,Recurrentsalary31,NonRecurrentsalary31)
p31 <- plot_ly(data31, x = ~year31, y = ~Recurrentsalary31, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary31, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#機電、管道及其他建築設備安裝業
Electricalplumbingandotherconstructionequipmentinstallationindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Electricalplumbingandotherconstructionequipmentinstallationindustry.csv")

year32<- Electricalplumbingandotherconstructionequipmentinstallationindustry $year
Recurrentsalary232<- Electricalplumbingandotherconstructionequipmentinstallationindustry $`Recurrent salary`
NonRecurrentsalary32<- Electricalplumbingandotherconstructionequipmentinstallationindustry $`Non-Recurrentsalary`
data32 <- data.frame(year21,Recurrentsalary32,NonRecurrentsalary32)
p32 <- plot_ly(data32, x = ~year32, y = ~Recurrentsalary32, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary32, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#其他專門營造業
Otherspecializedconstructionindustry <- read_csv("C:/Users/Daniel Chiang/Desktop/career/Otherspecializedconstructionindustry.csv")

year33<- Otherspecializedconstructionindustry $year
Recurrentsalary33<- Otherspecializedconstructionindustry $`Recurrent salary`
NonRecurrentsalary33<- Otherspecializedconstructionindustry $`Non-Recurrentsalary`
data33<- data.frame(year33,Recurrentsalary33,NonRecurrentsalary33)
p33 <- plot_ly(data33, x = ~year33, y = ~Recurrentsalary33, type = 'bar', name = 'Recurrentsalary') %>%
  add_trace(y = ~NonRecurrentsalary33, name = 'Non-Recurrentsalary') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
#男女
malefemale <- read_csv("C:/Users/Daniel Chiang/Desktop/career/malefemale.csv")
year<- c(malefemale$year)
male<- c(malefemale$male)
female<- c(malefemale$female)

data <- data.frame(year,male,female)

pm <- plot_ly( data,x = ~year, y = ~male, name = 'male', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~female, name = 'female', mode = 'lines')
#工作機會
opportunity <- read_csv("C:/Users/Daniel Chiang/Desktop/career/opportunity.csv")

p <- plot_ly(data = opportunity, x = ~opportunity$vacancy, y = ~opportunity$averagewage,
             marker = list(size = 10,
                           color = 'rgba(255, 182, 193, .9)',
                           line = list(color = 'rgba(152, 0, 0, .8)',
                                       width = 2))) %>%
  layout(title = 'vacancy',
         yaxis = list(zeroline = FALSE),
         xaxis = list(zeroline = FALSE))