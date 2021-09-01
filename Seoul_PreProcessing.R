
#Data Preprocessing Using R

getwd()

#Importing the Dataset
dataset= read.csv("SeoulBikeData_1.csv")

str(dataset)


#Finding missing values in the dataset
str(dataset)
summary(dataset)
summary(dataset$Holiday)
which(is.na(dataset)) #No Missing Values found in the Dataset


View(dataset)

#Encoding Categorical Variables

dataset$Seasons = factor(dataset$Seasons, levels = c('Autumn', 'Spring', 'Summer', 'Winter'), labels = c(1, 2, 3, 4))

dataset$Functioning.Day = factor(dataset$Functioning.Day, levels = c('Yes', 'No'), labels = c(1, 2))

dataset$Holiday = factor(dataset$Holiday, levels = c('No Holiday', 'Holiday'), labels = c(0, 1))

#Exporting the Transformed Dataset

write.csv(dataset, "Seoul1.csv")

#Scaling the Dataset
dataset_scale=dataset

View(dataset_scale)

dataset_scale[, 3:11] = scale(dataset_scale[,3:11])

#Exporting the Scaled Dataset

write.csv(dataset_scale, "Seoul2.csv")

#ScatterPlot Analysis

plot(dataset$Rented.Bike.Count, dataset$Hour, xlab = "Rental Bike Count", ylab = "Hour of the Day")

plot(dataset$Rented.Bike.Count, dataset$Temperature..C., xlab = "Rental Bike Count", ylab = "Temperature")

plot(dataset$Rented.Bike.Count, dataset$Humidity..., xlab = "Rental Bike Count", ylab = "Humidity")

plot(dataset$Rented.Bike.Count, dataset$Wind.speed..m.s., xlab = "Rental Bike Count", ylab = "Wind Speed(m/s)")

plot(dataset$Rented.Bike.Count, dataset$Visibility..10m., xlab = "Rental Bike Count", ylab = "Visibility (10m)")

plot(dataset$Rented.Bike.Count, dataset$Dew.point.temperature..C., xlab = "Rental Bike Count", ylab = "Dew Point Temperature")

plot(dataset$Rented.Bike.Count, dataset$Solar.Radiation..MJ.m2., xlab = "Rental Bike Count", ylab = "Solar Radiation(MJ/m2)")

plot(dataset$Rented.Bike.Count, dataset$Rainfall.mm., xlab = "Rental Bike Count", ylab = "Rainfall(mm)")

plot(dataset$Rented.Bike.Count, dataset$Snowfall..cm., xlab = "Rental Bike Count", ylab = "Snowfall(cm)")

plot(dataset$Rented.Bike.Count, dataset$Seasons, xlab = "Rental Bike Count", ylab = "Seasons")

plot(dataset$Rented.Bike.Count, dataset$Holiday, xlab = "Rental Bike Count", ylab = "Holiday")

plot(dataset$Rented.Bike.Count, dataset$Functioning.Day, xlab = "Rental Bike Count", ylab = "Functioning Day")

linear_model=lm(dataset$Rented.Bike.Count~ dataset$Hour + dataset$Dew.point.temperature..C. + dataset$Temperature..C. + dataset$Humidity... + dataset$Wind.speed..m.s. + dataset$Solar.Radiation..MJ.m2.+ dataset$Rainfall.mm. + dataset$Snowfall..cm. + dataset$Seasons + dataset$Holiday + dataset$Functioning.Day , dataset)
summary(linear_model)


summary(dataset$Rented.Bike.Count)

#Rough Work

getwd()

data= read.csv("Seoul1.csv")

plot(data$Rented.Bike.Count, data$Holiday, xlab = "Rental Bike Count", ylab = "Holiday")


linear_model1=lm(data$Rented.Bike.Count~ data$Hour + data$Temperature..C. + data$Humidity... + data$Wind.speed..m.s. +  data$Dew.point.temperature..C.+ data$Solar.Radiation..MJ.m2.+ data$Rainfall.mm. + data$Snowfall..cm. + data$Seasons + data$Holiday, data)

summary(linear_model1)

#Outlier Analysis

str(data)
#Finding the index position with extreme data count
which(dataset$Rented.Bike.Count>3500)

#Removing the extreme Rental Count
dataset1= dataset[-4819,]
View(dataset1)
plot(dataset1$Rented.Bike.Count, dataset1$Functioning.Day, xlab = "Rental Bike Count", ylab = "Functioning Hour")

linear_model=lm(dataset1$Rented.Bike.Count~ dataset1$Hour + dataset1$Dew.point.temperature..C. + dataset1$Temperature..C. + dataset1$Humidity... + dataset1$Wind.speed..m.s. + dataset1$Solar.Radiation..MJ.m2.+ dataset1$Rainfall.mm. + dataset1$Snowfall..cm. + dataset1$Seasons + dataset1$Holiday + dataset1$Functioning.Day , dataset1)
summary(linear_model)

#Finding index positions of Rental Bike Count during Non-Functional Hours

which(dataset1$Functioning.Day == 2)

#Finding index of Snowfall above 6cm

which(dataset1$Snowfall..cm.>6)

dataset2 = dataset1[-c(8601,8602,8603,8604),]

linear_model1=lm(dataset2$Rented.Bike.Count~ dataset2$Hour + dataset2$Dew.point.temperature..C. + dataset2$Temperature..C. + dataset2$Humidity... + dataset2$Wind.speed..m.s. + dataset2$Solar.Radiation..MJ.m2.+ dataset2$Rainfall.mm. + dataset2$Snowfall..cm. + dataset2$Seasons + dataset2$Holiday + dataset2$Functioning.Day , dataset2)
summary(linear_model1)
View(dataset2)

#Finding index of Rainfall above 25 mm

which(dataset2$Rainfall.mm.>25)

dataset3 = dataset2[-c(3998,6501),]

linear_model2 = lm(dataset3$Rented.Bike.Count~ dataset3$Hour + dataset3$Dew.point.temperature..C. + dataset3$Temperature..C. + dataset3$Humidity... + dataset3$Wind.speed..m.s. + dataset3$Solar.Radiation..MJ.m2.+ dataset3$Rainfall.mm. + dataset3$Snowfall..cm. + dataset3$Seasons + dataset3$Holiday + dataset3$Functioning.Day , dataset3)
summary(linear_model2)


#Finding index of Wind Speed greater than 6 m/s

which(dataset$Wind.speed..m.s.>6)

dataset4 = dataset3[-which(dataset$Wind.speed..m.s.>6),]

linear_model3 = lm(dataset4$Rented.Bike.Count~ dataset4$Hour + dataset4$Dew.point.temperature..C. + dataset4$Temperature..C. + dataset4$Humidity... + dataset4$Wind.speed..m.s. + dataset4$Solar.Radiation..MJ.m2.+ dataset4$Rainfall.mm. + dataset4$Snowfall..cm. + dataset4$Seasons + dataset4$Holiday + dataset4$Functioning.Day , dataset4)
summary(linear_model3)

#Finding datapoints whee Humidity is entered as zero

which(dataset4$Humidity == 0)

dataset5 = dataset4[-which(dataset4$Humidity == 0),]

linear_model4 = lm(dataset5$Rented.Bike.Count~ dataset5$Hour + dataset5$Dew.point.temperature..C. +  dataset5$Humidity... + dataset5$Wind.speed..m.s. + dataset5$Solar.Radiation..MJ.m2.+ dataset5$Rainfall.mm. + dataset5$Snowfall..cm. + dataset5$Seasons + dataset5$Holiday + dataset5$Functioning.Day , dataset5)
summary(linear_model4)

dataset6 = dataset5[,-1]
View(dataset6)

write.csv(dataset5, "Seoul3.csv")

write.csv(dataset6, "SeoulFinalDataSet.csv")


#Final Plots

plot(dataset8$Rented.Bike.Count, dataset8$Hour, xlab = "Rental Bike Count", ylab = "Hour of the Day")

plot(dataset8$Rented.Bike.Count, dataset8$Temperature..C., xlab = "Rental Bike Count", ylab = "Temperature")

plot(dataset8$Rented.Bike.Count, dataset8$Humidity..., xlab = "Rental Bike Count", ylab = "Humidity")

plot(dataset8$Rented.Bike.Count, dataset8$Wind.speed..m.s., xlab = "Rental Bike Count", ylab = "Wind Speed(m/s)")

plot(dataset8$Rented.Bike.Count, dataset8$Visibility..10m., xlab = "Rental Bike Count", ylab = "Visibility (10m)")

plot(dataset8$Rented.Bike.Count, dataset8$Dew.point.temperature..C., xlab = "Rental Bike Count", ylab = "Dew Point Temperature")

plot(dataset8$Rented.Bike.Count, dataset8$Solar.Radiation..MJ.m2., xlab = "Rental Bike Count", ylab = "Solar Radiation(MJ/m2)")

plot(dataset8$Rented.Bike.Count, dataset8$Rainfall.mm., xlab = "Rental Bike Count", ylab = "Rainfall(mm)")

plot(dataset8$Rented.Bike.Count, dataset8$Snowfall..cm., xlab = "Rental Bike Count", ylab = "Snowfall(cm)")

plot(dataset8$Rented.Bike.Count, dataset8$Seasons, xlab = "Rental Bike Count", ylab = "Seasons")

plot(dataset8$Rented.Bike.Count, dataset8$Holiday, xlab = "Rental Bike Count", ylab = "Holiday")

plot(dataset8$Rented.Bike.Count, dataset8$Functioning.Day, xlab = "Rental Bike Count", ylab = "Functioning Day")

#Finding Isolated DataPoints

which(dataset7$Wind.speed..m.s.> 5)

which(dataset7$Rainfall.mm.> 10)

which(dataset6$Snowfall..cm.> 4)

dataset7=dataset6[-c(421, 422, 5011, 5109, 6293, 6499, 8576, 8577),]

linear_model5 = lm(dataset7$Rented.Bike.Count~ dataset7$Hour + dataset7$Dew.point.temperature..C. +  dataset7$Humidity... + dataset7$Wind.speed..m.s. + dataset7$Solar.Radiation..MJ.m2.+ dataset7$Rainfall.mm. + dataset7$Snowfall..cm. + dataset7$Seasons + dataset7$Holiday + dataset7$Functioning.Day , dataset7)

summary(linear_model5)


dataset8=dataset7[-c(86, 88, 90, 225, 226, 227, 250, 254, 419, 420, 421, 422, 423, 972, 1307, 1407, 1720, 1721, 1722, 1789, 2018, 2019, 2168, 2172, 2175, 2503, 3040, 3063, 3131, 3133, 3134, 3135, 3136, 3175, 3706, 4005, 4009, 4021, 4170, 4663, 4664, 4684, 4954, 4956, 5072, 5073, 6469, 6471, 6610, 6611,  6656, 7885, 7906, 8198, 8201, 8214, 8576, 8577, 8578  ),]

linear_model6 = lm(dataset8$Rented.Bike.Count~ dataset8$Hour + dataset8$Dew.point.temperature..C. +  dataset8$Humidity... + dataset8$Wind.speed..m.s. + dataset8$Solar.Radiation..MJ.m2.+ dataset8$Rainfall.mm. + dataset8$Snowfall..cm. + dataset8$Seasons + dataset8$Holiday + dataset8$Functioning.Day , dataset8)

summary(linear_model6)

#Finding points where Rented Count>2500

which(dataset8$Rented.Bike.Count>2500)

dataset9=dataset8[-which(dataset8$Rented.Bike.Count>2500),]

linear_model7 = lm(dataset9$Rented.Bike.Count~ dataset9$Hour + dataset9$Dew.point.temperature..C. +  dataset9$Humidity... + dataset9$Solar.Radiation..MJ.m2.+ dataset9$Rainfall.mm. + dataset9$Snowfall..cm. + dataset9$Seasons + dataset9$Holiday + dataset9$Functioning.Day , dataset9)

summary(linear_model7)

#Verifying Plots

plot(dataset9$Rented.Bike.Count, dataset9$Hour, xlab = "Rental Bike Count", ylab = "Hour of the Day")

plot(dataset9$Rented.Bike.Count, dataset9$Temperature..C., xlab = "Rental Bike Count", ylab = "Temperature")

plot(dataset9$Rented.Bike.Count, dataset9$Humidity..., xlab = "Rental Bike Count", ylab = "Humidity")

plot(dataset9$Rented.Bike.Count, dataset9$Wind.speed..m.s., xlab = "Rental Bike Count", ylab = "Wind Speed(m/s)")

plot(dataset9$Rented.Bike.Count, dataset9$Visibility..10m., xlab = "Rental Bike Count", ylab = "Visibility (10m)")

plot(dataset9$Rented.Bike.Count, dataset9$Dew.point.temperature..C., xlab = "Rental Bike Count", ylab = "Dew Point Temperature")

plot(dataset9$Rented.Bike.Count, dataset9$Solar.Radiation..MJ.m2., xlab = "Rental Bike Count", ylab = "Solar Radiation(MJ/m2)")

plot(dataset9$Rented.Bike.Count, dataset9$Rainfall.mm., xlab = "Rental Bike Count", ylab = "Rainfall(mm)")

plot(dataset9$Rented.Bike.Count, dataset9$Snowfall..cm., xlab = "Rental Bike Count", ylab = "Snowfall(cm)")

plot(dataset9$Rented.Bike.Count, dataset9$Seasons, xlab = "Rental Bike Count", ylab = "Seasons")

plot(dataset9$Rented.Bike.Count, dataset9$Holiday, xlab = "Rental Bike Count", ylab = "Holiday")

plot(dataset9$Rented.Bike.Count, dataset9$Functioning.Day, xlab = "Rental Bike Count", ylab = "Functioning Day")

#Exporting Data

write.csv(dataset9, "SeoulFinal.csv")

