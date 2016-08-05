#Load dataset

data <- read.csv("CapstoneProjectTrainData.csv")
str(data)

# Find number of unique obs in each column
# apply(data, 2, function(x)length(unique(x)))

 #Assign 0 or 1 to Status_Binom. 0 == does not need repair, 1 == needs repair
data$Status_Binom <- with(data, ifelse(status_group == "functional", Status_Binom <- 0, Status_Binom <-1))

current_year <- 2016
data$age <- current_year - data$construction_year
#Remove all rows that do not have construction_year

data = data[data$age != 2016,]

#remove all rows with unknown management_group
data = data[data$management_group != "unknown", ]

#convert management_group into dummy variables

data$management_group_a <- with(data, ifelse(management_group == "commercial", management_group_a <- 1, management_group_a <- 0))
data$management_group_b <- with(data, ifelse(management_group == "other", management_group_b <- 1, management_group_b <- 0))
data$management_group_c <- with(data, ifelse(management_group == "parastatal", management_group_c <- 1, management_group_c <- 0))
data$management_group_d <- with(data, ifelse(management_group == "user-group", management_group_d <- 1, management_group_d <- 0))

#Identify the unique basins

#install.packages("dplyr")
library(dplyr)

distinct_basin = distinct(data, basin)
distinct_basin

#convert basins into dummy variable
data$basin_1 <- with(data, ifelse(basin == "Lake Nyasa", basin_1 <- 1, basin_1 <- 0))
data$basin_2 <- with(data, ifelse(basin == "Lake Victoria", basin_2 <- 1, basin_2 <- 0))
data$basin_3 <- with(data, ifelse(basin == "Pangani", basin_3 <- 1, basin_3 <- 0))
data$basin_4 <- with(data, ifelse(basin == "Ruvuma / Southern Coast", basin_4 <- 1, basin_1 <- 0))
data$basin_5 <- with(data, ifelse(basin == "Wami / Ruvu", basin_5 <- 1, basin_5 <- 0))
data$basin_6 <- with(data, ifelse(basin == "Lake Tanganyika", basin_6 <- 1, basin_6 <- 0))
data$basin_7 <- with(data, ifelse(basin == "Rufiji", basin_7 <- 1, basin_7 <- 0))
data$basin_8 <- with(data, ifelse(basin == "Internal", basin_8 <- 1, basin_8 <- 0))
data$basin_9 <- with(data, ifelse(basin == "Lake Rukwa", basin_9 <- 1, basin_9 <- 0))


#delete all rows with permit = NA
data <- data[!(is.na(data$permit)),]

#find unique public meetings
distinct_permit = distinct(data, permit)
distinct_permit

#convert public meeting into dummy 
data$permit_1 <- with(data, ifelse(permit == TRUE, permit_1 <- 1, permit_1 <- 0))


#delete all rows with public_meeting = NA
data <- data[!(is.na(data$public_meeting)),]

#find unique public meetings
distinct_public_meeting = distinct(data, public_meeting)
distinct_public_meeting

#convert public meeting into dummy 
data$public_meeting_1 <- with(data, ifelse(public_meeting == TRUE, public_meeting_1 <- 1, public_meeting_1 <- 0))

#delete all rows with extraction_type_class = NA
data <- data[!(is.na(data$extraction_type_class)),]

#find unique extraction type classes
distinct_extraction_type_class = distinct(data, extraction_type_class)
distinct_extraction_type_class

#convert extraction type class into dummy 
data$extraction_type_class_1 <- with(data, ifelse(extraction_type_class == "gravity", extraction_type_class_1 <- 1, extraction_type_class_1 <- 0))
data$extraction_type_class_2 <- with(data, ifelse(extraction_type_class == "submersible", extraction_type_class_2 <- 1, extraction_type_class_2 <- 0))
data$extraction_type_class_3 <- with(data, ifelse(extraction_type_class == "handpump", extraction_type_class_3 <- 1, extraction_type_class_3 <- 0))
data$extraction_type_class_4 <- with(data, ifelse(extraction_type_class == "other", extraction_type_class_4 <- 1, extraction_type_class_4 <- 0))
data$extraction_type_class_5 <- with(data, ifelse(extraction_type_class == "rope pump", extraction_type_class_5 <- 1, extraction_type_class_5 <- 0))
data$extraction_type_class_6 <- with(data, ifelse(extraction_type_class == "motorpump", extraction_type_class_6 <- 1, extraction_type_class_6 <- 0))
data$extraction_type_class_7 <- with(data, ifelse(extraction_type_class == "wind-powered", extraction_type_class_7 <- 1, extraction_type_class_7 <- 0))

#delete all rows with payment_type = NA
data <- data[!(is.na(data$payment_type)),]

#find unique payment_type
distinct_payment_type = distinct(data, payment_type)
distinct_payment_type

#convert payment_type into dummy 
data$payment_type_1 <- with(data, ifelse(payment_type == "gravity", payment_type_1 <- 1, payment_type_1 <- 0))
data$payment_type_2 <- with(data, ifelse(payment_type == "submersible", payment_type_2 <- 1, payment_type_2 <- 0))
data$payment_type_3 <- with(data, ifelse(payment_type == "handpump", payment_type_3 <- 1, payment_type_3 <- 0))
data$payment_type_4 <- with(data, ifelse(payment_type == "other", payment_type_4 <- 1, payment_type_4 <- 0))
data$payment_type_5 <- with(data, ifelse(payment_type == "rope pump", payment_type_5 <- 1, payment_type_5 <- 0))
data$payment_type_6 <- with(data, ifelse(payment_type == "motorpump", payment_type_6 <- 1, payment_type_6 <- 0))
data$payment_type_7 <- with(data, ifelse(payment_type == "wind-powered", payment_type_7 <- 1, payment_type_7 <- 0))


#delete all rows with water_quality = NA
data <- data[!(is.na(data$water_quality)),]

#find unique water quality
distinct_water_quality = distinct(data, water_quality)
distinct_water_quality

#convert payment_type into dummy 
data$water_quality_1 <- with(data, ifelse(water_quality == "soft", water_quality_1 <- 1, water_quality_1 <- 0))
data$water_quality_2 <- with(data, ifelse(water_quality == "salty", water_quality_2 <- 1, water_quality_2 <- 0))
data$water_quality_3 <- with(data, ifelse(water_quality == "unknown", water_quality_3 <- 1, water_quality_3 <- 0))
data$water_quality_4 <- with(data, ifelse(water_quality == "fluoride", water_quality_4 <- 1, water_quality_4 <- 0))
data$water_quality_5 <- with(data, ifelse(water_quality == "salty abandoned", water_quality_5 <- 1, water_quality_5 <- 0))
data$water_quality_6 <- with(data, ifelse(water_quality == "coloured", water_quality_6 <- 1, water_quality_6 <- 0))
data$water_quality_7 <- with(data, ifelse(water_quality == "milky", water_quality_7 <- 1, water_quality_7 <- 0))
data$water_quality_8 <- with(data, ifelse(water_quality == "fluoride abandoned", water_quality_8 <- 1, water_quality_8  <- 0))

#delete all rows with water_quality = NA
data <- data[!(is.na(data$waterpoint_type_group)),]

#find unique water quality
distinct_waterpoint_type_group = distinct(data, waterpoint_type_group)
distinct_waterpoint_type_group

#convert payment_type into dummy 
data$waterpoint_type_group_1 <- with(data, ifelse(waterpoint_type_group == "communal standpipe", waterpoint_type_group_1 <- 1, waterpoint_type_group_1 <- 0))
data$waterpoint_type_group_2 <- with(data, ifelse(waterpoint_type_group == "other", waterpoint_type_group_2 <- 1, waterpoint_type_group_2 <- 0))
data$waterpoint_type_group_3 <- with(data, ifelse(waterpoint_type_group == "hand pump", waterpoint_type_group_3 <- 1, waterpoint_type_group_3 <- 0))
data$waterpoint_type_group_4 <- with(data, ifelse(waterpoint_type_group == "improved spring", waterpoint_type_group_4 <- 1, waterpoint_type_group_4 <- 0))
data$waterpoint_type_group_5 <- with(data, ifelse(waterpoint_type_group == "dam", waterpoint_type_group_5 <- 1, waterpoint_type_group_5 <- 0))
data$waterpoint_type_group_6 <- with(data, ifelse(waterpoint_type_group == "cattle trough", waterpoint_type_group_6 <- 1, waterpoint_type_group_6 <- 0))


#delete all rows with source = NA
data <- data[!(is.na(data$source)),]

#find unique sources
distinct_source = distinct(data, source)
distinct_source

#convert source into dummy 
data$source_1 <- with(data, ifelse(source == "spring", source_1 <- 1, source_1 <- 0))
data$source_2 <- with(data, ifelse(source == "dam", source_2 <- 1, source_2 <- 0))
data$source_3 <- with(data, ifelse(source == "machine dbh", source_3 <- 1, source_3 <- 0))
data$source_4 <- with(data, ifelse(source == "other",source_4 <- 1, source_4 <- 0))
data$source_5 <- with(data, ifelse(source == "shallow well", source_5 <- 1, source_5 <- 0))
data$source_6 <- with(data, ifelse(source == "river", source_6 <- 1, source_6 <- 0))
data$source_7 <- with(data, ifelse(source == "rainwater harvesting", source_7 <- 1, source_7 <- 0))
data$source_8 <- with(data, ifelse(source == "hand dtw", source_8 <- 1, source_8 <- 0))
data$source_9 <- with(data, ifelse(source == "lake", source_9 <- 1, source_9 <- 0))
data$source_10 <- with(data, ifelse(source == "unknown", sourcep_10 <- 1, source_10 <- 0))

#delete all rows with quantity = NA
data <- data[!(is.na(data$quantity)),]

#find unique sources
distinct_quantity = distinct(data, quantity)
distinct_quantity

#convert source into dummy 
data$quantity_1 <- with(data, ifelse(quantity == "enough", quantity_1 <- 1, quantity_1 <- 0))
data$quantity_2 <- with(data, ifelse(quantity == "dry", quantity_2 <- 1, quantity_2 <- 0))
data$quantity_3 <- with(data, ifelse(quantity == "insufficient", quantity_3 <- 1, quantity_3 <- 0))
data$quantity_4 <- with(data, ifelse(quantity == "seasonal", quantity_4 <- 1, quantity_4 <- 0))
data$quantity_5 <- with(data, ifelse(quantity == "unknown", quantity_5 <- 1, quantity_5 <- 0))

#Find number of unique obs in each column
apply(data, 2, function(x)length(unique(x)))

#install.packages("caTools")
library(caTools)
#Split data into Train and Est
set.seed(45)
split = sample.split(data$Status_Binom, SplitRatio = 0.75)
split
dataTrain = subset(data, split == TRUE)
dataTest = subset(data, split == FALSE)

#Build model and check AIC of each one
RepairLog5 = glm(Status_Binom ~ gps_height + age + management_group_a + management_group_b + management_group_c + management_group_d + basin_1 + basin_2 + basin_3 + basin_4 + basin_5 + basin_6 +basin_7 + basin_8 + basin_9 + permit_1 + public_meeting_1 + extraction_type_class_1 + extraction_type_class_2 + extraction_type_class_3 + extraction_type_class_4 + extraction_type_class_5 + extraction_type_class_6 + extraction_type_class_7 + payment_type_1 +
                payment_type_2 +       
                payment_type_3 +        
                payment_type_4  +       
                payment_type_5  +       
                payment_type_6  +       
                payment_type_7 +
                waterpoint_type_group_1 +
                waterpoint_type_group_2 +
                waterpoint_type_group_3 +
                waterpoint_type_group_4 +
                waterpoint_type_group_5 +
                waterpoint_type_group_6 +
                source_1 +
                source_2 +
                source_3 +
                source_4 +
                source_5 +
                source_6 +
                source_7 +
                source_8 +
                source_9 +
                source_10 +
                quantity_1 +
                quantity_2 +
                quantity_3 +
                quantity_4 +
                quantity_5, data = dataTrain, family = binomial)
summary(RepairLog5)


#predict

predict_repair = predict(RepairLog5, type = "response")
summary(predict_repair)


tapply(predict_repair, dataTrain$Status_Binom, mean)


table(dataTrain$Status_Binom, predict_repair > 0.1)
table(dataTrain$Status_Binom, predict_repair > 0.2)
table(dataTrain$Status_Binom, predict_repair > 0.3)
table(dataTrain$Status_Binom, predict_repair > 0.4)
table(dataTrain$Status_Binom, predict_repair > 0.5)
table(dataTrain$Status_Binom, predict_repair > 0.6)
table(dataTrain$Status_Binom, predict_repair > 0.7)
table(dataTrain$Status_Binom, predict_repair > 0.8)
table(dataTrain$Status_Binom, predict_repair > 0.9)




accuracyTrain0.1 = (1106+11022)/(1106+13764+94+11022)
accuracyTrain0.2 = (4882+10363)/(4882+9898+753+10363)
accuracyTrain0.3 = (8838+9140)/(5942+8838+1976+9140)
accuracyTrain0.4 = (11438+7490)/(11438+3342+3626+7490)
accuracyTrain0.5 = (12939+6296)/(12939+1841+6296+4820)
accuracyTrain0.6 = (13822+5310)/(13822+5310+958+5806)
accuracyTrain0.7 = (14354+4493)/(14354+4493+426+6623)
accuracyTrain0.8 = (14541+3975)/(14541+3975+239+7141)
accuracyTrain0.9 = (14688+3098)/(14688+92+8018+3098)

accuracyTrain0.1
accuracyTrain0.2
accuracyTrain0.3
accuracyTrain0.4
accuracyTrain0.5
accuracyTrain0.6
accuracyTrain0.7
accuracyTrain0.8
accuracyTrain0.9

#threshold value of 0.5 is most accurate in Test data

#Test on dataTest
#Build model and check AIC of each one
RepairLog5 = glm(Status_Binom ~ gps_height + age + management_group_a + management_group_b + management_group_c + management_group_d + basin_1 + basin_2 + basin_3 + basin_4 + basin_5 + basin_6 +basin_7 + basin_8 + basin_9 + permit_1 + public_meeting_1 + extraction_type_class_1 + extraction_type_class_2 + extraction_type_class_3 + extraction_type_class_4 + extraction_type_class_5 + extraction_type_class_6 + extraction_type_class_7 + payment_type_1 +
                   payment_type_2 +       
                   payment_type_3 +        
                   payment_type_4  +       
                   payment_type_5  +       
                   payment_type_6  +       
                   payment_type_7 +
                   waterpoint_type_group_1 +
                   waterpoint_type_group_2 +
                   waterpoint_type_group_3 +
                   waterpoint_type_group_4 +
                   waterpoint_type_group_5 +
                   waterpoint_type_group_6 +
                   source_1 +
                   source_2 +
                   source_3 +
                   source_4 +
                   source_5 +
                   source_6 +
                   source_7 +
                   source_8 +
                   source_9 +
                   source_10 +
                   quantity_1 +
                   quantity_2 +
                   quantity_3 +
                   quantity_4 +
                   quantity_5, data = dataTest, family = binomial)
summary(RepairLog5)


#predict

predict_repair = predict(RepairLog5, type = "response")
summary(predict_repair)


tapply(predict_repair, dataTest$Status_Binom, mean)


table(dataTest$Status_Binom, predict_repair > 0.1)
table(dataTest$Status_Binom, predict_repair > 0.2)
table(dataTest$Status_Binom, predict_repair > 0.3)
table(dataTest$Status_Binom, predict_repair > 0.4)
table(dataTest$Status_Binom, predict_repair > 0.5)
table(dataTest$Status_Binom, predict_repair > 0.6)
table(dataTest$Status_Binom, predict_repair > 0.7)
table(dataTest$Status_Binom, predict_repair > 0.8)
table(dataTest$Status_Binom, predict_repair > 0.9)




accuracyTest0.1 = (439+3683)/(439+4487+22+3683)
accuracyTest0.2 = (1474+3487)/(1474+218+3487+3452)
accuracyTest0.3 = (2854+3073)/(2854+3073+632+2072)
accuracyTest0.4 = (3811+2470)/(3811+2470+1115+1235)
accuracyTest0.5 = (4282+2068)/(4282+2068+644+1637)
accuracyTest0.6 = (4593+1688)/(4593+333+2017+1688)
accuracyTest0.7 = (14767+1466)/(14767+1466+159+2239)
accuracyTest0.8 = (4850+1263)/(4853+1263+76+2442)
accuracyTest0.9 = (4896+945)/(4896+945+30+2760)

accuracyTest0.1
accuracyTest0.2
accuracyTest0.3
accuracyTest0.4
accuracyTest0.5
accuracyTest0.6
accuracyTest0.7
accuracyTest0.8
accuracyTest0.9

#**Threshhold value of t = 0.5 is still most accurate
plot(sort(predict_repair))