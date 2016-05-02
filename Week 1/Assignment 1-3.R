CPS <- read.csv("C:/Users/JaroD/Desktop/New Start/MOOC Courses/The Analytics Edge - MIT/Week 1/CPSData.csv")

nrow(CPS)

which.max(table(CPS$Industry))

sort(table(CPS$State)) 
which.min(sort(table(CPS$State)))
which.max(sort(table(CPS$State)))

table(CPS$Citizenship)

table(CPS$Race,CPS$Hispanic)

sapply(CPS, function(x) sum(is.na(x)))

table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

table(CPS$State, is.na(CPS$MetroAreaCode))

p24 = table(CPS$Region, is.na(CPS$MetroAreaCode))
p24[,2]/(p24[,1]+p24[,2])

sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

MetroAreaMap <- read.csv("E:/New Start/MOOC Courses/The Analytics Edge - MIT/Week 1/MetroAreaCodes.csv")
CountryMap <- read.csv("E:/New Start/MOOC Courses/The Analytics Edge - MIT/Week 1/CountryCodes.csv")

nrow(MetroAreaMap)
nrow(CountryMap)

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

colnames(CPS)
sum(is.na(CPS$MetroArea))

sort(table(CPS$MetroArea))

sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))

which.min(sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm=TRUE)))

CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode",by.y="Code", all.x=TRUE)
colnames(CPS)
sum(is.na(CPS$Country))

sort(table(CPS$Country))

table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")
1668/(1668+3736)

sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm=TRUE))
sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, sum, na.rm=TRUE))
sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm=TRUE))
