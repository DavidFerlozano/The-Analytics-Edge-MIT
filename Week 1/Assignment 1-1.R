mvt <- read.csv("E:/New Start/MOOC Courses/The Analytics Edge - MIT/Week 1/mvtWeek1.csv")

nrow(mvt)

ncol(mvt)

max(mvt$ID)

min(mvt$Beat)

length(which(mvt$Arrest==TRUE))

length(which(mvt$LocationDescription=="ALLEY"))

mvt$Date[1]

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))

median(DateConvert)

mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

which.min(table(mvt$Month))

which.max(table(mvt$Weekday))

table(mvt$Arrest,mvt$Month)

hist(mvt$Date, breaks=100)

boxplot(mvt$Date~mvt$Arrest)

table(mvt$Arrest, mvt$Year)
2152/(2152+18517)

1212/(1212+13068)

550/(550+13542)

sort(table(mvt$LocationDescription))

Top5 = subset(mvt, LocationDescription=="STREET" | 
                LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | 
                LocationDescription=="ALLEY" | 
                LocationDescription=="GAS STATION" | 
                LocationDescription=="DRIVEWAY - RESIDENTIAL")
nrow(Top5)

Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription,Top5$Arrest)

table(Top5$LocationDescription=="GAS STATION", Top5$Weekday)
table(Top5$LocationDescription=="DRIVEWAY - RESIDENTIAL", Top5$Weekday)
