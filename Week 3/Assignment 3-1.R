songs <- read.csv("C:/Users/JaroD/Desktop/New Start/MOOC Courses/The Analytics Edge - MIT/Week 3/songs.csv")

table(songs$year)

nrow(subset(songs, artistname == "Michael Jackson"))

subset(subset(songs, artistname == "Michael Jackson"), Top10 == 1)

table(songs$timesignature)

songs$songtitle[which.max(songs$tempo)]

SongsTrain = subset(songs, year <= 2009)
SongsTest = subset(songs, year == 2010)
nrow(SongsTrain)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

cor(SongsTrain$loudness, SongsTrain$energy)

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

predictTest = predict(SongsLog3, newdata = SongsTest, type="response")
table(SongsTest$Top10, predictTest >= 0.45)

table(SongsTest$Top10)

