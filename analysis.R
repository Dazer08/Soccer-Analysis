mydata = read.csv("team_data_2018_2019.csv")

#1
cor(mydata$wins, mydata$goals_scored)

m1 = lm(wins ~ goals_scored, data = mydata)
summary(m1)
plot(mydata$wins ~ mydata$goals_scored)
abline(m1)


#2
cor(mydata$goals_scored, mydata$firstGoalScoredPercentage_overall)
cor(mydata$goals_scored, mydata$shots)
cor(mydata$goals_scored, mydata$average_possession)
cor(mydata$wins, mydata$corners_total)

first_goal_scored = mydata$firstGoalScoredPercentage_overall
tail(sort(first_goal_scored),5)

mancity = subset(mydata,mydata$common_name == "Manchester City")
mancity$firstGoalScoredPercentage_overall
liverpool = subset(mydata,mydata$common_name == "Liverpool")
liverpool$firstGoalScoredPercentage_overall


goals = mydata$goals_scored
tail(sort(goals),5)
mancity$goals_scored
liverpool$goals_scored




#3
cor(mydata$average_possession, mydata$firstGoalScoredPercentage_overall)
cor(mydata$corners_total, mydata$firstGoalScoredPercentage_overall)
cor(mydata$shots, mydata$firstGoalScoredPercentage_overall)

cor(mydata$wins, mydata$shots)
cor(mydata$wins, mydata$corners_total)
cor(mydata$wins, mydata$firstGoalScoredPercentage_overall)

cor(mydata$goals_scored, mydata$shots)
cor(mydata$goals_scored, mydata$corners_total)

cor(mydata$firstGoalScoredPercentage_overall, mydata$corners_total)
cor(mydata$firstGoalScoredPercentage_overall, mydata$shots)


m2 = lm(firstGoalScoredPercentage_overall ~ corners_total, data = mydata)
summary(m2)
plot(mydata$firstGoalScoredPercentage_overall ~ mydata$corners_total)
abline(m2)

m4 = lm(firstGoalScoredPercentage_overall ~ shots, data = mydata)
summary(m4)
plot(mydata$firstGoalScoredPercentage_overall ~ mydata$shots)
abline(m4)

m3 = lm(firstGoalScoredPercentage_overall ~ average_possession, data = mydata)
summary(m3)
plot(mydata$firstGoalScoredPercentage_overall ~ mydata$average_possession)
abline(m3)


first_goal=mydata$firstGoalScoredPercentage_overall

arsenal = subset(mydata,mydata$common_name == "Arsenal")


tail(sort(goals),5) #top 5 goals scored
tail(sort(first_goal),5)
summary(mydata$firstGoalScoredPercentage_overall)
arsenal$firstGoalScoredPercentage_overall
summary(mydata$shots)
arsenal$shots
summary(mydata$average_possession)
arsenal$average_possession
summary(mydata$corners_total)
arsenal$corners_total



