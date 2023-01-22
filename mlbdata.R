myDF = read.csv('./mlb_elo_latest.csv')
head(myDF)
myDF[(myDF$team2 == 'CHW') | (myDF$team1 == 'CHW'), ]




mlb = read.csv('stats (2).csv')
head(mlb)
new_mlb = mlb[, -c(4, 20)] #Removes a rogue X column with all NA values,as well as year because we know this is all 2021 data.
head(new_mlb)
new_mlb[,'player_id'] = seq(1, length(new_mlb$player_id))
mlb = new_mlb
?detach
attach(mlb)
#Summary of all statistics
summary(mlb[,-c(1,2,3)]) #ignore player first + last name and player id
summary(mlb$barrel_batted_rate)
#Age
plot(table(mlb$player_age), ylab='count', xlab='Player Age', col='blue', main='MLB Player Age Distribution')
#The majority of the players in the dataset fall between 23 years old and 37 years old, following somewhat of a right skewed normal curve which is
#really interesting. 


#Batting Average, On-Base Percentage, and Slugging Percentage

boxplot(mlb$batting_avg, mlb$on_base_percent, mlb$slg_percent, col = c(3, 5, 7), las=1, names = c('BA', 'OBP', 'SLG'), main='Traditional Stat Distributions', ylab='% Avg')
#Batting average and on base percentage seem very condensed compared to slugging percentage, so much more variety there. 
#A couple high outliers for on-base and slugging percentage, must be really good players.
mlb[mlb$on_base_percent == max(mlb$on_base_percent), ]
#Juan Soto finished 2nd in MVP voting, so yes, he is a very good player.

mlb[mlb$slg_percent == max(mlb$slg_percent), ]
#Byron Buxton didn't receive MVP consideration because he barely made the cut of 250 plate appearances and played in just 61/162 games,
#but any OPS over 1.000 is elite-of-the-elite, and if he continued his 1.005 OPS for the whole season he would surely be considered for MVP.


#K% & BB%
library(ggplot2)
ggplot(data=mlb) + geom_point(aes(x = b_k_percent, y=on_base_percent, col='yellow')) + geom_point(aes(x = b_k_percent, y=slg_percent))
cor(slg_percent, b_k_percent)
#Just because players strike out more than others doesn't mean they're always bombing the ball on contact
cor(on_base_percent, b_k_percent)
#Even with players taking less walks, there's still a fairly weak negative correlation between on-base percentage and striking out.
cor(batting_avg, b_k_percent)
#When we don't account for walks, the negative correlation jumps to the moderate level, so are people striking out a lot also taking lots of walks?
plot(b_k_percent, b_bb_percent)
cor(b_k_percent, b_bb_percent)
#There is a positive correlation, although fairly weak, meaning as K% increases, BB% also increases. 
ggplot(data=mlb) + geom_point(aes(x = b_bb_percent, y=on_base_percent, color = 'blue')) 


head(mlb)
#Exit Velocity, Launch Angle, Sweet Spot, and Hard Hit
summary(mlb[,13:17])
boxplot(sweet_spot_percent, barrel_batted_rate, hard_hit_percent, col=c(3, 5, 15), names=c('SS','BR','HH'), ylab='Percentage (%)', xlab='Variable Initials', main='% Distribution of Batting Data', las=1)

plot(exit_velocity_avg, hard_hit_percent)
qqplot(exit_velocity_avg, hard_hit_percent)
plot(batting_avg~sweet_spot_percent)






















plot(mlb$b_home_run~mlb$barrel_batted_rate)
plot(mlb$batting_avg~mlb$sweet_spot_percent)
plot(mlb$slg_percent~mlb$exit_velocity_avg)

plot(mlb$batting_avg~mlb$z_swing_miss_percent)

max(mlb$on_base_percent)



?tapply
?subset
mlb_common_ages = subset(mlb, player_age != 20 & player_age != 39 & player_age != 41)
table(mlb_common_ages$player_age)
tapply(mlb$on_base_plus_slg, mlb$player_age, FUN=count)
anova==(mlb_common_ages$on_base_plus_slg~as.factor(mlb_common_ages$player_age))
?anova




#Boxplot approach
#One quick way to eyeball whether one group may have a significantly different average OPS than others is to look at a boxplot.
boxplot(mlb_common_ages$on_base_plus_slg~as.factor(mlb_common_ages$player_age))
#From the boxplot, it looks like 22 year olds have a significantly higher mean OPS than any other age group. Other than 22 year olds, 
#the rest of the age groups have quite similar OPS distributions, so looking at this boxplot I would say my hypothesis that players average
#a higher OPS in their prime is false.


#Anova Approach
#FIRST: Check assumptions
#Values come from a normal distribution
#equal variances
#
#H0: There is no change in OPS based on the hitter's age
#H1: At least one age group has a mean OPS significantly different from the others.
qqnorm(mlb_common_ages$on_base_plus_slg)
summary(aov(mlb_common_ages$on_base_plus_slg~as.factor(mlb_common_ages$player_age)))

pairwise.t.test(mlb_common_ages$on_base_plus_slg, as.factor(mlb_common_ages$player_age), p.adj='bonferroni', pool.sd=TRUE)


#
head(mlb)
?t.test

last20yearsrbis = read.csv('stats (6).csv')
#NOTE: This does not include the 2020 season as it was shortened to 60 games.
#SECOND NOTE: These are only the last 20 years, which is not entire population of baseball players in history, but it is much closer to the population mean than one season.
mean(last20yearsrbis$b_rbi) #57.887


#H0 The population mean amount of RBIs 
t.test(mlb$b_rbi, alternative="two.sided", conf.level=.95)
#We can say with 95% confidence that the population mean, if these new balls were used for the history of baseball, falls between 53.81 and 59.175. Since the mean over the 
#last 20 years, 57.877, falls right within that interval, we cannot reject H0, and so we cannot prove that the deadened balls this year had a negative impact on RBIs.

summary(mlb$b_home_run)
#Which advanced predictors can help us predict how many home runs a player will hit
plot(mlb$b_home_run~mlb$barrel_batted_rate)
plot(mlb$b_home_run~mlb$sweet_spot_percent)
plot(b_home_run~exit_velocity_avg)
plot(b_home_run~hard_hit_percent)
plot(b_home_run~launch_angle_avg)
HRmodel = lm(formula=b_home_run~barrel_batted_rate+exit_velocity_avg+hard_hit_percent+launch_angle_avg+b_k_percent+b_k_percent*b_bb_percent, data=mlb)
HRmodel = step(HRmodel)
summary(HRmodel)
#The backwards step method removed sweet spot percentage and hard hit percentage as predictors, leaving us with the formula:
#ASSUMPTIONS: Independence, Normality, Linearity, Equal Variance
#HR = -48.166 + 1.500(Barrel %) + .53775(Exit Velocity Average) +.299(Launch_Angle_Avg).
#The adjusted R^2 value is .60, which is alright but we've only accounted for 60% of the variance in home runs so our predictions may not too accurate.
#The qqnorm plot is pretty straight, so the home run values seem to be normally distributed
qqnorm(mlb$b_home_run)
mlb[mlb$last_name == 'Abreu', ] #Jose Abreu, a player for the Chicago White Sox, had 30 home runs in 2021. Let's see how well we can predict his home runs using our model.
predict(HRmodel, data.frame(barrel_batted_rate=10.2, launch_angle_avg=10.4, exit_velocity_avg=92), interval='predict', level=.95)
#The fitted home run value is around 20 home runs, with a prediction interval between 7.01 and 32.43. It must've been a very overachieving season for Jose or our model isn't that great.

mlb[mlb$last_name == 'Moncada', ]
predict(HRmodel, data.frame(barrel_batted_rate=8.2, launch_angle_avg=11.3, exit_velocity_avg=90.1), interval='predict', level=.95)
#The fitted value is about 16 home runs, very close to his actual 14 home runs. This time the model overpredicted his home runs, meaning he didn't hit as many as the stats show.

mlb[mlb$b_home_run == max(mlb$b_home_run), ]
predict(HRmodel, data.frame(barrel_batted_rate=15.1, launch_angle_avg=9.4, exit_velocity_avg=95.1), interval='predict', level=.95)
#For Vladimir Guerrero Jr, who tied for the most home runs in the league at 48, is only being predicted at 28.5 home runs. His total of 48 goes way above his 95% prediction interval max of 41,
#so according to the model his season was an outlier and should see regression next year. 

