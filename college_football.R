library(htmltab)
library(tidyverse)

df <- htmltab("https://en.wikipedia.org/wiki/AP_National_Championship_Trophy",3)
df$School[68] <- "LSU"
df$`Head Coach`[68] <- "Nick Saban"
wins_by_coach <- aggregate(df$School ~ df$`Head Coach` , FUN = length)
names(wins_by_coach) <- c("Head Coach", "Head_Coach_Wins")
wins_by_school <- aggregate(df$`Head Coach` ~ df$School , FUN = length)
names(wins_by_school) <- c("School", "School_Wins")

df <- merge(df, wins_by_coach, by = "Head Coach", all.x = TRUE)
df <- merge(df, wins_by_school, by = "School", all.x = TRUE)

hist(wins_by_school$School_Wins)
hist(wins_by_coach$Head_Coach_Wins)
sd(c(1,2,3))


summary(aov(df$School_Wins ~ df$`Head Coach`))
TukeyHSD(aov(df$School_Wins ~ df$`Head Coach`))

df_no_single_winners <- df %>% filter(School_Wins > 5)
summary(aov(df_no_single_winners$School_Wins ~ df_no_single_winners$School))
TukeyHSD(aov(df_no_single_winners$School_Wins ~ df_no_single_winners$School))


summary(aov(df$Head_Coach_Wins ~ df$`Head Coach`))
TukeyHSD(aov(df$Head_Coach_Wins ~ df$`Head Coach`))
