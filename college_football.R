library(htmltab)
library(tidyverse)

# pull code from wikipedia table
df <- htmltab("https://en.wikipedia.org/wiki/AP_National_Championship_Trophy",3)

# USC technically was the AP number 1 at the end of the season, but they didn't play in the championship
# game so they are being replaced.

# Fight on though.
df$School[68] <- "LSU"
df$`Head Coach`[68] <- "Nick Saban"

# Calculate how many wins each coach had
wins_by_coach <- aggregate(df$School ~ df$`Head Coach` , FUN = length)
names(wins_by_coach) <- c("Head Coach", "Head_Coach_Wins")

# Calculate how many wins each school had
wins_by_school <- aggregate(df$`Head Coach` ~ df$School , FUN = length)
names(wins_by_school) <- c("School", "School_Wins")

# Merge it all into one
df <- merge(df, wins_by_coach, by = "Head Coach", all.x = TRUE)
df <- merge(df, wins_by_school, by = "School", all.x = TRUE)


# Rough histograms for the visually curious
hist(wins_by_school$School_Wins, main = "School Wins", xlab = "Championships", col = "dodgerblue")
hist(wins_by_coach$Head_Coach_Wins, main = "Coach wins", xlab = "Championships", col = "maroon")

# Signifcance of School 
summary(aov(df$School_Wins ~ df$`Head Coach`))
#TukeyHSD(aov(df$School_Wins ~ df$`Head Coach`))

# Significance of Coach
summary(aov(df$Head_Coach_Wins ~ df$`Head Coach`))
#TukeyHSD(aov(df$Head_Coach_Wins ~ df$`Head Coach`))
