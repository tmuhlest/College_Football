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

# RAW DATA
#         School             Head Coach Season Head_Coach_Wins School_Wins
#        Alabama            Bear Bryant   1961               5          11
#        Alabama            Bear Bryant   1965               5          11
#        Alabama            Bear Bryant   1978               5          11
#        Alabama            Bear Bryant   1964               5          11
#        Alabama             Nick Saban   2012               6          11
#        Alabama         Gene Stallings   1992               1          11
#        Alabama             Nick Saban   2017               6          11
#        Alabama             Nick Saban   2009               6          11
#        Alabama            Bear Bryant   1979               5          11
#        Alabama             Nick Saban   2011               6          11
#        Alabama             Nick Saban   2015               6          11
#           Army             Earl Blaik   1945               2           2
#           Army             Earl Blaik   1944               2           2
#         Auburn            Gene Chizik   2010               1           2
#         Auburn           Ralph Jordan   1957               1           2
#            BYU         LaVell Edwards   1984               1           1
#        Clemson           Dabo Swinney   2018               2           3
#        Clemson           Dabo Swinney   2016               2           3
#        Clemson             Danny Ford   1981               1           3
#       Colorado         Bill McCartney   1990               1           1
#        Florida            Urban Meyer   2008               3           3
#        Florida            Urban Meyer   2006               3           3
#        Florida         Steve Spurrier   1996               1           3
#  Florida State           Jimbo Fisher   2013               1           3
#  Florida State           Bobby Bowden   1993               2           3
#  Florida State           Bobby Bowden   1999               2           3
#        Georgia           Vince Dooley   1980               1           1
#            LSU              Les Miles   2007               1           4
#            LSU             Ed Orgeron   2019               1           4
#            LSU             Nick Saban   2003               6           4
#            LSU           Paul Dietzel   1958               1           4
#       Maryland              Jim Tatum   1953               1           1
#     Miami (FL)        Dennis Erickson   1991               2           5
#     Miami (FL)        Dennis Erickson   1989               2           5
#     Miami (FL) Howard Schnellenberger   1983               1           5
#     Miami (FL)            Larry Coker   2001               1           5
#     Miami (FL)          Jimmy Johnson   1987               1           5
#       Michigan             Lloyd Carr   1997               1           2
#       Michigan      Bennie Oosterbaan   1948               1           2
# Michigan State            Biggie Munn   1952               1           1
#      Minnesota         Bernie Bierman   1936               3           4
#      Minnesota         Bernie Bierman   1941               3           4
#      Minnesota         Murray Warmath   1960               1           4
#      Minnesota         Bernie Bierman   1940               3           4
#       Nebraska            Bob Devaney   1971               2           4
#       Nebraska            Tom Osborne   1995               2           4
#       Nebraska            Bob Devaney   1970               2           4
#       Nebraska            Tom Osborne   1994               2           4
#     Notre Dame         Ara Parseghian   1966               2           8
#     Notre Dame         Ara Parseghian   1973               2           8
#     Notre Dame             Dan Devine   1977               1           8
#     Notre Dame            Frank Leahy   1943               4           8
#     Notre Dame              Lou Holtz   1988               1           8
#     Notre Dame            Frank Leahy   1946               4           8
#     Notre Dame            Frank Leahy   1947               4           8
#     Notre Dame            Frank Leahy   1949               4           8
#     Ohio State            Urban Meyer   2014               3           5
#     Ohio State            Jim Tressel   2002               1           5
#     Ohio State            Woody Hayes   1968               2           5
#     Ohio State             Paul Brown   1942               1           5
#     Ohio State            Woody Hayes   1954               2           5
#       Oklahoma          Barry Switzer   1985               3           7
#       Oklahoma          Bud Wilkinson   1950               3           7
#       Oklahoma             Bob Stoops   2000               1           7
#       Oklahoma          Barry Switzer   1974               3           7
#       Oklahoma          Barry Switzer   1975               3           7
#       Oklahoma          Bud Wilkinson   1955               3           7
#       Oklahoma          Bud Wilkinson   1956               3           7
#     Penn State            Joe Paterno   1982               2           2
#     Penn State            Joe Paterno   1986               2           2
#     Pittsburgh          Johnny Majors   1976               1           2
#     Pittsburgh        Jock Sutherland   1937               1           2
#       Syracuse     Ben Schwartzwalder   1959               1           1
#            TCU            Dutch Meyer   1938               1           1
#      Tennessee         Phillip Fulmer   1998               1           2
#      Tennessee         Robert Neyland   1951               1           2
#          Texas             Mack Brown   2005               1           3
#          Texas          Darrell Royal   1969               2           3
#          Texas          Darrell Royal   1963               2           3
#      Texas A&M           Homer Norton   1939               1           1
#            USC             John McKay   1972               3           4
#            USC           Pete Carroll   2004               1           4
#            USC             John McKay   1962               3           4
#            USC             John McKay   1967               3           4

