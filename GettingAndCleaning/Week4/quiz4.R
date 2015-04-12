setwd("~/DataScience/datasciencecoursera/GettingAndCleaning/Week4")
?Sys.timezone
# Question 1 - Quiz 4
download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", destfile = "q1.csv", method = "curl")
read.csv(file = "q1.csv")
q1df <- read.csv(file = "q1.csv")
View(`q1df`)
names(q1df)
splitted <- strsplit(x = names(q1df), split = "wgtp")
splitted[123]
# Question 2 - Quiz 4
download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", destfile = "q2.csv", method = "curl")
q2df <- read.csv(file = "q2.csv", skip = 4)
View(`q2df`)
q2df$X.4
class(q2df$X.4)
q2values <- gsub(pattern = ",", replacement = "", x = q2df$X.4)
q2values
library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
q2values <- str_trim(q2values)
q2values
?grepl
q2values2 <- grep(pattern = "[0-9]+", x = q2values, value = TRUE)
mean(as.numeric(q2values[1:190]))
# Question 3 - Quiz 4
grep("^United",q2df$X.3)
# Question 4 - Quiz 4
download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv", destfile = "f3.csv", method = "curl")
read.csv(file = "f3.csv")
q4df <- read.csv(file = "f3.csv")
View(`q4df`)
q2df[1:190,]
q2df2 <- q2df[1:190,]
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
joint <- inner_join(q2df2, q4df, by = c("X" = "CountryCode")) # X belongs to q2df2, CountryCode to q4df
names(joint)
View(joint)
grep(pattern = "fiscal", ignore.case = TRUE, x = joint) # WRONG! Specify where to grep.
grep(pattern = "fiscal", ignore.case = TRUE, x = joint$Special.Notes)
grep(pattern = "fiscal(.*)june", ignore.case = TRUE, x = joint$Special.Notes)
length(grep(pattern = "fiscal(.*)june", ignore.case = TRUE, x = joint$Special.Notes))
# Question 5 - Quiz 4
install.packages("quantmod")
library("quantmod", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
amzn = getSymbols("AMZN",auto.assign=FALSE)
amzn
str(amzn)
sampleTimes = index(amzn)
sampleTimes
library("lubridate", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
samples <- ymd(sampleTimes)
samples
year(samples)
year(samples) == 2012
length(year(samples)[year(samples) == 2012])
length(year(samples)[year(samples) == 2012 & wday(samples) == 2]) # Lubridate!
amzn
str(amzn)
class(amzn)
View(amzn)
