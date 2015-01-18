# Library for auto-authenticating in GitHub.
library("httpuv", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/rate_limit", gtoken)
stop_for_status(req)
content(req)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)
content(req)
reqContent <- content(req)
convContent <- jsonlite::fromJSON(toJSON(reqContent))
View(convContent)
convContent$created_at
# sqldf package allows querying data frames SQL-like.
download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv", destfile = "data.csv", method = "curl")
install.packages("sqldf")
acs <- read.csv("data.csv")
library("sqldf", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
sqldf("select pwgtp1 from acs where AGEP < 50")
unique(acs$AGEP)
sqldf("select distinct pwgtp1 from acs")
sqldf("select distinct AGEP from acs")
# Parse HTML.
page <- GET(url = "http://biostat.jhsph.edu/~jleek/contact.html")
content <- content(page, as = "text")
parsed <- htmlParse(content, asText = TRUE)
parsed
class(parsed)
(parsed <- htmlParse(content, asText = TRUE))
out <- capture.output(parsed)
nchar(out[10])
nchar(out[20])
nchar(out[30])
nchar(out[100])
# capture.output seems to ignore whitelines.
content
con <- url ("http://biostat.jhsph.edu/~jleek/contact.html")
html <- readLines(con)
html[10]
lapply(c(html[10], html[20], html[30], html[100]), FUN = nchar)
# Read a fixed width file. Negative widths are lengths that should be ignored.
download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for", destfile = "data.for", method = "curl")
read.fwf(file = "data.for", skip = 4, widths = c(-1, 9, -5, 4, 4, -5, 4, 4, -5, 4, 4, -5, 4, 4))
fwfread <- read.fwf(file = "data.for", skip = 4, widths = c(-1, 9, -5, 4, 4, -5, 4, 4, -5, 4, 4, -5, 4, 4))
sum(fwfread$V4)
