# Extract data from Twitter API using OAuth 1.0.
twitterApp <- oauth_app("LLConsumingFromR", key = "xxx", secret = "xxx")
sign <- sign_oauth1.0(twitterApp, token = "xxx", token_secret = "xxx")
sign
RbloggersTL <- GET("https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=Rbloggers&count=15", sign)
RbloggersTL
RbloggersTLContent <- content(RbloggersTL)
# jsonlite package for JSON data manipulation.
install.packages("jsonlite")
library("jsonlite", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
RbloggersTLConvertedContent <- jsonlite::fromJSON(toJSON(RbloggersTLContent))
class(RbloggersTLConvertedContent)
colnames(RbloggersTLConvertedContent)
head(RbloggersTLConvertedContent$text)
