# Connect to a local MySQL DB using RMySQL.
localdb <- dbConnect(MySQL(), user="root", host="localhost", password="xxx", db="world")
dbListTables(localdb)
dbGetQuery(localdb, "select count(*) from Country;")
query <- dbSendQuery(localdb, "select * from Country;")
country <- fetch(query)
str(country)
dbClearResult(query)
query <- dbSendQuery(localdb, "select * from CountryLanguage;")
lang2 <- dbFetch(query, n = 1000)
dbClearResult(query)
dbDisconnect(localdb)
