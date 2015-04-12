plot4 <- function() {
  require("RColorBrewer")
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  # Obtain coal combustion-related data.
  coal <- SCC[grep("Coal", ignore.case = TRUE, x = SCC$Short.Name), ]
  s1 <- subset(x = NEI, subset = NEI$year == 1999 & NEI$SCC %in% coal$SCC)
  s2 <- subset(x = NEI, subset = NEI$year == 2002 & NEI$SCC %in% coal$SCC)
  s3 <- subset(x = NEI, subset = NEI$year == 2005 & NEI$SCC %in% coal$SCC)
  s4 <- subset(x = NEI, subset = NEI$year == 2008 & NEI$SCC %in% coal$SCC)
  emissions_by_year = data.frame(c("1999", "2002", "2005", "2008"),
    c(sum(s1$Emissions), sum(s2$Emissions), sum(s3$Emissions),
      sum(s4$Emissions)), stringsAsFactors = FALSE)
  colnames(emissions_by_year) <- c("Year", "Total_Emissions")
  palette <- brewer.pal(4, "Blues")
  png(filename = paste("plot4.png", sep = ""), width = 800, height = 600,
    units = "px")
  barplot(emissions_by_year$Total_Emissions, names.arg = emissions_by_year$Year,
    col = palette, main = "PM2.5 emissions from coal combustion-related sources by year in the US",
    xlab="Year", ylab="Total Emissions (in tons.)")
  dev.off()
}
