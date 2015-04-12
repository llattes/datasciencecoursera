plot1 <- function() {
  require("RColorBrewer")
  NEI <- readRDS("summarySCC_PM25.rds")
  s1 <- subset(x = NEI, subset = NEI$year == 1999)
  s2 <- subset(x = NEI, subset = NEI$year == 2002)
  s3 <- subset(x = NEI, subset = NEI$year == 2005)
  s4 <- subset(x = NEI, subset = NEI$year == 2008)
  emissions_by_year = data.frame(c("1999", "2002", "2005", "2008"),
    c(sum(s1$Emissions), sum(s2$Emissions), sum(s3$Emissions),
      sum(s4$Emissions)), stringsAsFactors = FALSE)
  colnames(emissions_by_year) <- c("Year", "Total_Emissions")
  palette <- brewer.pal(4, "Blues")
  png(filename = paste("plot1.png", sep = ""), width = 800, height = 600,
    units = "px")
  barplot(emissions_by_year$Total_Emissions, names.arg = emissions_by_year$Year,
    col = palette, main = "PM2.5 emissions by year in the US",
    xlab="Year", ylab="Total Emissions (in tons.)")
  dev.off()
}
