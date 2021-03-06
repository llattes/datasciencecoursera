plot2 <- function() {
  require("RColorBrewer")
  NEI <- readRDS("summarySCC_PM25.rds")
  s1 <- subset(x = NEI, subset = NEI$year == 1999 & NEI$fips == "24510")
  s2 <- subset(x = NEI, subset = NEI$year == 2002 & NEI$fips == "24510")
  s3 <- subset(x = NEI, subset = NEI$year == 2005 & NEI$fips == "24510")
  s4 <- subset(x = NEI, subset = NEI$year == 2008 & NEI$fips == "24510")
  emissions_by_year = data.frame(c("1999", "2002", "2005", "2008"),
    c(sum(s1$Emissions), sum(s2$Emissions), sum(s3$Emissions),
      sum(s4$Emissions)), stringsAsFactors = FALSE)
  colnames(emissions_by_year) <- c("Year", "Total_Emissions")
  palette <- brewer.pal(4, "Blues")
  png(filename = paste("plot2.png", sep = ""), width = 800, height = 600,
    units = "px")
  barplot(emissions_by_year$Total_Emissions, names.arg = emissions_by_year$Year,
    col = palette, main = "PM2.5 emissions by year in Baltimore City",
    xlab="Year", ylab="Total Emissions (in tons.)")
  dev.off()
}
