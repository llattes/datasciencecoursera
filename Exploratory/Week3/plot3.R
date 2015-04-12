plot3 <- function() {
  require("ggplot2")
  NEI <- readRDS("summarySCC_PM25.rds")
  s1p <- subset(x = NEI, subset = NEI$year == 1999 & NEI$fips == "24510" & NEI$type == "POINT")
  s1n <- subset(x = NEI, subset = NEI$year == 1999 & NEI$fips == "24510" & NEI$type == "NONPOINT")
  s1o <- subset(x = NEI, subset = NEI$year == 1999 & NEI$fips == "24510" & NEI$type == "ON-ROAD")
  s1nr <- subset(x = NEI, subset = NEI$year == 1999 & NEI$fips == "24510" & NEI$type == "NON-ROAD")
  s2p <- subset(x = NEI, subset = NEI$year == 2002 & NEI$fips == "24510" & NEI$type == "POINT")
  s2n <- subset(x = NEI, subset = NEI$year == 2002 & NEI$fips == "24510" & NEI$type == "NONPOINT")
  s2o <- subset(x = NEI, subset = NEI$year == 2002 & NEI$fips == "24510" & NEI$type == "ON-ROAD")
  s2nr <- subset(x = NEI, subset = NEI$year == 2002 & NEI$fips == "24510" & NEI$type == "NON-ROAD")
  s3p <- subset(x = NEI, subset = NEI$year == 2005 & NEI$fips == "24510" & NEI$type == "POINT")
  s3n <- subset(x = NEI, subset = NEI$year == 2005 & NEI$fips == "24510" & NEI$type == "NONPOINT")
  s3o <- subset(x = NEI, subset = NEI$year == 2005 & NEI$fips == "24510" & NEI$type == "ON-ROAD")
  s3nr <- subset(x = NEI, subset = NEI$year == 2005 & NEI$fips == "24510" & NEI$type == "NON-ROAD")
  s4p <- subset(x = NEI, subset = NEI$year == 2008 & NEI$fips == "24510" & NEI$type == "POINT")
  s4n <- subset(x = NEI, subset = NEI$year == 2008 & NEI$fips == "24510" & NEI$type == "NONPOINT")
  s4o <- subset(x = NEI, subset = NEI$year == 2008 & NEI$fips == "24510" & NEI$type == "ON-ROAD")
  s4nr <- subset(x = NEI, subset = NEI$year == 2008 & NEI$fips == "24510" & NEI$type == "NON-ROAD")
  emissions_by_year_p = data.frame(c("1999", "2002", "2005", "2008"),
    c(sum(s1p$Emissions), sum(s2p$Emissions), sum(s3p$Emissions),
      sum(s4p$Emissions)), stringsAsFactors = FALSE)
  emissions_by_year_n = data.frame(c("1999", "2002", "2005", "2008"),
    c(sum(s1n$Emissions), sum(s2n$Emissions), sum(s3n$Emissions),
      sum(s4n$Emissions)), stringsAsFactors = FALSE)
  emissions_by_year_o = data.frame(c("1999", "2002", "2005", "2008"),
    c(sum(s1o$Emissions), sum(s2o$Emissions), sum(s3o$Emissions),
      sum(s4o$Emissions)), stringsAsFactors = FALSE)
  emissions_by_year_nr = data.frame(c("1999", "2002", "2005", "2008"),
    c(sum(s1nr$Emissions), sum(s2nr$Emissions), sum(s3nr$Emissions),
      sum(s4nr$Emissions)), stringsAsFactors = FALSE)
  colnames(emissions_by_year_p) <- c("Year", "Total_Emissions")
  colnames(emissions_by_year_n) <- c("Year", "Total_Emissions")
  colnames(emissions_by_year_o) <- c("Year", "Total_Emissions")
  colnames(emissions_by_year_nr) <- c("Year", "Total_Emissions")
  all <- rbind(emissions_by_year_p, emissions_by_year_n, emissions_by_year_o, emissions_by_year_nr)
  all$Type <- factor(c(rep("POINT", times = 4), rep("NONPOINT", times = 4), rep("ON-ROAD", times = 4), rep("NON-ROAD", times = 4)))
  png(filename = paste("plot3.png", sep = ""), width = 800, height = 600,
    units = "px")
  plot3 <- ggplot(all, aes(Year, Total_Emissions)) + geom_point(size = 4) + labs(x = "Year") + labs(y = "Emissions") + labs(title = "Total emissions by type in Baltimore City") + facet_grid(. ~ Type)
  print(plot3)
  dev.off()
}
