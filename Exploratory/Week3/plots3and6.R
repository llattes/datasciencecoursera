# Plot 6
# ------
## PA 2: Exploratory Data Analysis
## Plot 6
## 
## Libraries needed: 
library(ggplot2)

## Set working directory
setwd("D:\\Data Science Specialization\\Exploratory Data Analysis\\Course Project")

## Step 1: read in the data
## This first line will likely take a few seconds. Be patient!
if(!exists("NEI")){
  NEI <- readRDS("./data/summarySCC_PM25.rds")
}
if(!exists("SCC")){
  SCC <- readRDS("./data/Source_Classification_Code.rds")
}

## Q6: Compare emissions from motor vehicle sources in Baltimore City with emissions 
## from motor vehicle sources in Los Angeles County, California (fips == "06037").
## Which city has seen greater changes over time in motor vehicle emissions?
## Baltimore City, Maryland (fips == "24510"), 
## Los Angeles County, California (fips == "06037")

## Step 2: Searching for ON-ROAD type in NEI
## Searching for 'motor' in SCC only gave a subset (non-cars)
mvbalaPM25NEI <- NEI[(NEI$fips=="24510"|NEI$fips=="06037") & NEI$type=="ON-ROAD", ]
length(mvbalaPM25NEI) # 6

## Step 3: Searching for motor Vehicles type in SCC
mvsrcSCC <- unique(grep("Vehicles", SCC$EI.Sector, ignore.case = TRUE, value = TRUE))
mvsrcSCC1 <- SCC[SCC$EI.Sector %in% mvsrcSCC, ]["SCC"]

## Subset the motor vehicles from NEI for Baltimore, MD and Los Angeles County, CA
mvbalaPM25NEISCC <- NEI[NEI$SCC %in% mvsrcSCC1$SCC & 
    (NEI$fips == "24510"|NEI$fips == "06037"),]
length(mvbalaPM25NEISCC) # 6

## Step 4: Comparision of the two search "ON-ROAD" and "Vehicles" to ensure 
## that we captured the correct data
all.equal(mvbalaPM25NEI, mvbalaPM25NEISCC, tolerance = 0) 

## Step 5: Find the emissions due to motor vehicles in Baltimore city 
## and Los Angeles County using the search subset for "ON-ROAD" 
## type (mvbalaPM25NEI - Obtained in above Step 2)
mvbacatotalPM25YrFips <- aggregate(Emissions ~ year + fips, mvbalaPM25NEI, sum)
mvbacatotalPM25YrFips$fips[mvbacatotalPM25YrFips$fips=="24510"] <- "Baltimore, MD"
mvbacatotalPM25YrFips$fips[mvbacatotalPM25YrFips$fips=="06037"] <- "Los Angeles, CA"

## Step 6: prepare to plot to png
png("plot6.png", width=840, height=480)
gbaLA <- ggplot(mvbacatotalPM25YrFips, aes(factor(year), Emissions))
gbaLA <- gbaLA + facet_grid(. ~ fips)
gbaLA <- gbaLA + geom_bar(stat="identity") +
  xlab("Year") +
  ylab(expression("Total PM"[2.5]*" Emissions (tons)")) +
  ggtitle(expression("Baltimore City, MD vs Los Angeles County, CA PM"[2.5]*
      " Motor Vehicle Emission 1999-2008"))
print(gbaLA)
dev.off()

# Plot 3
# ------
## PA 2: Exploratory Data Analysis
## Plot 3
## 
## Libraries needed: 
library(ggplot2)

## Set working directory
setwd("D:\\Data Science Specialization\\Exploratory Data Analysis\\Course Project")

## Step 1: read in the data
## This first line will likely take a few seconds. Be patient!
if(!exists("NEI")){
  NEI <- readRDS("./data/summarySCC_PM25.rds")
}
if(!exists("SCC")){
  SCC <- readRDS("./data/Source_Classification_Code.rds")
}

## Q3: Of the four types of sources indicated by the type (point, nonpoint, 
## onroad, nonroad) variable, which of these four sources have seen decreases 
## in emissions from 1999-2008 for Baltimore City? Which have seen increases 
## in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot 
## answer this question. Baltimore City, Maryland (fips == "24510")

## Step 2: obtain the subsets to plot
baltimore <- NEI[NEI$fips=="24510", ]
totalPM25byYearType <- aggregate(Emissions ~ year + type, baltimore, sum)

## Step 3: prepare to plot to png
png("plot3.png", width=640, height=480)
g <- ggplot(totalPM25byYearType, aes(year, Emissions, color = type))
g <- g + geom_line() + xlab("Year") + 
  ylab(expression("Total PM"[2.5]*" Emissions (tons)")) +
  ggtitle(expression("Baltimore City PM"[2.5]*" Emission by Source and Year"))
print(g)
dev.off()

# Another Plot 3
# --------------
library(dplyr)
library(ggplot2)

# Read data
dfx <- readRDS("summarySCC_PM25.rds")

# Filter for Baltimore City county
dfx.bc <- filter(dfx, fips == "24510")

# Calculate emissions by year and type for filtered data
dfx.totals <- summarize(
  group_by(dfx.bc, year, type),
  Total.Emissions = sum(Emissions)
)

# Plot the emissions by year for each type
png("plot3.png", width=768, height=480)
p <- ggplot(dfx.totals, aes(x=year, y=Total.Emissions, group = type)) +
  geom_line(aes(color=type)) +
  geom_point() +
  labs(title = "Total PM2.5 Emissions in Baltimore City by Type") +
  labs(x = "Year") +
  labs(y = "Total Emissions (in tons)")
print(p)
dev.off()

# Another Plot 3
# --------------
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
NEI<-NEI[NEI$fips=="24510",]
df<-aggregate(NEI$Emissions,list(year=NEI$year,type=NEI$type),sum)
ggplot(data=df, aes(x=year, y=x, group=type, colour=type)) + 
  geom_line() + geom_point() + xlab("Year") + ylab("Emissions, tons") + 
  ggtitle("PM2.5 Emissions, Baltimore City, MD, 1999-2008") +
  scale_colour_discrete(name="Type of Source")
ggsave(filename="plot3.png",width=6,height=6)
