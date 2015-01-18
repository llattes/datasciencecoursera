require("ggplot2")
source('~/DataScience/datasciencecoursera/GettingAndCleaning/Week2/olescrapping.R')
england <- scrapPositions()
england <- transform(england, Pos = as.numeric(Pos))
england <- transform(england, Goals.for = as.numeric(Goals.for))
england <- transform(england, Goals.against = as.numeric(Goals.against))
england <- transform(england, Won = as.numeric(Won))
# Generate quartiles for the number of matches won.
wonQuartiles <- quantile(england$Won, seq(0, 1, length = 5), na.rm = TRUE)
england$wonQuartiles <- cut(england$Won, wonQuartiles, include.lowest = TRUE)
# Make a plot using ggplot function faceted by matches won.
g <- ggplot(england, aes(Pos, Goals.for))
g + geom_point(alpha = 1/3) + geom_smooth(method="lm", se=TRUE, col="steelblue") + theme_bw(base_size = 12) + labs(x = "Position") + labs(y = "Goals for") + labs(title = "English PL Standings") + facet_grid(. ~ wonQuartiles)
