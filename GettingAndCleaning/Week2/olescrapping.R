## Removes leading & trailing whitespace from strings.
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

## Removes \r and \n characters from strings.
cleanup <- function (x) gsub("[\r\n]", "", x)

## Creates a table given a Olé statistics url
scrapPositions <- function (url = "http://www.ole.com.ar/estadisticas/futbol/premier-league.html") {
  require("httr")
  require("XML")
  html2 <- GET(url)
  content <- content(html2, as = "text")
  parsedHTML <- htmlParse(content, asText = TRUE)
  pos <- xpathSApply(parsedHTML, "//td[@class='pos w_50']", xmlValue)
  nom <- xpathSApply(parsedHTML, "//div[@class='nombre1']", xmlValue)
  nom <- nom[1:20]
  pj <- xpathSApply(parsedHTML, "//td[@class='pj w_50']", xmlValue)
  pg <- xpathSApply(parsedHTML, "//td[@class='pg w_50']", xmlValue)
  pe <- xpathSApply(parsedHTML, "//td[@class='pe w_50']", xmlValue)
  pp <- xpathSApply(parsedHTML, "//td[@class='pp w_50']", xmlValue)
  gf <- xpathSApply(parsedHTML, "//td[@class='gf w_50']", xmlValue)
  gc <- xpathSApply(parsedHTML, "//td[@class='gc w_50']", xmlValue)
  pts <- xpathSApply(parsedHTML, "//td[@class='pts w_50']", xmlValue)
  pts <- xpathSApply(parsedHTML, "//td[@class='pts w_50 dark']", xmlValue)
  data.frame(pos, nom, pj, pg, pe, pp, gf, gc, pts, stringsAsFactors = FALSE)
  positions <- data.frame(pos, nom, pj, pg, pe, pp, gf, gc, pts, stringsAsFactors = FALSE)
  positions$nom <- cleanup(positions$nom)
  positions$nom <- trim(positions$nom)
  colnames(positions) <- c("Pos", "Team", "Played", "Won", "Tied", "Lost", "Goals for", "Goals against", "Points")
  positions
}

plotPositions <- function (positions, filename = "positions") {
  png(filename = paste(filename, ".png", sep = ""), width = 800, height = 600,
      units = "px")
  with(positions, plot(Pos, y = Points, main = "Standings", type = "n",
                       pch = 20, xlab = "Position", ylab = "Points"))
  with(positions, lines(x = Pos[1], y = Points[1], type = "o", pch = 20,
                        col = "red"))
  with(positions, text(Points[1], labels = Team[1], cex = 1, pos = 4,
                       col = "red"))
  with(positions, lines(x = Pos[1:20], y = Points[1:20], type = "o", pch = 21,
                        col = "black"))
  with(positions, text(Points[2:20], labels = Team[2:20], cex = 0.65, pos = 4,
                       col = "black"))
  dev.off()
}
