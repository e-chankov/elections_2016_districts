require("httr")          # to html parsing (version 1.2.0)
require("XML")           # to work with xml nodes (version 3.98-1.4)
require("magrittr")

###########################################
# loading election results in 225 districts
###########################################

# Let x be a vector of integers 
# Here is a function to find percentages of x elements with 2 decimals in such a way that
# the sum of these percentages be equal to exactly 100.00 %
# This is a fragment of the roundfixS function from the package sfsmisc
# See also https://en.wikipedia.org/wiki/Largest_remainder_method
roundfixS <- function(x){
  x<- x/sum(x)*10000
  n <- length(x)
  x0 <- floor(x)
  e <- x - x0
  S. <- sum(e)
  stopifnot(all.equal(S., (S <- round(S.))))
  if (S > 0) {
    r <- numeric(n)
    r[sort.list(e, decreasing = TRUE)[1:S]] <- 1
    x <- x0 + r
  }
  x/100
}



# party titles in the order of the ballot 
parties.titles <- c("rodina", "communists",
                          "pensioners", "edinaya_rossia",
                          "greens", "civil_platform",
                          "LDPR", "PARNAS",
                          "ROST", "civil_power",
                          "yabloko", "CPRF",
                          "patriots", "sprav_rossia")
names(parties.titles) <- c("Родина", "Коммунисты России",
                                 "Партия пенсионеров",
                                 "Единая Россия","Зеленые",
                                 "Гражданская платформа",
                                 "ЛДПР", "ПАРНАС",
                                 "Партия роста", "Гражданская сила", 
                                 "Яблоко", "КПРФ",
                                 "Патриоты России",
                                 "Справедливая Россия")

source.web.page <- "http://www.vybory.izbirkom.ru/region/region/izbirkom?action=show&root=1&tvd=100100067795854&vrn=100100067795849&region=0&global=1&sub_region=0&prver=0&pronetvd=0&vibid=100100067795854&type=233" 
html.file <- GET(source.web.page) %>% 
  content(as="text", encoding = "Windows-1251") %>% 
  htmlTreeParse(useInternalNodes = TRUE, 
                encoding = "UTF-8")  

table.column.names <- xpathSApply(html.file, "//table[@border='1px']//nobr//a",xmlValue)[1:225]
distric.set <- as.numeric(gsub("ОИК №", "", table.column.names))
# here is a set of table elements with bold font, all of them are table cells entries
# except the first element "Сумма"    
table.values <- xpathSApply(html.file, "//table[@border='1px']//nobr//b",xmlValue)[-1]

k <- 33
number.of.voters <- as.numeric(table.values[(k+1):(k+225)])

k <- 225*2+33
issued.ballots <- as.numeric(table.values[(k+1):(k+225*3)]) %>% 
                    matrix(nrow = 3, byrow = T) %>% 
                    colSums()

k <- 225*8+33
number.of.invalid.ballots <- as.numeric(table.values[(k+1):(k+225)])

k <- 225*9+33
number.of.valid.ballots <- as.numeric(table.values[(k+1):(k+225)])

total.number.of.ballots <- number.of.invalid.ballots + number.of.valid.ballots
# here is the number of the first 18 rows by all 225 subjects plus number of the total 33 cells
k <- 225*18+33
election.results <- as.numeric(table.values[(k+1):(k+14*225)])
election.results <- matrix(election.results, nrow = 14, byrow = TRUE)

election.results.pct <- apply(rbind(election.results, number.of.invalid.ballots), 2, roundfixS)
# remove the percentages of invalid ballots
election.results.pct <- election.results.pct[-15,]

# total results with correction to 'Yabloko'
total.results <- round(rowSums(election.results)/sum(total.number.of.ballots)* 100, 2) - c(rep(0,10), 0.01, rep(0,3))
names(total.results) <- c(parties.titles)

# add total turnout value to the total.results
total.results <- c(total.results, turnout = round(sum(issued.ballots)/sum(number.of.voters)*100, 2))

election.results <- t(rbind(election.results, number.of.invalid.ballots, election.results.pct))
# Add the turnout figures 
election.results <- cbind(election.results, turnout = round(issued.ballots/number.of.voters * 100, 2))
# Set increasing order of districts (= rows of the matrix)
election.results <- election.results[order(distric.set),]

dimnames(election.results)[[2]] <- c(parties.titles, "invalid_ballots", paste0(parties.titles, ".pct"), "turnout")


############################################
# loading geo data
############################################
require(rgdal)
require(jsonlite)

# We use geo shapefiles of the election districts prepared by guys from gis-lab.info team in geojson format
russian.election.districts <- readOGR("okrug_all_diss-2016-02-06_simple", "29")
russian.election.districts@proj4string <-CRS("+init=epsg:3857")
russian.election.districts <- spTransform(russian.election.districts,
                                          CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

russian.election.districts@data <- select(russian.election.districts@data, distict.id = okrug)

# load districts titles from the original json files and modify them slightly
ru.geo.data <- fromJSON("29.geojson", simplifyVector = FALSE)
districts.titles <- sapply(ru.geo.data$features, function(x) x$properties$wiki_name)
districts.titles <- iconv(districts.titles, from = "UTF-8", to = "Windows-1251")
districts.titles <- gsub("одномандатный избирательный округ", "округ", districts.titles)
districts.titles <- gsub("автономная область", "АО", districts.titles)
districts.titles <- gsub("автономный округ", "АО", districts.titles)
districts.titles <- gsub("Республика", "Респ.", districts.titles)
districts.titles <- gsub("область", "обл.", districts.titles)
districts.titles <- gsub("Город", "г.", districts.titles)
districts.titles <- gsub("Адыгея \\(Адыгея\\)", "Адыгея", districts.titles)
districts.titles <- gsub("Татарстан \\(Татарстан\\)", "Татарстан", districts.titles)
rm(ru.geo.data)

# Add to the database the districts titles and the election results
russian.election.districts@data <- cbind(russian.election.districts@data,
                                         district = districts.titles,
                                         election.results)

# The areas of the election districts in Moscow and Saint-Petersburg are too small.
# That is why we plot them on separate maps. 
full.map <- readOGR("okrug_all_diss-2016-02-06", "okrug_all_diss")

Msk.districts <- 196:210
Msk.map <- full.map[Msk.districts,]

Spb.districts <- 211: 218
Spb.map <- full.map[Spb.districts,]


save(russian.election.districts, Msk.districts, Msk.map, Spb.districts, Spb.map,
     parties.titles, total.results, roundfixS, file = "election.results_leaflet.RData")
