install.packages("data.table") #v.1.9.6
# Install development version of data.table
install.packages("data.table", repos = "https://Rdatatable.github.io/data.table", type = "source") #v1.9.7
library("data.table")

#data table has it's own read function - to rapidly read data into R
#can be used for data.frames
gapminderFiveYearData <- fread("gapminder-FiveYearData.csv", data.table=F)
class(gapminderFiveYearData)
dim(gapminderFiveYearData)
head(gapminderFiveYearData)
tail(gapminderFiveYearData)
str(gapminderFiveYearData)

library("ggplot2")
ggplot(data = gapminderFiveYearData, aes(x = lifeExp, y = gdpPercap, color=continent)) +
  geom_point()

#defaults to a data.table
gapminderFiveYearData <- fread("gapminder-FiveYearData.csv")
class(gapminderFiveYearData)
dim(gapminderFiveYearData)
head(gapminderFiveYearData)
tail(gapminderFiveYearData)
str(gapminderFiveYearData)
#data tables also auto-trim when printing to console
gapminderFiveYearData

#data tables are backwards compatible with a lot of operations which use data.frames
#plots
dev.off()
ggplot(data = gapminderFiveYearData, aes(x = lifeExp, y = gdpPercap, color=continent)) +
  geom_point()
#linear models
linear_model <- lm(gdpPercap ~ pop + year, gapminderFiveYearData)
summary(linear_model)
linear_model <- lm(lifeExp ~ gdpPercap + pop + year, gapminderFiveYearData)
summary(linear_model)
linear_model <- glm(lifeExp ~ gdpPercap + continent + pop + year, family  ="gaussian", gapminderFiveYearData)
summary(linear_model)
#dplyr things
library("plyr")
calcGDP <- function(dat, year=NULL, country=NULL) {
  if(!is.null(year)) {
    dat <- dat[dat$year %in% year, ]
  }
  if (!is.null(country)) {
    dat <- dat[dat$country %in% country,]
  }
  gdp <- dat$pop * dat$gdpPercap
  
  new <- cbind(dat, gdp=gdp)
  return(new)
}
plyr::ddply(
  .data = calcGDP(gapminderFiveYearData),
  .variables = "continent",
  .fun = function(x) mean(x$gdp)
)

#fread is fast
gapminderlarge <- fread("gapminder-large.csv", header=T)
rm(gapminderlarge)
#fread is smart
gapminderFiveYearData <- fread("gapminder-FiveYearData.tsv") #tab delimited
gapminderFiveYearData <- fread("gapminder-FiveYearData.txt") #space delimited
#auto detects column classes, separators, headers, nrows - for a regularly separated file
#same comand for a whole bunch of file formats
#all the usual reading options can be specified manually 
gapminderFiveYearDataCrop <- fread("gapminder-FiveYearData.tsv", header=T, col.names=c("place", "time", "people", "big place", "life", "money"), nrows=1000, stringsAsFactors=F)
#cool progress bars for large files :)
gapminderlarger <- fread("gapminder-larger.csv") #so fast it tells you (9-10s)
#system.time(gapminderlarger <- read.csv("gapminder-larger.csv", header=T)) #the same operation took 57.203s with base R 

#FYI - there's also a "fast write"
fwrite(gapminderlarger, file="test.csv") #defaults to csv
fwrite(gapminderlarger, file="test.tsv", sep="\t")
#which is also fast to write data
system.time(write.csv(gapminderlarger, file="test.csv")) #46.216s user, 71.382s elapsed
#rm(gapminderlarger)


#bigmemory
library("bigmemory")
#uses the "big.matrix' format to access large data files in a C++ framework - rather than stored in RAM/memory as usual in R.
gapminderFiveYearData.big <- as.big.matrix(gapminderFiveYearData) #convert R data matrix into a "big.matrix"
gapminderFiveYearData.big
class(gapminderFiveYearData.big)
dim(gapminderFiveYearData.big)
head(gapminderFiveYearData.big)
tail(gapminderFiveYearData.big)
str(gapminderFiveYearData.big)
#also has read/write functions direct to big.matrix format
write.big.matrix(gapminderFiveYearData.big, "gapminder-FiveYearData.csv")
gapminderFiveYearData.big <- read.big.matrix("gapminder-FiveYearData.csv")
#efficient for memory - how fast is it?
system.time(gapminderlarger.big <- read.big.matrix("gapminder-larger.csv")) #28.272s user, 28.647s elapsed
system.time(write.big.matrix(gapminderFiveYearData.big, "test.csv")) #0.012s, 0.068s elapsed
  
#FEATHER (it's own fast file format) - from Hadley Wickham ggplot/dplyr/etc... and Wes Mckinney (pandas in Python)
#in development (unstable) - future versions may not read past versions - use to transfer files quickly (e.g., between R and Python)
library("devtools")
devtools::install_github("wesm/feather/R")
library(feather)
path <- "gapminder-FiveYearData.feather"
write_feather(gapminderFiveYearData, path) #write data frame to file
gapminderFiveYearData <- read_feather(path) #read to data frame
gapminderFiveYearData
#did I mention it's crazy fast?
path <- "gapminderlarger.feather"
system.time(write_feather(gapminderlarger, path)) #1.160s user, 5.008s elapsed
system.time(gapminderlarger <- read_feather(path)) #2.344 user, 2.414s elapsed

##FILE I/O Summary
##READ
#base R: read.csv: 57.203s
#data.table: fread: 8.154s
#bigmemory: 28.647s
#feather: 2.414s

#convert dataframe to format:
#data.table: 0.002s (0.001s back to dataframe)
#big.memory: 66.07s

##WRITE
#base R: write.csv: 71.382s
#data.table: fwrite: 35.453s
#bigmemory: 0.068s
#feather: 5.008s

#Manipulating Data Tables