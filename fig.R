############################################################
### Global Income Distribution graph

# Jack Blundell
# Stanford University
# Spring 2017

############################################################
## This file takes GCIP data and produces .html graphs
## CORE

### 1. Setup
############################################################

rm(list = ls()) 	
options("scipen"=100, "digits"=4)


setwd("C:/Users/Jack/Documents/Git/Globalinc") # set to github dir

library(epade)
#library(plotly)
library(reshape)
#library(rJava)
#library(xlsx)

 
raw <- read.csv("/Users/Jack/Dropbox/Documents/Projects/CORE/skyscraper/data/gid-previewexcel.csv", sep=',', header=TRUE)  # Full dataset
full <- raw

### 2. Filter countries
############################################################
# Drop HK
# Drop Singapore before 2005
# Drop all countries with less than 750,000 population - this is done year by year so countries can 'grow into' the chart
# Drop Uganda (wierd data)
# Drop Bosnia from 1996 (mean income doubles)

full <- full[which(full$country != "Hong Kong SAR, China"),]
full <- full[which(full$population>=750000),]

full$drop <- 0

for (i in 1:dim(full)[1]){
  if (full$country[i] == "Singapore" & full$year[i] <= 2000)
    full$drop[i] <- 1
  if (full$country[i] == "Uganda")
    full$drop[i] <- 1
  if (full$country[i] == "Bosnia and Herzegovina" & full$year[i] >= 1996)
    full$drop[i] <- 1
  if (full$country[i] == "Georgia"  & full$year[i] >= 1996)
    full$drop[i] <- 1
}

full <- full[which(full$drop ==0),]

### 2. Set up country labels and colour codes
############################################################

# Set which countries to label
count_label <- c("United States","United Kingdom","China","India","Nigeria","Indonesia","Brazil","Botswana","Japan","Norway")

# Assign labels
full$lab <- rep("")
for (i in 1:dim(full)[1]){
  if (is.element(full$country[i], count_label))
    full$lab[i] <- as.character(full$country[i])
}

# Set label colours
#full$labcol <-"#000000"
#for (i in 1:dim(full)[1]) {
#  if (full$lab[i] == "")
#    full$labcol[i] <- "FFFFFF"
#  }

#http://www.strangeplanet.fr/work/gradient-generator/index.php is a useful tool


### Option 1: background colours with a few countries highlighted

  # Set which countries to highlight in a different colour
  #count_high <- c("United States","United Kingdom","China","India")

  # Non-highlighted country colour:
  #col_back <- "#B9DFED"

  # Non-highlighted country transparency (0-1):
  #trans <- 0.1

  # Highlighted country colour:
  #col_high <- "F67F6A"

  # Assign colours to countries
  #full$col <- rep(col_back)
  #for (i in 1:dim(full)[1]){
  #  if (is.element(full$country[i], count_high))
  #    full$col[i] <- col_high
  #}

  # Assign transparency 

  #full$trans <- 1
  #for (i in 1:dim(full)[1]){
  #  if (full$col[i] == col_back)
  #    full$trans[i] <- trans
  #}

### Option 2: Spectrum of colours based on 1980 ranking

  col_codes <- "#A60000 #A70300 #A80700 #A90A00 #AA0E00 #AB1200 #AC1500 #AD1900 #AE1D00 #AF2000 #B02400 #B12800 #B32B00 #B42F00 #B53300 #B63600 #B73A00 #B83D00 #B94100 #BA4500 #BB4800 #BC4C00 #BD5000 #BE5300 #C05700 #C15B00 #C25E00 #C36200 #C46600 #C56900 #C66D00 #C77000 #C87400 #C97800 #CA7B00 #CC7F00 #CD8300 #CE8600 #CF8A00 #D08E00 #D19100 #D29500 #D39900 #D49C00 #D5A000 #D6A300 #D7A700 #D9AB00 #DAAE00 #DBB200 #DCB600 #DDB900 #DEBD00 #DFC100 #E0C400 #E1C800 #E2CC00 #E3CF00 #E4D300 #E6D600 #E7DA00 #E8DE00 #E9E100 #EAE500 #EBE900 #ECEC00 #EDF000 #EEF400 #EFF700 #F0FB00 #F2FF00 #EEFD00 #EBFB00 #E7F900 #E4F700 #E0F500 #DDF300 #D9F200 #D6F000 #D2EE00 #CFEC00 #CCEA00 #C8E800 #C5E700 #C1E500 #BEE300 #BAE100 #B7DF00 #B3DD00 #B0DC00 #ADDA00 #A9D800 #A6D600 #A2D400 #9FD200 #9BD000 #98CF00 #94CD00 #91CB00 #8DC900 #8AC700 #87C500 #83C400 #80C200 #7CC000 #79BE00 #75BC00 #72BA00 #6EB900 #6BB700 #68B500 #64B300 #61B100 #5DAF00 #5AAE00 #56AC00 #53AA00 #4FA800 #4CA600 #48A400 #45A200 #42A100 #3E9F00 #3B9D00 #379B00 #349900 #309700 #2D9600 #299400 #269200 #239000 #1F8E00 #1C8C00 #188B00 #158900 #118700 #0E8500 #0A8300 #078100 #048000 #048000 #048000 #048000 #048000 #048000 #048000 #048000 #048000 #048000"
  col_codes <- strsplit(col_codes, " ")
  order_year <- full[which(full$year=="1980"), ] # Dataset for a single year
  attach(order_year)
  order_year <- order_year[order(mean),]
  detach(order_year)
  for (i in 1:dim(order_year)[[1]]){
    order_year$col[i] <- col_codes[[1]][i]
  }
  col_codes <- as.vector(col_codes[[1]])

  vars <- c("country","col")
  country_col <- order_year[vars] # this provides a dataframe of country names and associated colour
  
  new_count_colname <- list() # these are used later in assigning colours to countries that are not present in the 1980 chart
  new_count_col <- list()
  
### 3. Convert income
############################################################

full$income1 <- 12*full$income1
full$income2 <- 12*full$income2
full$income3 <- 12*full$income3
full$income4 <- 12*full$income4
full$income5 <- 12*full$income5
full$income6 <- 12*full$income6
full$income7 <- 12*full$income7
full$income8 <- 12*full$income8
full$income9 <- 12*full$income9
full$income10 <- 12*full$income10

### 4. Main Loop
############################################################
# Could also just list individual years, but need to alter export section at end of this script accordingly

st_year <- 1980 # first year
end_year <- 2014 # last year

for (k in st_year:end_year){ #
  
### Useful toggles

yr <- k # choose a year to display

sing_year <- full[which(full$year==yr), ] # Dataset for a single year

### Assign colours pt 1

# assign colours from 1980 ranking

sing_year_col <- merge(sing_year,country_col,by="country", all.x = TRUE, sort = FALSE)
sing_year <- sing_year_col

### Order by average income

attach(sing_year)
sing_year <- sing_year[order(mean),]
detach(sing_year)

country_list <- sing_year$country # Extract country list

ncount <- length(country_list) # Number of countries in dataset for this year

### Assign colours pt 2
# if a country was not in the dataset in 1980, they now need to be assigned a colour.
# I assign them a colour roughly suitable to their starting position in the spectrum.
# NB: somewhat fiddly code here

# First check if the country has been assigned a colour and if it has, use it
if (length(new_count_col)!=0) {
for (i in 1:dim(sing_year)[1]) {
  for (j in 1:length(new_count_col)){
    if (sing_year$country[i] == new_count_colname[j])
      sing_year$col[i] <- new_count_col[j]
  }
}
}

# Next assign new colours for countries that have not been assigned one yet
for (i in 1:dim(sing_year)[1]) {
  if (is.na(sing_year$col[i]) == 1) {
    sing_year$col[i] <- col_codes[i]
    new_count_colname<- c(new_count_colname,as.character(sing_year$country[i]))
    new_count_col <- c(new_count_col,sing_year$col[i])
  }
}


### Scale by population

# Here I adjust so that the total size of the graph does not change over years

Prop_pop <- sing_year$population # Proportion of worold population in each country
Prop_pop <- (sing_year$population/sum(sing_year$population))*100
Prop_pop_rd <- ceiling(Prop_pop)
#sing_year$population <- sing_year$population / 10000000 # Put population in 10s of millions
#sing_year$population <- ceiling(sing_year$population) # Round population up

sing_year.expanded <- sing_year[rep(row.names(sing_year), Prop_pop_rd), 1:dim(sing_year)[2]] # scales by population

nbars <- dim(sing_year.expanded)[1] # Number of bars, ie pop*countries

### Move labels
# For countries with large populations (and hence more than one bloack in chart), we only want one label. This code deletes all but the first label.

for (i in 2:dim(sing_year.expanded)[1]) {
  if (is.element(sing_year.expanded$lab[i], sing_year.expanded$lab[1:i-1]) == 1) {
    sing_year.expanded$lab[i] <- ""
  }
}

# now for label colours
#for (i in 2:dim(sing_year.expanded)[1]) {
#  if (is.element(sing_year.expanded$lab[i], sing_year.expanded$lab[1:i-1]) == 1) {
#    sing_year.expanded$labcol[i] <- "FFFFFF"
#  }
#}

# Move China and India labels further on in chart

ch_in <- c("China","India")

for (i in 1:(dim(sing_year.expanded)[1]-10)) {
  if  (sing_year.expanded$lab[i] == "China") {
    sing_year.expanded$lab[i+12] <- sing_year.expanded$lab[i]
    sing_year.expanded$lab[i] <- ""
  break
      }
}

for (i in 1:(dim(sing_year.expanded)[1]-10)) {
  if  (sing_year.expanded$lab[i] == "India") {
    sing_year.expanded$lab[i+8] <- sing_year.expanded$lab[i]
    sing_year.expanded$lab[i] <- ""
    break
  }
}

### Gen restricted dataset

vars_list <- c("country","income1", "income2", "income3", "income4", "income5", "income6",
            "income7", "income8", "income9", "income10","col","lab") #trans", "labcol"
final <- sing_year.expanded[vars_list]


# rotate the matrix (optional)

#temp <- deciles
#for (i in 1:10)
#  temp[,i] = deciles[,11 -i]
#deciles <- temp


### 5. Export as .html code
############################################################

#write.csv(final, file = paste("Data/data", yr))

# This exports the full html with links to each year

sink(paste("docs/html/fig_",k,".html", sep =""))

cat("\n")
cat("<!--Comment-->")
cat("\n")
cat("<!DOCTYPE html>")
cat("\n")
cat("<html>")
cat("\n")
cat("<style>")
cat("\n")
cat("a:link {")
cat("\n")
cat("  color: #4863A0;")
cat("\n")
cat("}")
cat("\n")
cat("/* visited link */")
cat("\n")
cat("  a:visited {")
cat("\n")
cat("    color: #4863A0;")
cat("\n")
cat("  }")
cat("\n")
cat("/* mouse over link */")
cat("\n")
cat("  a:hover {")
cat("\n")
cat("    color: red;")
cat("\n")
cat("  }")
cat("\n")
cat("/* selected link */")
cat("\n")
cat("  a:active {")
cat("\n")
cat("    color: #4863A0;")
cat("\n")
cat("  }")
cat("\n")
cat("a:link {")
cat("\n")
cat("  text-decoration: none;")
cat("\n")
cat("}")
cat("\n")
cat("a:visited {")
cat("\n")
cat("  text-decoration: none;")
cat("\n")
cat("}")
cat("\n")
cat("a:hover {")
cat("\n")
cat("  text-decoration: underline;")
cat("\n")
cat("}")
cat("\n")
cat("a:active {")
cat("\n")
cat("  text-decoration: underline;")
cat("\n")
cat("}")
cat("\n")
cat("</style>")
cat("\n")  
cat("<center>")
cat("\n")  
cat("<font face=\"verdana\">")
cat("\n")  
cat("  <head style=\"font-family:verdana\">")
cat("\n")
cat("		<title>Global Income Distribution | amCharts</title>")
cat("\n")
cat("		<meta name=\"description\" content=\"chart created using amCharts live editor\" />")
cat("\n")
cat("")
cat("\n")
cat("		<!-- amCharts javascript sources -->")
cat("\n")
cat("		<script type=\"text/javascript\" src=\"http://www.amcharts.com/lib/3/amcharts.js\"></script>")
cat("\n")
cat("		<script type=\"text/javascript\" src=\"http://www.amcharts.com/lib/3/serial.js\"></script>")
cat("\n")
cat("		<script type=\"text/javascript\" src=\"http://www.amcharts.com/lib/3/plugins/export/export.js\"></script>")
cat("\n")
cat("		<link rel=\"stylesheet\" href=\"http://www.amcharts.com/lib/3/plugins/export/export.css\">")
cat("\n")
cat("")
cat("\n")
cat("		<!-- amCharts javascript code -->")
cat("\n")
cat("		<script type=\"text/javascript\">")
cat("\n")
cat("			AmCharts.makeChart(\"chartdiv\",")
cat("\n")
cat("				{")
cat("\n")
cat("					\"type\": \"serial\",")
cat("\n")
cat("					\"categoryField\": \"Label\",")
cat("\n")
cat("					\"columnWidth\": 1,")
cat("\n")
cat("					\"angle\": 30,")
cat("\n")
cat("					\"depth3D\": 170,")
cat("\n")
cat("					\"marginLeft\": 40,")
cat("\n")
cat("					\"marginRight\": 200,")
cat("\n")
cat("					\"marginBottom\": 100,")
cat("\n")
cat("					\"marginTop\": 70,")
cat("\n")
cat("					\"minMarginBottom\": 80,")
cat("\n")
cat("					\"sequencedAnimation\": false,")
cat("\n")
cat("					\"startDuration\": 0.000000000000, //0.00001 for animation")
cat("\n")
cat("					\"startEffect\": \"easeOutSine\",	")
cat("\n")
cat("					\"theme\": \"light\",")
cat("\n")
cat("					\"accessible\": true,")
cat("\n")
cat("					\"mouseWheelZoomEnabled\": false,")
cat("\n")
cat("					\"precision\": 0,")
cat("\n")
cat("					\"gridAboveGraphs\": false,")
cat("\n")
cat("					\"export\": {")
cat("\n")
cat("						\"enabled\": true")
cat("\n")
cat("					},")
cat("\n")
cat("					\"categoryAxis\": {")
cat("\n")
cat("						\"gridPosition\": \"start\",")
cat("\n")
cat("						\"axisAlpha\": 0,")
cat("\n")
cat("						\"labelOffset\": -5,")
cat("\n")
cat("						\"labelsEnabled\": true,")
cat("\n")
cat("						\"minHorizontalGap\": 5,")
cat("\n")
cat("						\"minVerticalGap\": 5,")
cat("\n")
cat("						\"labelColorField\": \"Label colour\",")
cat("\n")
cat("						\"labelRotation\": 25,")
cat("\n")
cat("						\"gridCount\": 10000,")
cat("\n")
cat("						\"gridAlpha\": 0.1,")
cat("\n")
cat("						\"minorGridEnabled\": true,")
cat("\n")
cat("						\"minorGridAlpha\": 1,")
cat("\n")
cat("						\"minorTickLength\": 10,")
cat("\n")
cat("					},")
cat("\n")
cat("					\"valueScrollbar\": {")
cat("\n")
cat("						\"enabled\": false")
cat("\n")
cat("					},")
cat("\n")
cat("					//\"chartCursor\": {")
cat("\n")
cat("					//	\"enabled\": true,")
cat("\n")
cat("					//	\"animationDuration\": 0,")
cat("\n")
cat("					//	\"avoidBalloonOverlapping\": false,")
cat("\n")
cat("					//	\"bulletsEnabled\": true,")
cat("\n")
cat("					//	\"bulletSize\": 13,")
cat("\n")
cat("					//	\"leaveCursor\": true,")
cat("\n")
cat("					//	\"limitToGraph\": \"AmGraph-10\",")
cat("\n")
cat("					//	\"oneBalloonOnly\": true,")
cat("\n")
cat("					//	\"pan\": true,")
cat("\n")
cat("					//	\"tabIndex\": 5")
cat("\n")
cat("				//	},")
cat("\n")
cat("					\"trendLines\": [],")
cat("\n")
cat("					\"graphs\": [")
cat("\n")
cat("						{")
cat("\n")
cat("							\"alphaField\": \"Transparency\",")
cat("\n")
cat("							\"balloonText\": \"[[title]] of [[cat]]: $[[value]]\",")
cat("\n")
cat("							\"behindColumns\": true,")
cat("\n")
cat("							\"fillAlphas\": 1,")
cat("\n")
cat("							\"fillColors\": \"#6899b5\",")
cat("\n")
cat("							\"id\": \"AmGraph-1\",")
cat("\n")
cat("							\"lineAlpha\": 0.9,")
cat("\n")
cat("							\"lineColor\": \"#74bee9\",")
cat("\n")
cat("							\"title\": \"Decile 1\",")
cat("\n")
cat("							\"type\": \"column\",")
cat("\n")
cat("							\"valueField\": \"Decile 1\",")
cat("\n")
cat("							\"fillColorsField\": \"Colour\",")
cat("\n")
cat("							\"lineColorField\": \"Colour\"")
cat("\n")
cat("						},")
cat("\n")
cat("						{")
cat("\n")
cat("							\"alphaField\": \"Transparency\",")
cat("\n")
cat("							\"balloonText\": \"[[title]] of [[cat]]: $[[value]]\",")
cat("\n")
cat("							\"fillAlphas\": 1,")
cat("\n")
cat("							\"fillColors\": \"#6899b5\",")
cat("\n")
cat("							\"id\": \"AmGraph-2\",")
cat("\n")
cat("							\"lineAlpha\": 0.9,")
cat("\n")
cat("							\"lineColor\": \"#74bee9\",")
cat("\n")
cat("							\"title\": \"Decile 2\",")
cat("\n")
cat("							\"type\": \"column\",")
cat("\n")
cat("							\"valueField\": \"Decile 2\",")
cat("\n")
cat("							\"fillColorsField\": \"Colour\",")
cat("\n")
cat("							\"lineColorField\": \"Colour\"")
cat("\n")
cat("						},")
cat("\n")
cat("						{")
cat("\n")
cat("							\"alphaField\": \"Transparency\",")
cat("\n")
cat("							\"balloonText\": \"[[title]] of [[cat]]: $[[value]]\",")
cat("\n")
cat("							\"fillAlphas\": 1,")
cat("\n")
cat("							\"fillColors\": \"#6899b5\",")
cat("\n")
cat("							\"id\": \"AmGraph-3\",")
cat("\n")
cat("							\"lineAlpha\": 0.9,")
cat("\n")
cat("							\"lineColor\": \"#74bee9\",")
cat("\n")
cat("							\"title\": \"Decile 3\",")
cat("\n")
cat("							\"type\": \"column\",")
cat("\n")
cat("							\"valueField\": \"Decile 3\",")
cat("\n")
cat("							\"fillColorsField\": \"Colour\",")
cat("\n")
cat("							\"lineColorField\": \"Colour\"")
cat("\n")
cat("						},")
cat("\n")
cat("						{")
cat("\n")
cat("							\"alphaField\": \"Transparency\",")
cat("\n")
cat("							\"balloonText\": \"[[title]] of [[cat]]: $[[value]]\",")
cat("\n")
cat("							\"fillAlphas\": 1,")
cat("\n")
cat("							\"fillColors\": \"#6899b5\",")
cat("\n")
cat("							\"id\": \"AmGraph-4\",")
cat("\n")
cat("							\"lineAlpha\": 0.9,")
cat("\n")
cat("							\"lineColor\": \"#74bee9\",")
cat("\n")
cat("							\"title\": \"Decile 4\",")
cat("\n")
cat("							\"type\": \"column\",")
cat("\n")
cat("							\"valueField\": \"Decile 4\",")
cat("\n")
cat("							\"fillColorsField\": \"Colour\",")
cat("\n")
cat("							\"lineColorField\": \"Colour\"")
cat("\n")
cat("						},")
cat("\n")
cat("						{")
cat("\n")
cat("							\"alphaField\": \"Transparency\",")
cat("\n")
cat("							\"balloonText\": \"[[title]] of [[cat]]: $[[value]]\",")
cat("\n")
cat("							\"fillAlphas\": 1,")
cat("\n")
cat("							\"fillColors\": \"#6899b5\",")
cat("\n")
cat("							\"id\": \"AmGraph-5\",")
cat("\n")
cat("							\"lineAlpha\": 0.9,")
cat("\n")
cat("							\"lineColor\": \"#74bee9\",")
cat("\n")
cat("							\"title\": \"Decile 5\",")
cat("\n")
cat("							\"type\": \"column\",")
cat("\n")
cat("							\"valueField\": \"Decile 5\",")
cat("\n")
cat("							\"fillColorsField\": \"Colour\",")
cat("\n")
cat("							\"lineColorField\": \"Colour\"")
cat("\n")
cat("						},")
cat("\n")
cat("						{")
cat("\n")
cat("							\"alphaField\": \"Transparency\",")
cat("\n")
cat("							\"balloonText\": \"[[title]] of [[cat]]: $[[value]]\",")
cat("\n")
cat("							\"fillAlphas\": 1,")
cat("\n")
cat("							\"fillColors\": \"#6899b5\",")
cat("\n")
cat("							\"id\": \"AmGraph-6\",")
cat("\n")
cat("							\"lineAlpha\": 0.9,")
cat("\n")
cat("							\"lineColor\": \"#74bee9\",")
cat("\n")
cat("							\"title\": \"Decile 6\",")
cat("\n")
cat("							\"type\": \"column\",")
cat("\n")
cat("							\"valueField\": \"Decile 6\",")
cat("\n")
cat("							\"fillColorsField\": \"Colour\",")
cat("\n")
cat("							\"lineColorField\": \"Colour\"							")
cat("\n")
cat("						},")
cat("\n")
cat("						{")
cat("\n")
cat("							\"alphaField\": \"Transparency\",")
cat("\n")
cat("							\"balloonText\": \"[[title]] of [[cat]]: $[[value]]\",")
cat("\n")
cat("							\"fillAlphas\": 1,")
cat("\n")
cat("							\"fillColors\": \"#6899b5\",")
cat("\n")
cat("							\"id\": \"AmGraph-7\",")
cat("\n")
cat("							\"lineAlpha\": 0.9,")
cat("\n")
cat("							\"lineColor\": \"#74bee9\",")
cat("\n")
cat("							\"title\": \"Decile 7\",")
cat("\n")
cat("							\"type\": \"column\",")
cat("\n")
cat("							\"valueField\": \"Decile 7\",")
cat("\n")
cat("							\"fillColorsField\": \"Colour\",")
cat("\n")
cat("							\"lineColorField\": \"Colour\"")
cat("\n")
cat("						},")
cat("\n")
cat("						{")
cat("\n")
cat("							\"alphaField\": \"Transparency\",")
cat("\n")
cat("							\"balloonText\": \"[[title]] of [[cat]]: $[[value]]\",")
cat("\n")
cat("							\"fillAlphas\": 1,")
cat("\n")
cat("							\"fillColors\": \"#6899b5\",")
cat("\n")
cat("							\"id\": \"AmGraph-8\",")
cat("\n")
cat("							\"lineAlpha\": 0.9,")
cat("\n")
cat("							\"lineColor\": \"#74bee9\",")
cat("\n")
cat("							\"title\": \"Decile 8\",")
cat("\n")
cat("							\"type\": \"column\",")
cat("\n")
cat("							\"valueField\": \"Decile 8\",")
cat("\n")
cat("							\"fillColorsField\": \"Colour\",")
cat("\n")
cat("							\"lineColorField\": \"Colour\"")
cat("\n")
cat("						},")
cat("\n")
cat("						{")
cat("\n")
cat("							\"alphaField\": \"Transparency\",")
cat("\n")
cat("							\"balloonText\": \"[[title]] of [[cat]]: $[[value]]\",")
cat("\n")
cat("							\"fillAlphas\": 1,")
cat("\n")
cat("							\"fillColors\": \"#6899b5\",")
cat("\n")
cat("							\"id\": \"AmGraph-9\",")
cat("\n")
cat("							\"lineAlpha\": 0.9,")
cat("\n")
cat("							\"lineColor\": \"#74bee9\",")
cat("\n")
cat("							\"title\": \"Decile 9\",")
cat("\n")
cat("							\"type\": \"column\",")
cat("\n")
cat("							\"valueField\": \"Decile 9\",")
cat("\n")
cat("							\"fillColorsField\": \"Colour\",")
cat("\n")
cat("							\"lineColorField\": \"Colour\"")
cat("\n")
cat("						},")
cat("\n")
cat("						{")
cat("\n")
cat("							\"alphaField\": \"Transparency\",")
cat("\n")
cat("							\"balloonText\": \"[[title]] of [[cat]]: $[[value]]\",")
cat("\n")
cat("							\"fillAlphas\": 1,")
cat("\n")
cat("							\"fillColors\": \"#6899b5\",")
cat("\n")
cat("							\"id\": \"AmGraph-10\",")
cat("\n")
cat("							\"lineAlpha\": 0.9,")
cat("\n")
cat("							\"lineColor\": \"#74bee9\",")
cat("\n")
cat("							\"title\": \"Decile 10\",")
cat("\n")
cat("							\"type\": \"column\",")
cat("\n")
cat("							\"valueField\": \"Decile 10\",")
cat("\n")
cat("							\"fillColorsField\": \"Colour\",")
cat("\n")
cat("							\"lineColorField\": \"Colour\"")
cat("\n")
cat("						}")
cat("\n")
cat("					],")
cat("\n")
cat("					\"guides\": [")
cat("\n")
cat("							{")
cat("\n")
cat("							\"balloonText\": \"Taller blocks correspond to higher incomes. Countries with larger populations are assigned more blocks. Colours correspond to how rich the country is in 1980, with poorer countries shaded red and richer countries shaded green. Try hovering over a block to learn more!\",")
cat("\n")
cat("							\"category\": \"X\",")
cat("\n")
cat("							\"dashLength\": 4,")
cat("\n")
cat("							\"expand\": true,")
cat("\n")
cat("							\"id\": \"Guide-1\",")
cat("\n")
cat("							\"inside\": true,")
cat("\n")
cat("							\"label\": \"\",")
cat("\n")
cat("							\"lineAlpha\": 1,")
cat("\n")
cat("							\"position\": \"right\",")
cat("\n")
cat("							\"toCategory\": \"X\",")
cat("\n")
cat("							\"toValue\": 100000,")
cat("\n")
cat("							\"value\": 0,")
cat("\n")
cat("							\"valueAxis\": \"ValueAxis-1\",")
cat("\n")
cat("										\"position\": \"top\",")
cat("\n")
cat("													\"inside\": true")
cat("\n")
cat("							}")
cat("\n")
cat("						],")
cat("\n")
cat("					\"valueAxes\": [")
cat("\n")
cat("						{")
cat("\n")
cat("							\"id\": \"ValueAxis-1\",")
cat("\n")
cat("							\"stackType\": \"3d\",")
cat("\n")
cat("							\"title\": \"Annual Income (2005$ PPP)\",")
cat("\n")
cat("							\"maximum\": 100000,")
cat("\n")
cat("							\"titleFontSize\": 15")
cat("\n")
cat("						}")
cat("\n")
cat("					],")
cat("\n")
cat("					\"allLabels\": [")
cat("\n")
cat("						{")
cat("\n")
cat("							\"id\": \"Label-1\",")
cat("\n")
cat("							\"size\": 15,")
cat("\n")
cat("							\"text\": \"Poorer countries\",")
cat("\n")
cat("							\"x\": \"10%\",")
cat("\n")
cat("							\"y\": \"95%\"")
cat("\n")
cat("						},")
cat("\n")
cat("						{")
cat("\n")
cat("							\"id\": \"Label-2\",")
cat("\n")
cat("							\"size\": 15,")
cat("\n")
cat("							\"text\": \"Richer countries\",")
cat("\n")
cat("							\"x\": \"70%\",")
cat("\n")
cat("							\"y\": \"95%\"")
cat("\n")
cat("					//	},")
cat("\n")
cat("					//	{")
cat("\n")
cat("					//		\"id\": \"Label-3\",")
cat("\n")
cat("					//		\"size\": 12,")
cat("\n")
cat("					//		\"text\": \"Poorer to richer people\",")
cat("\n")
cat("					//		\"x\": \"76%\",")
cat("\n")
cat("					//		\"y\": \"86%\",")
cat("\n")
cat("					//		\"rotation\": -30.6")
cat("\n")
cat("					//	},")
cat("\n")
cat("					//	{")
cat("\n")
cat("					//		\"id\": \"Label-3\",")
cat("\n")
cat("					//		\"size\": 12,")
cat("\n")
cat("					//		\"text\": \"within a country\",")
cat("\n")
cat("					//		\"x\": \"77%\",")
cat("\n")
cat("					//		\"y\": \"87%\",")
cat("\n")
cat("					//		\"rotation\": -30.6")
cat("\n")
cat("						}")
cat("\n")
cat("					],					")
cat("\n")
cat("					\"balloon\": {")
cat("\n")
cat("						\"fixedPosition\": false")
cat("\n")
cat("					},")
cat("\n")
cat("					\"titles\": [")
cat("\n")
cat("						{")
cat("\n")
cat("							\"id\": \"Title-1\",")
cat("\n")
cat("							\"size\": 20,")
cat("\n")
cat("							\"text\": \"Global Income Distribution ",paste(k),"\"", sep = "")
cat("\n")
cat("						}")
cat("\n")
cat("					],")
cat("\n")
cat("					\"dataProvider\": [")
cat("\n")
cat("					// insert data here")
cat("\n")

for (i in 1:length(final$country)) {
  cat("{")
  cat("\n")
  cat("\"cat\":"," \"",paste(final$country[i]),"\"",",", sep = "")
  cat("\n")
  cat("\"Decile 1\":"," \"",paste(final$income1[i]),"\"",",", sep = "")
  cat("\n")
  cat("\"Decile 2\":"," \"",paste(final$income2[i]),"\"",",", sep = "")
  cat("\n")
  cat("\"Decile 3\":"," \"",paste(final$income3[i]),"\"",",", sep = "")
  cat("\n")
  cat("\"Decile 4\":"," \"",paste(final$income4[i]),"\"",",", sep = "")
  cat("\n")
  cat("\"Decile 5\":"," \"",paste(final$income5[i]),"\"",",", sep = "")
  cat("\n")
  cat("\"Decile 6\":"," \"",paste(final$income6[i]),"\"",",", sep = "")
  cat("\n")
  cat("\"Decile 7\":"," \"",paste(final$income7[i]),"\"",",", sep = "")
  cat("\n")
  cat("\"Decile 8\":"," \"",paste(final$income8[i]),"\"",",", sep = "")
  cat("\n")
  cat("\"Decile 9\":"," \"",paste(final$income9[i]),"\"",",", sep = "")
  cat("\n")
  cat("\"Decile 10\":"," \"",paste(final$income10[i]),"\"",",", sep = "")
  cat("\n")
  cat("\"Colour\":"," \"",paste(final$col[i]),"\"",",", sep = "")
  cat("\n")
  cat("\"Label\":"," \"",paste(final$lab[i]),"\"",",", sep = "")
  cat("\n")
  #cat("\"Transparency\":"," \"",paste(final$trans[i]),"\"",",", sep = "") # only if using transparancies
  #cat("\n")
  #cat("\"Label colour\":"," \"",paste(final$labcol[i]),"\"",",", sep = "") # only if using label colours
  #cat("\n")
  cat("},")
  cat("\n")
}


cat("						//data ends here")
cat("\n")
cat("					]")
cat("\n")
cat("				}")
cat("\n")
cat("			);")
cat("\n")
cat("		</script>")
cat("\n")
cat("	</head>")
cat("\n")
cat("	<body>")
cat("\n")
cat("		<div id=\"chartdiv\" style=\"width: 100%; height: 600px; background-color: #FFFFFF;\" ></div>")
cat("\n")
cat(" <br/> <br/>")
cat("\n")
cat("	</body>")
cat("\n")
cat(" Pick another year, or return to the <a href=\"https://jackblun.github.io/Globalinc/\">home</a> page to learn more about the project <br /> <br />")
cat("\n")
for (i in st_year:end_year){
  cat("   <a href=\"fig_",paste(i),".html\">",paste(i),"</a>", sep ="") # link to other years
  cat("\n")
}
cat("\n")
  cat("</center>")
  cat("\n")
  cat("</font>")
  cat("\n")
cat("</html>")

sink()

}
