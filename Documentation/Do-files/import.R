require(gdata)
require(quanteda)
library(stringr)
library(dplyr)
library(ggplot2)
library(foreign)

#Set directory
setwd("/Users/morrisreeves/Desktop/Submission/Documentation/Data/Importable")

#Bureau to prefecture conversion table
guide <- read.xls("original_bureautoprefecture.xls")

#****ADOPTIONS****
#Function to clean adoption data
cleanadoptiondata <- function(filename, year){
  df <- read.xls(filename, sheet=3)
  #Clean bureau names column
  bnames <- as.list(tokens(as.character(df$X), remove_punct = TRUE))
  bnames <- sapply(bnames, function(n) paste(n, collapse=""))
  bnames <- bnames[!str_detect(as.list(bnames),pattern="[地]")] #remove top header
  bnames <- as.character(bnames[lapply(bnames,nchar)>0]) #remove empty elements of list
  #Get honsekinin adoptions column
  h_a <- as.numeric(gsub("[^.0-9]+","",df$X.7))
  h_a <- h_a[!is.na(h_a)]
  #Get hihonsekinin adoptions column
  hh_a <- as.numeric(gsub("[^.0-9]+","",df$X.9))
  hh_a <- hh_a[!is.na(hh_a)]
  #Combine bureau names and all three adoptions columns in to a data frame
  combined <- data.frame(bnames, h_a, hh_a, year)
  #Get rid of subtotal rows from data frame included in original excel file
  combined <- combined[!grepl("[管総外]+", combined$bnames),]
  #Calculate total adoptions per bureau by (honsekinin + hihonsekinin adoptions)
  combined <- combined %>% mutate("adoptions" = h_a + hh_a)
  bdata <- merge(combined, guide, by="bnames") 
  prefecturedata <- aggregate(adoptions ~ Prefecture+year, data=bdata, FUN=sum)
  return(prefecturedata)
}

#Clean adoption data and merge legal affairs bureaus with prefectures 
adoptions2006 <- cleanadoptiondata("original_adoptions2006.xls", year=2006) 
adoptions2007 <- cleanadoptiondata("original_adoptions2007.xls", year=2007)
adoptions2008 <- cleanadoptiondata("original_adoptions2008.xls", year=2008)
adoptions2009 <- cleanadoptiondata("original_adoptions2009.xls", year=2009)
adoptions2010 <- cleanadoptiondata("original_adoptions2010.xls", year=2010)
adoptions2011 <- cleanadoptiondata("original_adoptions2011.xls", year=2011)
adoptions2012 <- cleanadoptiondata("original_adoptions2012.xls", year=2012)
adoptions2013 <- cleanadoptiondata("original_adoptions2013.xls", year=2013)
adoptions2014 <- cleanadoptiondata("original_adoptions2014.xls", year=2014)
adoptions2015 <- cleanadoptiondata("original_adoptions2015.xls", year=2015)
adoptions2016 <- cleanadoptiondata("original_adoptions2016.xls", year=2016)

#Combines adoption (by year and prefecture) data into a single long data frame
adoptionseries <- do.call(rbind, mget(ls(pattern="^adoptions20*")))

#****POPULATION****
#Function to clean prefecture names column
cleanpnames <- function(factor){
  pnames <- sapply(as.character(factor), function(n) paste(n, collapse=""))
  pnames <- as.character(pnames[lapply(pnames,nchar)>0])
  pnames <- gsub("-ken","",pnames)
  pnames <- gsub("-fu","",pnames)
  pnames <- gsub("-to","",pnames)
  pnames <- gsub(" ","",pnames)
  pnames <- gsub("Gumma","Gunma",pnames) #Catches alternate spelling of "Gunma prefecture"
  pnames <- pnames[-1] #Remove label for row of national (Japan) totals
  pnames <- pnames[-c(48:length(pnames))] #Only keep the top 47 entries in the sheet (each prefecture)
  return(pnames)
}

#Function to get and clean population data from 2006 to 2015
getpopdata <- function(year=2006) {
  if(year==2016) return("Cannot use getpopdata to get 2016 data. Use getpopdata2016 instead.")
  #Heisei 18 (2006) corresponds to sheet 7 of the table, 2007 to sheet 8, etc.
  df <- read.xls("original_population2000to2015.xls", sheet=year-2006+7)
  #Clean prefecture names column
  pnames <- cleanpnames(df$X.2)
  #Clean population data columns:
  pop <- as.data.frame(matrix(data = NA,ncol=17,nrow=47)) #Initialize empty population data frame
  #Loop through 17 age groups (5 year increments from 0-4 to 80+) and populate prefecture populations
  for(agegroup in 4:19){
    colname = paste0("X.", agegroup)
    subpop <- df[colname]
    subpop <- subpop[[1]]
    subpop <- as.numeric(gsub("[^.0-9]+","",subpop))
    subpop <- subpop[!is.na(subpop)]
    subpop <- subpop[-c(1:2)]
    if(agegroup == 11) subpop <- subpop[-1] #quirk in data (the year is entered in this column)
    subpop <- subpop[-c(48:length(subpop))]
    subpop <- data.frame(subpop)
    pop[agegroup-3] <- subpop
  }
  #The "85 and older" age group was introduced (and appears in the data) starting from 2007.
  #Set last column to 80 and older if the year is 2006; otherwise, the last columns is the sum of the 80-84 and 85 and older columns.
  if(year==2006){
    over80 <- df$X.20
    over80 <- as.numeric(gsub("[^.0-9]+","",over80))
    over80 <- over80[!is.na(over80)]
    over80 <- over80[-c(1:2)]
    over80 <- over80[-c(48:length(over80))]
    over80 <- data.frame(over80)
    pop[17] <- over80
  }
  else{
    over80 <- df$X.20
    over80 <- as.numeric(gsub("[^.0-9]+","",over80))
    over80 <- over80[!is.na(over80)]
    over80 <- over80[-c(1:2)]
    over80 <- over80[-c(48:length(over80))]
    over85 <- df$X.21
    over85 <- as.numeric(gsub("[^.0-9]+","",over85))
    over85 <- over85[!is.na(over85)]
    over85 <- over85[-c(1:2)]
    over85 <- over85[-c(48:length(over85))]
    pop[17] <- data.frame(over80+over85)
  }
  names(pop) <- c("p0to4","p5to9","p10to14","p15to19","p20to24","p25to29","p30to34","p35to39","p40to44","p45to49","p50to54","p55to59","p60to64","p65to69","p70to74","p75to79","p80up")
  combined <- data.frame(pnames, pop, year)
  return(combined)
}

getpopdata2016 <- function() {
  #Extract 2016 data separately (not available as part of the 2006-2015 time series)
  url2016 <- "https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031560319&fileKind=0"
  filename2016 <- "~/importable_population2016.xls"
  download.file(url2016, filename2016)
  df2016 <- read.xls(filename2016)
  pop2016 <- as.data.frame(matrix(data = NA,ncol=17,nrow=47))
  
  pnames <- cleanpnames(df2016$X.11)
  
  for(agegroup in 2:17){
    colname = paste0("X", agegroup)
    subpop <- df2016[colname]
    subpop <- subpop[[1]]
    subpop <- as.numeric(gsub("[^.0-9]+","",subpop))
    subpop <- subpop[!is.na(subpop)]
    subpop <- subpop[-c(1:2)]
    if(agegroup == 9) subpop <- subpop[-c(1:2)] #quirk in data
    if(agegroup == 8) subpop <- subpop[-c(1:2)] #quirk in data
    subpop <- subpop[-c(48:length(subpop))]
    subpop <- data.frame(subpop)
    pop2016[agegroup-1] <- subpop
  }
  over80 <- df2016$X18
  over80 <- as.numeric(gsub("[^.0-9]+","",over80))
  over80 <- over80[!is.na(over80)]
  over80 <- over80[-c(1:2)]
  over80 <- over80[-c(48:length(over80))]
  over85 <- df2016$X19
  over85 <- as.numeric(gsub("[^.0-9]+","",over85))
  over85 <- over85[!is.na(over85)]
  over85 <- over85[-c(1:2)]
  over85 <- over85[-c(48:length(over85))]
  pop2016[17] <- data.frame(over80+over85)
  names(pop2016) <- c("p0to4","p5to9","p10to14","p15to19","p20to24","p25to29","p30to34","p35to39","p40to44","p45to49","p50to54","p55to59","p60to64","p65to69","p70to74","p75to79","p80up")
  combined <- data.frame(pnames, pop2016, year=2016)
  return(combined)
}

#Combines population (by year and prefecture) data into a single long data frame
datalist = list()
for(year in 2006:2016){
  if(year==2016) {datalist[[year-2005]] <- getpopdata2016()}
  else {datalist[[year-2005]] <- getpopdata(year)}
}
pseries <- do.call(rbind, datalist)

#Convert population (in 1000s) to population
for(i in 2:18){
  pseries[i] = pseries[i]*1000
}

#Creates column with total population per prefecture per year (add population subtotals)
pseries <- pseries %>% mutate("ptotal" = rowSums(pseries[, 2:18]))
names(pseries)[1] <- "Prefecture"


#****ASSETS****
#original_thirdquartilenetsavingsallhh2014
getassetdata <- function(filename, year=2014) {
  if(year!=2014) stop("Warning. Prefecture-level asset data for all households is only available in 2014.")
  raw <- read.xls(filename, sheet=1)
  asset <- raw[c(106:125),c(30:76)] #Remove all non-data rows and columns
  asset <- asset[-seq(1, nrow(asset), 4),] #Remove label rows
  asset <- as.data.frame(sapply(asset, function(n) as.numeric(as.character(n))))
  
  prefnames <- read.xls("original_prefecturelist.xls", header=FALSE)[1] #Gets prefecture names (same order as the data)
  prefnames <- lapply(prefnames, as.character)
  prefnames <- unlist(prefnames)
  colnames(asset) <- prefnames
  asset <- as.data.frame(t(asset))
  for(i in 1:15){
    asset[i] = asset[i]*1000 #Multiplies by 1000, since asset quartiles and median are in thousands of yen
  }
  #Add labels: Balance of savings and liabilities (in thousand yen); Estimated value of houses and residential land (in thousand yen); Estimated value of present residence (in thousand yen); Estimated value of excepted present residence (in thousand yen); Estimated value of durable goods (in thousand yen)
  colnames(asset) <- c("sminusl_1", "sminusl_med", "sminusl_3", "houseres_1", "houseres_med", "houseres_3", "res_1", "res_med", "res_3", "nonres_1", "nonres_med", "nonres_3", "durable_1", "durable_med", "durable_3")
  asset$year = year
  asset$Prefecture = prefnames
  return(asset)
}
clean_thirdquartilenetsavingsallhh2014 <- getassetdata("original_thirdquartilenetsavingsallhh2014.xls",year=2014)

#original_elderlyhhavgsavings2009 and 2014:
getassetdata2 <- function(filename, year=2009) {
  if(year==2014|year==2009){
    raw <- read.xls(filename, sheet=1)
    if(year==2014) asset <- raw[c(15:61), c(13:16, 18, 32)]
    if(year==2009) asset <- raw[c(16:62), c(13:16, 18, 32)]
    asset <- apply(asset, 2, gsub, patt=",", replace="")
    asset <- as.data.frame(apply(asset, 2, as.numeric))
    colnames(asset) <- c("personsperhh", "earnersperhh", "agehhhead", "malehhhead", "yincome", "savings")
    prefnames <- read.xls("original_prefecturelist.xls", header=FALSE)[1] #Gets prefecture names (same order as the data)
    prefnames <- lapply(prefnames, as.character)
    prefnames <- unlist(prefnames)
    rownames(asset) <- prefnames
    asset$yincome <- asset$yincome*1000 #Multiply yearly income by thousand (since data is in thousands)
    asset$savings <- asset$savings*1000 #Multiply savings by thousand (since data is in thousands)
    asset$year = year
    asset$Prefecture = prefnames
    return(asset)
  }
  else{
    stop("Given year is not in the time period of analysis for which data is available.")
  }
}

clean_original_elderlyhhavgsavings2009 <- getassetdata2("original_elderlyhhavgsavings2009.xls",year=2009)
clean_original_elderlyhhavgsavings2014 <- getassetdata2("original_elderlyhhavgsavings2014.xls",year=2014)


#original_prophhassets2plushh2009:
getassetdata3 <- function(filename, year=2009){
  raw <- read.xls(filename, sheet=1)
  if(year==2009) asset <- raw[c(16:24),c(51:97)]
  else asset <- raw[c(13:21),c(30:76)]
  asset <- apply(asset, 2, gsub, patt=",", replace="")
  asset <- apply(asset, 2, gsub, patt="--", replace="-")
  asset <- as.data.frame(apply(asset, 2, as.numeric))
  prefnames <- read.xls("original_prefecturelist.xls", header=FALSE)[1]
  prefnames <- lapply(prefnames, as.character)
  prefnames <- unlist(prefnames)
  colnames(asset) <- prefnames
  asset <- as.data.frame(t(asset))
  asset$year = year
  asset$Prefecture = prefnames
  asset$prophhover100M = asset$V9/(asset$V1+asset$V2+asset$V3+asset$V4+asset$V5+asset$V6+asset$V7+asset$V8+asset$V9)
  prophhassetover100M <- asset[,c("Prefecture", "prophhover100M", "year")]
  return(prophhassetover100M)
}

clean_prophhassets2plushh2009 <- getassetdata3("original_prophhassets2plushh2009.xls",year=2009)
clean_prophhassets2plushh2014 <- getassetdata3("original_prophhassets2plushh2014.xls",year=2014)

#****FARMERS****
raw <- read.xls("original_2007farmers.xls", sheet=6)
farmers2007 <- raw[c(10:56),c(26, 38)]
prefnames <- as.character(read.xls("original_prefecturelist.xls", header=FALSE)[,1])
farmers2007$Prefecture <- prefnames
colnames(farmers2007)[1] <- "farmers55to64"
farmers2007$farmers55to64 <- as.numeric(as.character(farmers2007$farmers55to64))
raw <- read.xls("original_2007farmers.xls", sheet=7)
farmers2007$farmers65up <- raw[c(10:56),c(26)]
farmers2007$farmers65up <- as.numeric(as.character(farmers2007$farmers65up))
farmers2007$year <- 2007
farmers2007 <- farmers2007[,c(1,3,4,5)]

raw <- read.xls("original_2012farmers.xls", sheet=6)
farmers2012 <- raw[c(11:57),c(30, 43)]
farmers2012$Prefecture <- prefnames
colnames(farmers2012)[1] <- "farmers55to64"
farmers2012$farmers55to64 <- as.numeric(as.character(farmers2012$farmers55to64))
raw <- read.xls("original_2012farmers.xls", sheet=7)
farmers2012$farmers65up <- raw[c(11:57),c(30)]
farmers2012$farmers65up <- as.numeric(as.character(farmers2012$farmers65up))
farmers2012$year <- 2012
farmers2012 <- farmers2012[,c(1,3,4,5)]

#****SELF-EMPLOYED ELDERLY****
raw <- read.xls("original_2007selfemployed.xlsx", sheet=6)
selfempl2007 <- raw[c(12:58),c(11, 35)]
selfempl2007$Prefecture <- prefnames
colnames(selfempl2007)[1] <- "selfemp55to64"
selfempl2007$selfemp55to64 <- gsub(patt=",", replace="", selfempl2007$selfemp55to64)
raw <- read.xls("original_2007selfemployed.xlsx", sheet=7)
selfempl2007$selfemp65up <- raw[c(12:58),c(11)]
selfempl2007$selfemp65up <- gsub(patt=",", replace="", selfempl2007$selfemp65up)
selfempl2007$selfemp55to64 <- as.numeric(as.character(selfempl2007$selfemp55to64))
selfempl2007$selfemp65up <- as.numeric(as.character(selfempl2007$selfemp65up))
selfempl2007$year <- 2007
selfempl2007 <- selfempl2007[,c(1,3,4,5)]

raw <- read.xls("original_2012selfemployed.xls", sheet=6)
selfempl2012 <- raw[c(14:60),c(11, 34)]
selfempl2012$Prefecture <- prefnames
colnames(selfempl2012)[1] <- "selfemp55to64"
selfempl2012$selfemp55to64 <- gsub(patt=",", replace="", selfempl2012$selfemp55to64)
raw <- read.xls("original_2012selfemployed.xls", sheet=7)
selfempl2012$selfemp65up <- raw[c(14:60),c(11)]
selfempl2012$selfemp65up <- gsub(patt=",", replace="", selfempl2012$selfemp65up)
selfempl2012$selfemp55to64 <- as.numeric(as.character(selfempl2012$selfemp55to64))
selfempl2012$selfemp65up <- as.numeric(as.character(selfempl2012$selfemp65up))
selfempl2012$year <- 2012
selfempl2012 <- selfempl2012[,c(1,3,4,5)]

#****BUSINESSES (2014)****
raw <- read.xls("original_2014businessbysize.xls", sheet=1)
businesses <- raw[c(12:58),c(47, 56)]
businesses$Prefecture <- prefnames
colnames(businesses)[1] <- "empl5to9"
colnames(businesses)[2] <- "empl300plus"
businesses$empl5to9 <- as.numeric(as.character(gsub(patt=",", replace="", businesses$empl5to9)))
businesses$empl300plus <- as.numeric(as.character(gsub(patt=",", replace="", businesses$empl300plus)))
businesses$year <- 2014

#****ELDERLY CARE (2006 to 2016)****
elderlycare <- read.xls("importable_elderlycare2006to2016.xlsx", sheet=1)
elderlycare$Prefecture <- as.character(elderlycare$Prefecture)

#****original_numelderlyincomeallhh2014****
raw <- read.xls("original_numelderlyincomeallhh2014.xls", sheet=1)
numelderlyincomeallhh2014 <- as.data.frame(t(raw[c(18, 30),c(36:82)]))
colnames(numelderlyincomeallhh2014) <- c("people65upperhh", "avgannualincperhh")
numelderlyincomeallhh2014$people65upperhh <- as.numeric(as.character(gsub(patt=",", replace="", numelderlyincomeallhh2014$people65upperhh)))
numelderlyincomeallhh2014$avgannualincperhh <- 1000 * as.numeric(as.character(gsub(patt=",", replace="", numelderlyincomeallhh2014$avgannualincperhh)))
numelderlyincomeallhh2014$Prefecture <- prefnames
numelderlyincomeallhh2014$year <- 2014

#****PRIMARY INDUSTRY (2006 to 2016)****
primaryindustry <- read.csv("importable_primaryindustryGDP2001to2014.csv")
primaryindustry$Prefecture <- as.character(primaryindustry$Prefecture)

#****INCOME FROM TAX FILINGS (2006 to 2016)****
income <- read.csv("importable_taxincome2006to2016.csv", stringsAsFactors = FALSE)
income[] <- lapply(income, gsub, pattern='－', replacement=0)
income[] <- lapply(income, gsub, pattern='-', replacement=0)
income[,2:ncol(income)] <- apply(income[,2:ncol(income)], 2, as.numeric)
income <- income %>% mutate("above50Myen"= (X100to50Myen_Filer + X200to100Myen_Filer + X500to200Myen_Filer + X1Bto500Myen_Filer + X2Bto1Byen_Filer + X5Bto2Byen_Filer + X10Bto5Byen_Filer + X10ByenAbove_Filer))
income <- income[,c("Prefecture", "year", "above50Myen")]

#****NUMBER OF HOUSEHOLDS IN 2014****
raw <- read.xls("original_numhouseholds2014.xls", stringsAsFactors = FALSE)
numhousehold <- raw[-c(1:4),c(2,6)]
numhousehold$Prefecture <- prefnames
numhousehold$year <- 2014
colnames(numhousehold)[2] <- "numhh"
numhousehold$numhh <- as.numeric(as.character(gsub(patt=",", replace="", numhousehold$numhh)))

#****MERGE FILES****
#ADOPTIONS PER 10000
adoptionseries$Prefecture <- as.character(adoptionseries$Prefecture)
temp <- merge(adoptionseries, pseries, by=c("Prefecture", "year"))
temp <- merge(temp, numhousehold, by=c("Prefecture", "year"), all.x=TRUE)
temp <- temp %>% mutate("aperhh" = (adoptions/numhh))
adoption_capita <- temp %>% mutate("aper10000" = (adoptions/ptotal)*10000) %>% select(Prefecture, year, aper10000, aperhh, numhh)

#ELDERLY
pseries <- pseries %>% mutate("elderlyper10000" = 10000*(p55to59 + p60to64 + p65to69 + p70to74 + p75to79 + p80up)/ptotal)
pseries$Prefecture <- as.character(pseries$Prefecture)
full <- merge(adoption_capita,pseries[,c("Prefecture", "year", "elderlyper10000", "ptotal")],by=c("Prefecture", "year"))
full$Prefecture <- as.character(full$Prefecture)

#ELDERLY CARE SERVICES
full <- merge(full, elderlycare, by=c("Prefecture", "year"))
full <- full %>% mutate("RojinOccupants10000" = 10000*(RojinFukushiOccupants)/ptotal)

#ELDERLY SELF EMPLOYED
selfemployed <- rbind(selfempl2007, selfempl2012)
full <- merge(full, selfemployed, by=c("Prefecture", "year"), all.x=TRUE)
full <- full %>% mutate("oldSelf10000" = 10000*(selfemp55to64 + selfemp65up)/ptotal)

#ELDERLY FARMER
farmers <- rbind(farmers2007, farmers2012)
full <- merge(full, farmers, by=c("Prefecture", "year"), all.x=TRUE)
full$oldFFF <- 10000 * (full$farmers55to64+full$farmers65up)/full$ptotal

#REGIONS
regions <- read.csv("original_regions.csv", stringsAsFactors = FALSE)
full <- merge(full, regions, by="Prefecture")

#clean_thirdquartilenetsavingsallhh2014
clean_thirdquartilenetsavingsallhh2014 <- clean_thirdquartilenetsavingsallhh2014[,c("Prefecture", "year", "sminusl_3")]
full <- merge(full, clean_thirdquartilenetsavingsallhh2014, by=c("Prefecture", "year"), all.x = TRUE)

#clean_original_elderlyhhavgsavings
clean_original_elderlyhhavgsavings2009 <- clean_original_elderlyhhavgsavings2009[,c("Prefecture", "year", "savings")]
clean_original_elderlyhhavgsavings2014 <- clean_original_elderlyhhavgsavings2014[,c("Prefecture", "year", "savings")]
clean_original_elderlyhhavgsavings <- rbind(clean_original_elderlyhhavgsavings2009, clean_original_elderlyhhavgsavings2014)
full <- merge(full, clean_original_elderlyhhavgsavings, by=c("Prefecture", "year"), all.x = TRUE)

#clean_prophhassets2plushh
clean_prophhassets2plushh <- rbind(clean_prophhassets2plushh2009, clean_prophhassets2plushh2014)
full <- merge(full, clean_prophhassets2plushh, by=c("Prefecture", "year"), all.x = TRUE)

#businesses (by employees #)
full <- merge(full, businesses, by=c("Prefecture", "year"), all.x = TRUE)

#numelderlyincomeallhh2014
full <- merge(full, numelderlyincomeallhh2014, by=c("Prefecture", "year"), all.x = TRUE)

#primaryindustry
primaryindustry <- primaryindustry %>% mutate("primaryProp" = primaryProdMill/(primaryProdMill+secondaryProdMill+tertiaryProdMill))
primaryindustry <- primaryindustry[,c("Prefecture", "year", "primaryProp")]
full <- merge(full, primaryindustry, by=c("Prefecture", "year"), all.x = TRUE)

#income from tax filings
full <- merge(full, income, by=c("Prefecture", "year"), all.x = TRUE)
full <- full %>% mutate("above50Myen10000" = 10000*(above50Myen/ptotal))
full <- full %>% mutate("empl5to9perhh" = empl5to9/numhh)
full <- full %>% mutate("empl300plusperhh" = empl300plus/numhh)

final <- full[,c("Prefecture", "year", "aper10000", "aperhh", "elderlyper10000", "RojinOccupants10000", "oldSelf10000", "oldFFF", "Region", "sminusl_3", "savings", "prophhover100M", "people65upperhh", "avgannualincperhh", "primaryProp", "above50Myen10000", "empl5to9perhh", "empl300plusperhh")]
write.dta(final, "/Users/morrisreeves/Desktop/Submission/Documentation/Do-files/final.dta")



#Plots of adoption rates
library(choroplethr)
library(choroplethrAdmin1)
prefectural <- full
prefectural$value <- full$aper10000
prefectural$region <- tolower(full$Prefecture)
prefectural <- prefectural[prefectural$year==2007,]
tiff("AdoptionsMap2007.tiff", height = 12, width = 17, units = 'cm', compression = "lzw", res = 300)
admin1_choropleth("japan", prefectural, num_colors = 5) + 
  ggtitle("\n\nAdoptions per 10,000 in 2007") + 
  theme(plot.title = element_text(hjust = 0.5, vjust=2, size=10), legend.position = c(0.8,0.2))
dev.off()

prefectural2 <- full
prefectural2$value <- full$aper10000
prefectural2$region <- tolower(full$Prefecture)
prefectural2 <- prefectural2[prefectural2$year==2012,]
tiff("AdoptionsMap2012.tiff", height = 12, width = 17, units = 'cm', compression = "lzw", res = 300)
admin1_choropleth("japan", prefectural2, num_colors = 5) + 
  ggtitle("\n\nAdoptions per 10,000 in 2012") + 
  theme(plot.title = element_text(hjust = 0.5, vjust=2, size=10), legend.position = c(0.8,0.2))
dev.off()

#Set directory
setwd("/Users/morrisreeves/Desktop/")
#Histogram of adoptions per capita
tiff("HistAdoptionsPer100002006-2016.tiff", height = 12, width = 17, units = 'cm', compression = "lzw", res = 300)
qplot(full$aper10000,geom="histogram",
      main = "Histogram for Adoptions per 10,000 People (2006-2016)",
      xlab = "Adoptions per 10,000 People", ylab = "Count", binwidth = 0.2)+ theme_bw() +
      theme(plot.title = element_text(hjust = 0.5), text=element_text(family="Times New Roman"))
dev.off()

