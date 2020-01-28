# !diagnostics off

#BSA Data Cleanup
#extracting data from Excel workbook(s) and export as .csv files
#output: a folder, named after the workbook, that's populated by the indvidual .csv files
#questions: sarah.perry@water.ca.gov

#~~~~~~~~~~~~~~~~~~~~~~~~
#~~~Variables to Edit~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~
#clear workspace
rm(list = ls())

#name of excel workbook(s)
filePath = 'C:/R/Zoop/' #eg.  #(not backslashes)
excelFile = c('zoop_2018-03_2018-12.xlsx') #keep parentheses; add multiple excel files if wanted

#~~~~~~~~~~~~~~~~~~~~~~
#~~~CODE STARTS HERE~~~#
#~~~~~~~~~~~~~~~~~~~~~~
#import packages
require(readxl)
require(janitor)
require(tidyverse)

#~~~Extract the Data~~~#
#create full file
for (wkbk in excelFile) {
  excelBook <- paste(filePath,wkbk, sep='') #full path
  excelName <- strsplit(wkbk, '.', fixed=TRUE)[[1]][1] #remove extension from name
  
  #grab all the sheet names in a map
  sheetMap <- excel_sheets(excelBook) 
  
  #iterate over all the sheets
  for (sheetName in sheetMap) {
    #set up list of column types for extracting data
    dfTypes <- c('guess','skip','skip','skip','skip','skip','skip','skip','numeric','skip','numeric')
    
    #import left side of data
    dfOne <- suppressMessages(
      read_excel(
        excelBook
        ,sheet = sheetName
        ,range = "A18:K65"
        ,col_types = dfTypes
        ,col_names = F
      )
    )
    
    #import right side of data
    dfTwo <- suppressMessages(
      read_excel(
        excelBook
        ,sheet = sheetName
        ,range= "Q18:AA63"
        ,col_types = dfTypes
        ,col_names = F
      )
    )
    
    #combine the two data sets
    dfAll <- rbind(dfOne,dfTwo)
    
    #~~~Clean Up Data~~~#
    #rename columns
    names(dfAll) <- c('taxon','count','subsample')
    
    #skip sheet if no data
    if (sum(dfAll$count, na.rm = TRUE) == 0) {
      next
    }
    
    #remove blank rows
    dfAbund <- janitor::remove_empty(dfAll, which = 'rows')
    
    #Populate the Category Column
    #create a list of the categories
    catList <- subset(dfAbund$taxon, dfAbund$taxon == toupper(dfAbund$taxon))
    
    #set starting values for the for loop
    catCount <- 1
    start <- 1
    catVal <- 2
    catVec <- c()
    
    #populate category vector
    for (x in seq(start, length(dfAbund$taxon))) {
      catCount <- catCount + 1
      if (catVal <= length(catList)-1) {
        if (dfAbund$taxon[x] != catList[catVal]) {
          catVec <- c(catVec,catList[catVal-1])
        } else {
          catVec <- c(catVec,catList[catVal])
          start <- catCount
          catVal = catVal + 1
        }
      } else {
        catVec <- c(catVec,catList[catVal])
      }
    }
    
    #append to df
    dfAbund$category <- catVec
    
    #change Harpacticoids to lowercase
    for (x in seq(1, length(dfAbund$taxon))) {
      if (dfAbund$taxon[x] == 'HARPACTICOIDS') {
        dfAbund$taxon[x] = 'Harpacticoids'
      }
    }
    
    #remove category rows based on case
    upperCols <- dfAbund$taxon == toupper(dfAbund$taxon)
    dfAbund <- dfAbund[!upperCols,]
    
    #change NAs to 0s
    dfAbund$count[is.na(dfAbund$count)] <- 0
    dfAbund$subsample[is.na(dfAbund$subsample)] <- 0
    
    #~~~Extract the Metadata~~~#
    #pull out date 
    dfDate <- suppressMessages(
      read_excel(
        excelBook
        ,sheet = sheetName
        ,range = 'F6:M6'
        ,col_types = 'numeric'
        ,col_names = F
      )
    )
    
    #collapse date to string
    dateStr <- unlist(dfDate, use.names = FALSE) %>% paste(.,collapse = '')
    
    #convert to datetime and add to df
    dateDate <- as.Date(dateStr, '%m%d%Y')
    dfAbund$date <- dateDate
    
    #extract time from sheet
    dfTime <- suppressMessages(
      read_excel(
        excelBook
        ,sheet = sheetName
        ,range = 'F9'
        ,col_types = 'date'
        ,col_names = F
      )
    )
    
    #format timestamp
    timeDate <- strptime(dfTime[[1]], "%Y-%m-%d %H:%M:%S") %>% format(., '%H:%M')
    
    #add time to df
    dfAbund$time <- timeDate
    
    #extract sample name
    sampName <- as.character(
      suppressMessages(
        read_excel(
          excelBook
          ,sheet = sheetName
          ,range = 'V6'
          ,col_names = F
        )
      )
    )
    
    #remove whitespace
    sampName <- trimws(sampName)
    
    #changes spaces to underscores
    if (' ' %in% sampName) {
      sampName <- gsub(" ", "_", sampName)  
    }
    
    #add samp name to df
    dfAbund$sample <- sampName
    
    #Populate/Append Remaining Variables
    #import v1 df
    vOneDf <- suppressMessages(
      read_excel(
        excelBook
        ,sheet = sheetName
        ,range = 'M9:Q9'
        ,col_types = 'numeric'
        ,col_names = F
      )
    )
    
    #grab value/append to df
    if (sum(vOneDf, na.rm = TRUE) == 0) {
      print(paste('Check sheet',sheetName)) #error in sheet
      dfAbund$v1_ml <- NA #append NA
    } else {
      vOne <- vOneDf[,colSums(is.na(vOneDf)) == 0][[1]] #collapse to value
      dfAbund$v1_ml <- vOne #append
    }
    
    #import v2 df
    vTwoDf <- suppressMessages(
      read_excel(
        excelBook
        ,sheet = sheetName
        ,range = 'M9:Q9'
        ,col_types = 'numeric'
        ,col_names = F
      )
    )
    
    #grab value/append to df
    if (sum(vTwoDf, na.rm = TRUE) == 0) {
      print(paste('Check sheet',sheetName)) #error in sheet
      dfAbund$v2_ml <- NA #append NA
    } else {
      vTwo <- vTwoDf[,colSums(is.na(vTwoDf)) == 0][[1]] #collapse to value
      dfAbund$v2_ml <- vTwo #append
    }
    
    #import sub1 df
    subOneDf <- suppressMessages(
      read_excel(
        excelBook
        ,sheet = sheetName
        ,range = 'U9:W9'
        ,col_types = 'numeric'
        ,col_names = F
      )
    )
    
    #grab value/append to df
    if (sum(subOneDf, na.rm = TRUE) == 0) {
      print(paste('Check sheet',sheetName)) #error in sheet
      dfAbund$sub1_ml <- NA #append NA
    } else {
      subOne <- subOneDf[,colSums(is.na(subOneDf)) == 0][[1]] #collapse to value
      dfAbund$sub1_ml <- subOne #append
    }
    
    #import sub2 df
    subTwoDf <- suppressMessages(
      read_excel(
        excelBook
        ,sheet = sheetName
        ,range = 'H12:I12'
        ,col_types = 'numeric'
        ,col_names = F
      )
    )
    
    #grab value/append to df
    if (sum(subTwoDf, na.rm = TRUE) == 0) {
      print(paste('Check sheet',sheetName)) #error in sheet
      dfAbund$sub2_ml <- NA #append NA
    } else {
      subTwo <- subTwoDf[,colSums(is.na(subTwoDf)) == 0][[1]] #collapse to value
      dfAbund$sub2_ml <- subTwo #append
    }
  
    #~~~Export Dataframe~~~#  
    #reorganize columns
    dfAbund <- dfAbund[,c('sample','date','time','category','taxon','count','subsample','v1_ml','sub1_ml','v2_ml','sub2_ml')]
  
    #export CSV files into own folder
    fullPath <- paste(filePath,excelName,'/', sep = '')
    fileName <- paste(fullPath,sampName,'_',dateDate,'.csv', sep = '')
    
    #create directory if it doesn't exist
    dir.create(file.path(fullPath), showWarnings = FALSE)
    
    #write CSV
    write.csv(dfAbund,fileName,row.names=F)
  }
}

print('Done! :)')
