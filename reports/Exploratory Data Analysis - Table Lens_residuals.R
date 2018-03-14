# Plotting all data using tabplots
library(tabplot)


#Function to return the column numbers in the right order in a vector
getVectorOfColumnsToPlot <- function(i, totalNrOfColVisible, totalCol, sortColNr) {
  startCol <- (i - 1) * (totalNrOfColVisible - 1) + 1
  endCol <- i * (totalNrOfColVisible - 1)
  endCol <- ifelse(endCol > totalCol, totalCol, endCol)
  
  if(between(sortColNr, startCol, endCol) == T) {
    selColNames <- c(startCol:endCol)
    selColNames <- selColNames[-which(selColNames == sortColNr)]
    selColNames <- append(selColNames, sortColNr)
  } else {
    selColNames <- c(startCol:endCol, sortColNr)
  }
  return (selColNames)
}

trainResults

# Initialize variables
totalNrOfColVisible <- 6    # total nr of columns in a set of table lenses
nBins <- 80                 # nrOfBins, depend on data size
sortCol <- "xgbResidial"   # sort column

# Save images in folder under graphics
fileDirPlot <- "graphs//eda//table lens"
fileNamePlot <- paste0("HousePricesKaggleResid_", sortCol) 

totalCol <- ncol(trainResults)
sortColNr <- as.numeric(which( colnames(trainResults) == sortCol))
totalPlots <- ceiling((totalCol - 1) / (totalNrOfColVisible - 1.0))

#Make a copy of the data and remove it all afterwards
data <- trainResults

#use factors
for (i in 1:totalCol) {
  if (typeof(data[, i]) == "character") {
    data[is.na(data[, i]), i] <- ""
    data[, i] <- as.factor(data[, i])
  }
}

for (i in 1 : totalPlots) {
  selColNr <- getVectorOfColumnsToPlot (i, totalNrOfColVisible, totalCol, sortColNr)
  #selColNrWithoutSortColumn <- selColNr[-which(selColNr == sortColNr)] #for title name
  
  selColNames <- colnames(data)[selColNr]
  #selColNamesWithoutSortColumn <- colnames(data)[selColNrWithoutSortColumn]  #for title name
  
  totalNrOfColVisibleInPlot <- length(selColNames)
  
  pdf(paste0(fileDirPlot, "//", fileNamePlot, as.character(i),".pdf"))
    
    plot(
      tableplot(data, select_string = selColNames, sortCol = totalNrOfColVisibleInPlot, nBins = nBins, plot = F),
      fontsize = 10#,
      #title = paste0("log(SalePrice) vs ", paste(selColNamesWithoutSortColumn, collapse = "+")),
      #showTitle = TRUE,
      #fontsize.title = 12
    )
  dev.off()
}

rm("data", "nBins", "totalNrOfColVisibleInPlot", "totalNrOfColVisible", "sortCol", "sortColNr", "totalCol")
rm("selColNames", "selColNr","totalPlots", "i")
#rm("selColNamesWithoutSortColumn", "selColNrWithoutSortColumn")
rm("getVectorOfColumnsToPlot", "fileDirPlot", "fileNamePlot")