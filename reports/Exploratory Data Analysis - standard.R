#We assume most of the initial preprocessing is done
train <- train_hp_preprocessed


subset_colclasses <- function(DF, colclasses="numeric") {
  DF[, sapply(DF, function(vec, test) class(vec) %in% test, test=colclasses)]
}

train_int <- subset_colclasses(train, c("integer")) #factor
train_num <- subset_colclasses(train, c("numeric")) 
train_char <- subset_colclasses(train, c("character")) 








plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
}




colPlot <- 2
rowPlot <- 3




totalCol <- ncol(train_char)
totalChartOnPlot <- colPlot * rowPlot
totalPlots <- ceiling(totalCol / totalChartOnPlot)


for (i in 1:totalPlots) { 
  iStart <- i * (totalChartOnPlot - 1) + 1  
  iEnd <- i * totalChartOnPlot
  iEnd <- ifelse(iEnd > totalCol, totalCol, iEnd)
  
  doPlots(train_char, fun = plotHist, ii = iStart:iEnd, ncol = 2)
}

#TODO...

totalCol <- ncol(train_int)
totalChartOnPlot <- colPlot * rowPlot
totalPlots <- ceiling(totalCol / totalChartOnPlot)

DoPlots(train_cont, fun = plotDen, ii = 2:6, ncol = 2)
doPlots(train_cont, fun = plotDen, ii = 7:12, ncol = 2)
doPlots(train_cont, fun = plotDen, ii = 13:17, ncol = 2)




# correlations
correlations <- cor(train_num)

row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
correlations<- correlations[row_indic ,row_indic ]
corrplot(correlations, method="square")
