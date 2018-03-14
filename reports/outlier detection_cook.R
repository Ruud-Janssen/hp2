H <- House.Prices.Kaggle.preprocessed

#H[770, ]$OverallQual.n = 10
#H[1299, ]$SalePrice = 600000

mod <- lm(SalePriceLog ~ ., data=H)
cooksd <- cooks.distance(mod)


plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 6*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels






# H$isOutlier <- 0
# H[cooksd >0.02, ]$isOutlier <- 1
# 
# ggplot(H, aes(SalePriceLog, TotalSquareFootage, isOutlier, Id, label = ifelse(isOutlier == 1, Id, ""))) + geom_point(aes(alpha = 0.1, color = factor(isOutlier))) + geom_text(aes(size = 2))
# ggplot(H, aes(SalePriceLog, GrLivArea, isOutlier, Id, label = ifelse(isOutlier == 1, Id, ""))) + geom_point(aes(alpha = 0.1, color = factor(isOutlier))) + geom_text(aes(size = 2))
# ggplot(H, aes(SalePriceLog, TotalBaths, isOutlier, Id, label = ifelse(isOutlier == 1, Id, ""))) + geom_point(aes(alpha = 0.1, color = factor(isOutlier))) + geom_text(aes(size = 2))
# ggplot(H, aes(SalePriceLog, TotalBsmtSpace, isOutlier, Id, label = ifelse(isOutlier == 1, Id, ""))) + geom_point(aes(alpha = 0.1, color = factor(isOutlier))) + geom_text(aes(size = 2))
# ggplot(H, aes(SalePriceLog, OverallCond.n, isOutlier, Id, label = ifelse(isOutlier == 1, Id, ""))) + geom_point(aes(alpha = 0.1, color = factor(isOutlier))) + geom_text(aes(size = 2))
# ggplot(H, aes(SalePriceLog, OverallQual.n, isOutlier, Id, label = ifelse(isOutlier == 1, Id, ""))) + geom_point(aes(alpha = 0.1, color = factor(isOutlier))) + geom_text(aes(size = 2))
# 
# r
# ggplot(rawdata, aes(SaleCondition)) + geom_bar()


