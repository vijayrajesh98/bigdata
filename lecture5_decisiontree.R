install.packages("xlsx")
install.packages("rpart")

library(xlsx) #Loading the package needed to read Excel file
library(rpart) #Loading the package needed to construct and prune trees

DIRNAME <- "/Users/rvijayaraghavan/Desktop/rajesh/Personal/Teaching/DataDrivenSrikantClass/"
XLFILENAME <- "03 Assignment Workbook - Regression_10-6.xlsx"
FILENAME <- paste0(DIRNAME,XLFILENAME)
lecture5data <- read.xlsx(FILENAME, sheetName="Training Data")


lecture5data$Male <- ifelse(lecture5data$Implied.Gender == "M", 1, 0)
lecture5data$Female <- ifelse(lecture5data$Implied.Gender == "F", 1, 0)
lecture5data$Home <- ifelse(lecture5data$Home.Apt..PO.Box == "H", 1, 0)
lecture5data$Apt <- ifelse(lecture5data$Home.Apt..PO.Box == "A", 1,0)

fit <- rpart(PREGNANT ~ Birth.Control + Feminine.Hygiene  +  Folic.Acid + Prenatal.Vitamins ,
             method="class", data=lecture5data)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 

summary(fit) # detailed summary of splits

plot(fit, uniform=TRUE, main="Pregnancy Decision Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.8)



fit <- rpart(PREGNANT ~ Female + Male + Home + Apt + Pregnancy.Test + Birth.Control 
             + Feminine.Hygiene  +  Folic.Acid + Prenatal.Vitamins + Prenatal.Yoga + 
               Body.Pillow + Ginger.Ale + Sea.Bands + Stopped.buying.ciggies + 
               Cigarettes + Smoking.Cessation + Stopped.buying.wine + 
               Wine + Maternity.Clothes,
             method="class", data=lecture5data)


printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 

summary(fit) # detailed summary of splits

plot(fit, uniform=TRUE, main="Pregnancy Decision Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.8)