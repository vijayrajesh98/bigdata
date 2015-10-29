DIRNAME <- "/Users/rvijayaraghavan/Desktop/rajesh/Personal/Teaching/DataDrivenSrikantClass/"
XLFILENAME <- "03 Assignment Workbook - Regression_10-6.xlsx"
FILENAME <- paste0(DIRNAME,XLFILENAME)
library(xlsx)

lecture3data <- read.xlsx(FILENAME, sheetName="Training Data")

nrow(lecture3data) # Number of rows in the dataset
ncol(lecture3data) # Number of columns in the dataset

names(lecture3data) #To see all feature names, i.e., column names. 

lecture3data$Female <- ifelse(lecture3data$Implied.Gender == "F", 1, 0)

lecture3data$Male <- ifelse(lecture3data$Implied.Gender == "M", 1, 0)
lecture3data$Home <- ifelse(lecture3data$Home.Apt..PO.Box == "H", 1, 0)
lecture3data$Apt <- ifelse(lecture3data$Home.Apt..PO.Box == "A", 1,0)

ncol(lecture3data)

reg.model <- lm(PREGNANT ~ Female + Male + Home + Apt + Pregnancy.Test + Birth.Control 
                + Feminine.Hygiene  +  Folic.Acid + Prenatal.Vitamins + Prenatal.Yoga + 
                  Body.Pillow + Ginger.Ale + Sea.Bands + Stopped.buying.ciggies + 
                  Cigarettes + Smoking.Cessation + Stopped.buying.wine + 
                  Wine + Maternity.Clothes, data = lecture3data)

summary(reg.model) #summary of the regression model

coefficients(reg.model) # To get the model coefficients
fit <- fitted(reg.model) # To get the predicted values
res <- residuals(reg.model) # To obtain the residuals

plot(fit) ## Notice the difference.
fit <- sort(fit) ## We sort by the values.
plot(fit) ## store it in fit again.

reg.model <- lm(PREGNANT ~ . -Implied.Gender -Home.Apt..PO.Box , data=lecture3data)

reg.model <- lm(PREGNANT ~ factor(Implied.Gender) + factor(Home.Apt..PO.Box) + 
                  Pregnancy.Test + Birth.Control + Feminine.Hygiene  + Folic.Acid + 
                  Prenatal.Vitamins + Prenatal.Yoga + Body.Pillow + Ginger.Ale + 
                  Sea.Bands + Stopped.buying.ciggies + Cigarettes + Smoking.Cessation + 
                  Stopped.buying.wine + Wine + Maternity.Clothes, data = lecture3data)
