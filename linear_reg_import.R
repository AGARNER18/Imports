# Amber Garner
# Oct. 20, 2016

options(scipen=999)

# Find, load, and view sample of cholesterol data
dir()

# Load dataset into variable import
import <- read.csv("imports-85.csv", head=T, sep = ",")

# Find names & locations of all the columns
names(import)

# Remove engine type, make, number of cylinders, and fuel system attributes
import <- import[, -c(3, 15, 16, 18)]

# Verify that attributes were removed
names(import)

# find number of rows with missing values
nrow(import[!complete.cases(import),])

# find number missing in each variable
apply(import, 2, function(import) sum(is.na(import)))

# replace the normalized losses missing values with the mean
import$normalized_losses[is.na(import$normalized_losses)] <- mean(import$normalized_losses, na.rm = TRUE)

# remove 6 remaining rows with missing values
import <- na.omit(import)

# Set random number so can be replicated
set.seed(1234)

# Divide data into training and test sets
ind <- sample(2, nrow(import), replace = TRUE, prob = c(0.7, 0.3))
train.import <- import[ind == 1, ]
test.import <- import[ind == 2, ]

# Formula: price is outcome, all other variables are input
myFormula<-Price~.

# Model the formula using the training set
model<-lm(myFormula, data=train.import)

# See results of model
print(model)

# See summary of model
summary(model)

# Use predict function to evaluate the model using the test data
pred <- predict(model,  newdata=test.import)

# Plot the predicted values against the actual values for the test data
plot(test.import$Price, pred, xlab = "Observed", ylab = "Prediction", main = "Predicted vs. Observed")
abline(a = 0, b = 1)

# Create diagnostic plots
plot (model)

# Use step to remove an independent variable at each step to create minimal model
model2<-step(model, direction="backward")

# View details about minimal model
summary(model2)

#******THE END*******************
