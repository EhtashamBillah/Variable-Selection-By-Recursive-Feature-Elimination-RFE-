
############################################################
# Variable selection: By Recursive Feature Elimination (RFE)
###########################################################

str(nbFuncs)
func<- nbFuncs

index <- createMultiFolds(dataset$diagnosis,times=5)
varsize <- seq(1,30,by=2)
func$summary <- function(...) c(twoClassSummary(...),
                                defaultSummary(...))

varctrl <- rfeControl(method = "repeatedcv",
                      repeats = 5,
                      verbose = TRUE,
                      functions = func,
                      index = index)

set.seed(12345)
nbrfe <- rfe(x=dataset[,-1],
             y=dataset$diagnosis,
             sizes = varsize,
             metric = "ROC",
             rfeControl = varctrl)

nbrfe
opt_var <- predictors(nbrfe) # gives the variables with optimum effect
nbrfe$fit
nbrfe$resample
summary(nbrfe$resample)


#Plotting the result
trellis.par.set(caretTheme())
plot(nbrfe, type = c("g", "o"),
     main= "Number of Variables Vs ROC",
     xlab="Number of Variables",
     col="green")
densityplot(nbrfe)
histogram(nbrfe)

# Taking the subset of variables that gives the optimum ROC with RFE
dataset_rfe <- dataset[,c("diagnosis",opt_var)]


