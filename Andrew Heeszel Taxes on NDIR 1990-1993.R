#The following code will read the dataset we will use and prepare it for analysis. We will include a new categorical variabe
#indicating if the state is in either the West or Northeast



di <- read.table("http://pages.uoregon.edu/dlevin/DATA/DI.txt", header=T,row.names=1,sep="\t")



di[45,12] <- "West"
di$IsWoNE <- NA



for (i in 1:48){
  if (xor(di[i,12] == "West", di[i,12] == "Northeast")){
    di[i,13] <- "T"
  }
  if (di[i,12] != "West" & di[i,12] != "Northeast"){
    di[i,13] <- "F"
  }
}

#We will now regress NDIR vs all other variables of the dataset, perform a T-test on the fitted coefficients, and 
#F-test on the residuals from western states and non-western states to test for homoskedasticity within the model

model_1 <- lm(NDIR~., data = di)
summary(model_1)

west_resid <- c()
south_resid <- c()
midwest_resid <- c()
northeast_resid <- c()


for (i in 1:48){
  if (di[i,12] == "West"){
    west_resid <- c(west_resid, model_1$residuals[i])
  }
  if (di[i,12] == "South"){
    south_resid <- c(south_resid, model_1$residuals[i])
  }
  if (di[i,12] == "Midwest"){
    midwest_resid <- c(midwest_resid, model_1$residuals[i])
  }
  if (di[i,12] == "Northeast"){
    northeast_resid <- c(northeast_resid, model_1$residuals[i])
  }
}

var.test(west_resid, c(northeast_resid, south_resid, midwest_resid))

#The plot below gives box plots for the residuals between each geographical region

plot(model_1$residuals~Region, data = di, ylab = "Residuals")


#We will additionally test if the residuals from the northeast and south share the same variance


var.test(northeast_resid, south_resid, alternative = "two.sided")




#We will now plot NDIR vs Taxes by region including best fit lines for each region


plot(NDIR~Taxes, data = di, col = Region, pch = 19)
label = c("Midwest", "Northeast", "South", "West")
legend('topright', pch=19, legend = label, col = c("black", "red", "green", "blue"), cex = 0.7)
west_line <- lm(NDIR~Taxes, data = subset(di, Region == "West"))
abline(west_line, col = "blue")
south_line <- lm(NDIR~Taxes, data = subset(di, Region == "South"))
abline(south_line, col = "green")
midwest_line <- lm(NDIR~Taxes, data = subset(di, Region == "Midwest"))
abline(midwest_line)
northeast_line <- lm(NDIR~Taxes, data = subset(di, Region == "Northeast"))
abline(northeast_line, col = "red")


#We will now perform feasible weighted least squares assuming hetroskedasticity among all regions aside from the south 
#and northeast. We will additoinally remove Wage, Metrop, Educ, BusFail, and Temp as regressors

#We will begin by partitioning our dataset by geographic region. We will group the northeast and south together.



west_data <- subset(di, Region == "West")
midwest_data <- subset(di, Region == "Midwest")
ne_s_data <- subset(di, xor(Region == "Northeast", Region == "South")) 


#We will now obtain the standard error from the mean when sharing the same regressors as the previous Ols model to obtain our 
#weights


west_model <- lm(NDIR~ Crime+Poor+Income+Taxes, data = west_data)
midwest_model <- lm(NDIR~ Crime+Poor+Income+Taxes, data = midwest_data)
ne_s_model <- lm(NDIR~ Crime+Poor+Income+Region+Taxes+Taxes:IsWoNE, data = ne_s_data)



#The following code forms the appropriate weights for the wls model

h <- c( (1/summary(west_model)$sigma)^2, (1/summary(midwest_model)$sigma)^2, (1/summary(ne_s_model)$sigma)^2)

w <- c()

for (i in 1:48){
  if (di[i,12] == "West"){
    w <- c(w, h[1])
  }
  if (di[i,12] == "Midwest"){
    w <- c(w, h[2])
  }
  if (xor(di[i,12] == "South", di[i,12] == "Northeast")){
    w <- c(w, h[3])
  }
}

#We will now fit a weighted least squares model including Taxes as a categorical variable to a state being located in either the
#West or Northeast using the weights we just obtained and plot the residuals by region.
#We will then plot the residuals by fitted values, and give a histogram of residual values.

model_2 <- lm(NDIR~ Crime+Poor+Income+Region+Taxes+Taxes:IsWoNE, data = di, weights = w)

plot(weighted.residuals(model_2)~Region, data = di, ylab = "Weighted Residuals")

plot(weighted.residuals(model_2)~ model_2$fitted.values, xlab = "Fitted Values", ylab = "Weighted Residuals")
histogram(weighted.residuals(model_2), xlab = "Weighted Residuals", col = "white")


#We will now perform a T-test on the fitted coefficients of the model, and an F-test for the inclusion of both Taxes, and 
#Taxes as a categorical variable of being either a western or northeastern state

summary(model_2)

model_3 <- lm(NDIR~ Crime+Poor+Income+Region, data = di, weights = w)

model_4 <- lm(NDIR~ Crime+Poor+Income+Region+Taxes, data = di, weights = w)

anova(model_3, model_4)

anova(model_4, model_2)


#We will now test if the intercept for the midwest is equal to the intercept for northeast

is_ne <- c()
is_s <- c()
is_w <- c()

for (i in 1:48){
  if (di[i,12] == "West"){
    is_ne <- c(is_ne, 0)
    is_s <- c(is_s, 0)
    is_w <- c(is_w, 1)
  }
  if (di[i,12] == "South"){
    is_ne <- c(is_ne, 0)
    is_s <- c(is_s, 1)
    is_w <- c(is_w, 0)
  }
  if (di[i,12] == "Midwest"){
    is_ne <- c(is_ne, 0)
    is_s <- c(is_s, 0)
    is_w <- c(is_w, 0)
  }
  if (di[i,12] == "Northeast"){
    is_ne <- c(is_ne, 1)
    is_s <- c(is_s, 0)
    is_w <- c(is_w, 0)
  }
}

one_minus_ne <- 1 - is_ne

model_5 <- lm(NDIR~ Crime+Poor+Income+one_minus_ne+ is_s + is_w+Taxes+Taxes:IsWoNE-1, data = di, weights = w)


anova(model_5, model_2)


