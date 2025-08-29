View(bedford)

library(tidyverse)
library(haven)
library(ggplot2)
library(mgcv)
library(gridExtra)
library(reshape2)

#Exploratory analysis-----

#observing some of the variables
dim(bedford)
summary(bedford$price)
summary(bedford$tfarea)
summary(bedford$numberrooms)

unique(bedford$propertytype)

grid.arrange(
  #histogram of houseprices
  ggplot(bedford, aes(x=price)) + geom_histogram(),
  #histogram of floor area
  ggplot(bedford, aes(x=tfarea)) + geom_histogram(),
  #histogram of number of rooms
  ggplot(bedford, aes(x=numberrooms)) + geom_bar(),
  #histogram of energy efficiency
  ggplot(bedford, aes(x=CURRENT_ENERGY_EFFICIENCY)) + geom_histogram(),
  #histogram of potential energy efficieny
  ggplot(bedford, aes(x=POTENTIAL_ENERGY_EFFICIENCY)) + geom_histogram(),
  #barchart of towns
  ggplot(bedford, aes(x=towncity)) + geom_bar(),
  #barchart of year
  ggplot(bedford, aes(x=year)) + geom_bar()
)

#creating log variables as the original variables are skewed
logprice <- log(bedford$price)
logarea <- log(bedford$tfarea)

#renaming energy efficiency variables as the current names are too long
eff <- bedford$CURRENT_ENERGY_EFFICIENCY
poteff <- bedford$POTENTIAL_ENERGY_EFFICIENCY

#converting the year variable to a factor
bedford$year <- as.factor(bedford$year)

#plotting the relationships between the explantory variables and the response
grid.arrange(
  #scatterplot of price and floor area
  ggplot(bedford, aes(x=logarea, y=logprice)) + geom_point(),
  #scatterplot of price and no. of rooms
  ggplot(bedford, aes(x=numberrooms, y=price)) + geom_point(),
  #scatterplot of price and energy efficiency
  ggplot(bedford, aes(x=eff, y=price)) + geom_point(),
  #scatterplot of price and potential energy efficiency
  ggplot(bedford, aes(x=poteff, y=price)) + geom_point(),
  #boxplot of towncity and price
  ggplot(bedford, aes(x=towncity, y=price)) + geom_boxplot(),
  #boxplot of price and year of sale
  ggplot(bedford, aes(x=year, y=price)) + geom_boxplot()
)

#Model fitting----------

#fitting a linear model
model1 <- gam(logprice ~ logarea + numberrooms + eff + poteff 
              + towncity + year + propertytype, data=bedford, 
            family = gaussian(link="identity"))

summary(model1)

#observing the fit
qq.gam(model1, rep=100)

#checking the residuals of the continuous variables
bedford$residuals <- residuals(model1, type="response")

grid.arrange(
  ggplot(bedford, aes(x=logarea, y=residuals)) + geom_point() + geom_smooth(),
  ggplot(bedford, aes(x=numberrooms, y=residuals)) + geom_point() + geom_smooth(),
  ggplot(bedford, aes(x=eff, y=residuals)) + geom_point() + geom_smooth(),
  ggplot(bedford, aes(x=poteff, y=residuals)) + geom_point() + geom_smooth()
)

#trying a negative binomial
model2 <- gam(price ~ logarea + numberrooms + eff + poteff + towncity + year + propertytype, data=bedford, 
              family = "nb")

summary(model2)

qq.gam(model2, rep=100)

#plotting residuals against all continuous variables
bedford$residuals <- residuals(model2, type="response")

grid.arrange(
  ggplot(bedford, aes(x=logarea, y=residuals)) + geom_point() + geom_smooth(),
  ggplot(bedford, aes(x=numberrooms, y=residuals)) + geom_point() + geom_smooth(),
  ggplot(bedford, aes(x=eff, y=residuals)) + geom_point() + geom_smooth(),
  ggplot(bedford, aes(x=poteff, y=residuals)) + geom_point() + geom_smooth()
)

#plotting another model with polynomial terms to deal with patterns
model3 <- gam(price ~ poly(logarea, 4) + numberrooms + eff + poly(poteff, 5) + towncity + year + propertytype, data=bedford, 
              family = "nb")

summary(model3)

qq.gam(model3, rep=100)

bedford$residuals <- residuals(model3, type="response")

grid.arrange(
  ggplot(bedford, aes(x=logarea, y=residuals)) + geom_point() + geom_smooth(),
  ggplot(bedford, aes(x=numberrooms, y=residuals)) + geom_point() + geom_smooth(),
  ggplot(bedford, aes(x=eff, y=residuals)) + geom_point() + geom_smooth(),
  ggplot(bedford, aes(x=poteff, y=residuals)) + geom_point() + geom_smooth()
)

#residual deviance
ggplot(mapping=aes(x=bedford$price,
                   y=residuals(model5,type='deviance'))) + 
  geom_point() + geom_smooth()+
  labs(x='House prices',y='Deviance residuals',
       title='Model 2')

#comparing the AIC values for the two negative binomial models
AIC(model2)
AIC(model3)

#assessing the residual deviance
pchisq(model3$deviance/summary(model3)$dispersion, model3$df.residual, lower.tail = FALSE)


#inference-----

#making predictions for the whole dataset
bedfordpreds1 <- predict(model3, bedford, se.fit = TRUE, type = "terms")

#finding the 95% confidence intervals for every prediction
bedfordpreds1$lower <- bedfordpreds1$fit - 1.96*bedfordpreds1$se.fit
bedfordpreds1$upper <- bedfordpreds1$fit + 1.96*bedfordpreds1$se.fit

#adding these to new columns in the prediction dataframe
colnames(bedfordpreds1$fit) <- paste(colnames(bedfordpreds1$fit),'fit',sep='.')
colnames(bedfordpreds1$lower) <- paste(colnames(bedfordpreds1$lower),'lower',sep='.')
colnames(bedfordpreds1$upper) <- paste(colnames(bedfordpreds1$upper),'upper',sep='.')

bedfordpreds_full <- bedford %>% 
  cbind(as.data.frame(bedfordpreds1$fit)) %>%
  cbind(as.data.frame(bedfordpreds1$lower)) %>%
  cbind(as.data.frame(bedfordpreds1$upper))

#plotting the polynomial effect of floor area on house price
ggplot(bedfordpreds_full) + 
  geom_ribbon(aes(x=tfarea, 
                  ymin=exp(`poly(logarea, 4).lower`), 
                  ymax=exp(`poly(logarea, 4).upper`), alpha=0.5,)) + 
  geom_line(aes(x=tfarea, y=exp(`poly(logarea, 4).fit`))) +
  geom_hline(yintercept = 1, linetype=2) +
  labs(x="Total floor area of a house",
       y="Polynomial effect on house price")

ggplot(bedfordpreds_full) + 
  geom_ribbon(aes(x=tfarea, 
                  ymin=`poly(logarea, 4).lower`, 
                  ymax=`poly(logarea, 4).upper`, alpha=0.5)) + 
  geom_line(aes(x=tfarea, y=`poly(logarea, 4).fit`)) +
  geom_hline(yintercept = 1, linetype=2) +
  labs(x="Total floor area of a house",
       y="Polynomial effect on house price")

#plotting the effect of energy efficiency on house price
ggplot(bedfordpreds_full) +
  geom_ribbon(aes(x=eff, 
                  ymin=`eff.lower`, 
                  ymax=`eff.upper`, alpha=0.5)) + 
  geom_line(aes(x=eff, y=`eff.fit`)) +
  labs(x="Energy efficiency rating", y="Effect of energy efficiency on house prices")

#plotting different house pricing effects by area

ggplot(bedfordpreds_full %>% select(towncity, towncity.lower, towncity.fit, towncity.upper) %>% distinct(),
       aes(x=towncity, y=towncity.fit, ymin=towncity.lower, ymax=towncity.upper, label=round(towncity.fit,2))) +
  geom_errorbar() + geom_label() +
  labs(x="Town/City", y="Effect on house prices")



