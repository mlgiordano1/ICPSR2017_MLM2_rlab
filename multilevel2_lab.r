## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
require("knitr")
opts_knit$set(root.dir ="C:\\Users\\mgiordan\\git\\Multilevel_II_Rworkshop_ICPSR2017")

## ---- include = FALSE----------------------------------------------------
# used for loading stata dataset
# pulled this out...put it back?
#library('haven')

## ----libraries, message=FALSE, warning=FALSE-----------------------------
# Fits multilevel models
library("nlme")
# Also fits multilevel models
library("lme4")
# Adds p-values to lme4
library("lmerTest")
# Used for algorithms
library('optimx')
# bootstrapping
library("boot")
# Useful for ploting
library("sjPlot")
library("sjmisc")
library("sjlabelled")
# Canned Rstan models (needed for the multiple membership example)
library("brms")
# tidyverse
library("tidyverse")

## ----paths, eval=FALSE---------------------------------------------------
## # Enter the folder path on your computer
## setwd('C:/enter/the/folderpath/on/your/computer')

## ----linear_read_data, cache = TRUE--------------------------------------
# Reading in the data
chapman <- readRDS("longitudinal_chapman.rds")
# Checking the dataset
dim(chapman)


## ----linear_look_data, cache = TRUE, echo = FALSE------------------------
# Set a seed and randomly sample from rows 1-1000.
# The random sample here isn't important, only useful for displaying data.
set.seed(872017)
rand <- sample(unique(chapman$id),2)
# Print the randomly sampled rows
kable(head(chapman[chapman$id %in% rand,], n = 10))

## ----Linear_plot_slopes, cache= TRUE-------------------------------------
# Randomly sample 10 id's
set.seed(111111111)
rand <-sample(unique(chapman$id), 10)

# Plot those 10 id's individually over time. 
ggplot(data = chapman[chapman$id %in% rand,], aes(x=week, y = score)) +
  geom_point() +
  ylim(50, 300)+
  geom_smooth(method='lm',formula=y~x) +
  facet_wrap("id")


## ----longitudinal_reanova, cache=TRUE------------------------------------
# Fitting the model
fit_m0 <- lme4::lmer(score ~ 1 + (1 | id),
					 data      = chapman,
					 REML      = FALSE,
					 na.action = na.omit)

# Printing the model summary
summary(fit_m0)

# Printing the ICC
tau2  	  <- 574.3
sigma2    <- 1583.7
ICC 	  <- tau2/(sigma2 +tau2)
paste("The ICC is:", round(ICC, 3))

## ---- cache = TRUE-------------------------------------------------------
# Fitting a conditional model
fit_m1 <- lme4::lmer(score ~ 1 + week + (1 | id),
					 data      = chapman,
					 REML      = FALSE,
					 na.action = na.omit)
# summarize model
summary(fit_m1)

## ----standard_errs, cache=TRUE-------------------------------------------
# Bootstrap Standard errors with the 'boot' library
b_par<-bootMer(x=fit_m1,FUN=fixef,nsim=500)
boot.ci(b_par,type="perc", index=1)
boot.ci(b_par,type="perc", index=2)

# Or we could bootstrap with the 'confint' function
confint.merMod(fit_m1, parm=c(3,4), method = "boot", nsim = 500, boot.type = "perc")

# Or we could fit the model with "lmerTest::lmer()" 
# which uses the same syntax as 'lmer()'
fit_m1 <- lmerTest::lmer(score ~ 1 + week + (1 | id),
						 data      =chapman,
						 REML      = FALSE,
						 na.action = na.omit)
# summarize model
summary(fit_m1)

## ------------------------------------------------------------------------
# Adding a random slope for week
fit_m2 <- lme4::lmer(score ~ 1 + week + (1 + week | id),
					 data      = chapman,
					 REML      = FALSE,
					 na.action = na.omit)
summary(fit_m2)

# LR test for comparing model 1 vs model 2
anova(fit_m1, fit_m2)

## ---- cache = TRUE-------------------------------------------------------
# Plotting the random intercepts and slopes
sjp.lmer(fit_m2, 
		 type       = "re",
		 facet.grid = FALSE,
		 #sort.est  = "sort.all",
		 y.offset   = .4)
sjp.lmer(fit_m2, 
		 type       = "re",
		 facet.grid = FALSE,
		 sort.est   = "sort.all",
		 y.offset   = .4)

# Pulling out the random intercepts
ranef(fit_m2)

# Plot random slopes and random intercepts simultaneously
sjp.lmer(fit_m2, 
		 type = "rs.ri")

## ----plotly1, cache = TRUE-----------------------------------------------
# adding in the predicted scores
chapman <- as.data.frame(cbind(chapman, predict(fit_m1), predict(fit_m2)))

# Plotting m1 with fixed slopes
p1 <- ggplot(data =chapman, aes(x=week, y=predict(fit_m1), 
                       		   color = factor(id), group = factor(id))) +
  	  geom_point() +
      geom_line()
# Plotting m2 with randome slopes
p2 <-ggplot(data =chapman, aes(x=week, y=predict(fit_m2), 
                       		   color = factor(id), group = factor(id))) +
  			geom_point() +
  		 	geom_line()

plotly::ggplotly(p1)
plotly::ggplotly(p2)

## ----uncor_ranef, cache=TRUE---------------------------------------------
fit_m2 <- lmer(score ~ 1 + week + (1 | id) + (-1 + week|id),
			   data      = chapman,
			   REML      = FALSE,
			   na.action = na.omit)
summary(fit_m2)


## ----optimizers, cache=TRUE----------------------------------------------
# Model with default optimizer
fit_m2 <- lmer(score ~ 1 + week + (1 + week | id),
  			   data      = chapman,
  			   REML      = FALSE,
  			   na.action = na.omit,
  			   control   = lmerControl(optimizer = "bobyqa"))
summary(fit_m2)

# adding the 'nloptwrap' optimizer
fit_m2 <- lmer(score ~ 1 + week + (1 + week | id),
			   data      = chapman,
			   REML      = FALSE,
			   na.action = na.omit,
			   control   = lmerControl(optimizer = "nloptwrap"))
summary(fit_m2)

# The 'optimx' package has more optimzers. Here I use the 'nlminb' optimizer
fit_m2 <- lmer(score ~ 1 + week + (1 + week | id),
			   data       = chapman,
			   REML       = FALSE,
			   na.action  = na.omit,
			   control    = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
			   							optCtrl    = list(method = "nlminb", 
			   							starttests = FALSE, kkt = FALSE)))
summary(fit_m2)


## ------------------------------------------------------------------------
# Fitting the same random slopes model - NO autocorrelation
# Note that I have specified missing data actions and an optimizer
fit_m2_nlme <- lme(score ~ 1 + week, random = ~ 1+week|id,
					data      = chapman,
					na.action ="na.omit",
					control   = lmeControl(opt='optim')) # defaults to 'nlminb'
summary(fit_m2_nlme)

# Adding an autoregressive process of order 1.
fit_m2AR_nlme <- lme(score ~ 1 + week, random = ~ 1+week|id,
					 data        =chapman,
					 na.action   ="na.omit",
					 control     = lmeControl(opt='optim'),
					 correlation = corAR1())
summary(fit_m2AR_nlme)
# adding an autoregressive moving average process
# p and q arguments are arbitrary here. I'd think more deeply about
# how you want to specify those
fit_m2AR_nlme <- lme(score ~ 1 + week, random = ~ 1+week|id,
					 data        = chapman,
					 na.action   ="na.omit",
					 control     = lmeControl(opt='optim'),
					 correlation = corARMA(p=1,q=1))
summary(fit_m2AR_nlme)

#Does the ARMA autocorrelation improve model fit?
anova(fit_m2_nlme, fit_m2AR_nlme)



## ----bayes_noargsshown, eval=FALSE---------------------------------------
## # Fitting the brms model
## fit_m0b <- brm(score ~ 1 + week + (1 + week | id),
##                data = chapman)

## ----longitudinal_brms, cache = TRUE-------------------------------------
# Fitting the brms model
fit_m2b <- brm(score ~ 1 + week + (1 + week | id), 
			   data    = chapman,
			   prior   = NULL,
			   autocor = NULL,
			   chains  = 4,
			   iter    = 2000,
			   warmup  = 1000)
summary(fit_m2b)

## ---- cache=TRUE---------------------------------------------------------
# plotting paramters
plot(fit_m2b)
stanplot(fit_m2b)

## ----corplot, cache=TRUE-------------------------------------------------
stanplot(fit_m2b, pars = "cor_id")


## ----longitudinal_brms_withprior, cache = TRUE---------------------------
# Fitting the brms model
fit_m2b_priors <- brm(score ~ 1 + week + (1 + week | id),
          			  data    = chapman,
          			  prior   = set_prior(prior = "normal(0,10)",
          			                      class = "b"),
          			  autocor = NULL,
          			  chains  = 4,
          			  iter    = 2000,
          			  warmup  = 1000
          			  )

## ---- cache=TRUE---------------------------------------------------------
summary(fit_m2b_priors)
# plotting paramters
plot(fit_m2b_priors)
stanplot(fit_m2b_priors)


## ----bayes_autocorrelation, cache=TRUE-----------------------------------
# Fitting the brms model
fit_m2b_AR <- brm(score ~ 1 + week + (1 | id),
          	      data    = chapman,
          	      prior   = set_prior(prior = "normal(0,10)",
          	                          class = "b"),
          	      autocor = cor_ar(~week|id, p=1), # Adding an AR 1 process here
          	      chains  = 4,
          	      iter    = 2000,
          	      warmup  = 1000
          	      )


## ----nonlinear_read_data-------------------------------------------------
# Reading in the data
thai <- readRDS('binary_thai.rds')
# Checking the dataset
dim(thai)

## ----nonlinear_look_data, cache = TRUE, echo = FALSE---------------------
# Set a seed and randomly sample from rows.
# The random sample here isn't important, only useful for displaying data.
set.seed(81111)
rand <- sample(unique(thai$schoolid),1)
temp <- thai[thai$schoolid %in% rand,]

# Print the randomly sampled rows
kable(temp[sample(nrow(temp), 10),])
  

## ---- cache=TRUE---------------------------------------------------------
# Fitting a null model
fit_m0 <- glmer(pped ~ 1+ (1 |schoolid),
				family                 = "binomial", 
				data                   = thai,
				glmerControl(optimizer = "bobyqa"))
summary(fit_m0)

# Adding predictors and random slopes
fit_m1 <- glmer(pped~1+male+ msesc+ (1+male|schoolid),
				family                 = "binomial", 
				data                   = thai,
				glmerControl(optimizer ="bobyqa"))
summary(fit_m1)

#putting confidence intervals around effects
confint(fit_m1, parm = "beta_", method="Wald") 
# Odds ratios instead of log-odds
exp(fixef(fit_m1))


## ---- cache = TRUE-------------------------------------------------------
fit <-glmer(pped~1+male+msesc +(1|schoolid),
			family                 = "binomial", 
			data                   = thai,
			nAGQ                   = 5, 
			glmerControl(optimizer ="bobyqa"))

## ----eval = FALSE, cache = TRUE------------------------------------------
## fit <- glmer(pped~1+male+msesc +(1+male|schoolid),
## 			 family                 ="binomial",
## 			 data                   =thai,
## 			 nAGQ                   = 5,
## 			 glmerControl(optimizer ="bobyqa"))

## ---- cache=TRUE---------------------------------------------------------
fit <- glmer(pped~1+male+msesc +(1+male|schoolid),
			 family       ="binomial", 
			 data         =thai,
			 nAGQ         = 1, 
			 glmerControl = lmerControl(optimizer = "nloptwrap"))


## ---- cache=TRUE, warning=FALSE------------------------------------------
fit <- brm(pped ~ 1 + male + msesc + (1|schoolid),
		   family = "binomial", 
		   data   = thai,
		   chains = 2)
summary(fit)

plot(fit)
stanplot(fit)

## ------------------------------------------------------------------------
data("Penicillin")
dim(Penicillin)
head(Penicillin)

## ----pen_plot ,cache = TRUE----------------------------------------------
p3 <- ggplot(data = Penicillin, aes(y=diameter, x=plate, color = sample, group = sample)) +
       geom_point()+
       geom_line()

plotly::ggplotly(p3)


## ---- cache=TRUE---------------------------------------------------------
fit <- lme4::lmer(diameter ~ 1 + (1|plate) + (1|sample),
				  data      = Penicillin,
				  REML      = FALSE,
				  na.action = na.omit,
				  control   = lmerControl(optimizer = "bobyqa"))
summary(fit)

## ---- cache=TRUE---------------------------------------------------------
fit <- brm(diameter ~ 1 + (1|plate) + (1|sample),
			data   = Penicillin,
			warmup = 5000,
			iter   = 10000)
summary(fit)
plot(fit)
stanplot(fit)

## ---- cache=TRUE---------------------------------------------------------
fit <- brm(diameter ~ 1 + (1|plate) + (1|sample),
			data    = Penicillin,
			warmup  = 5000,
			iter    = 10000,
			control = list(adapt_delta = .99))
summary(fit)
plot(fit)
stanplot(fit)

## ---- cache = TRUE-------------------------------------------------------
cross <- read.csv("C:/users/mgiordan/desktop/crossclassified.csv")
dim(cross)

kable(head(cross))

## ------------------------------------------------------------------------
fit <- lme4::glmer(outcome ~ 1 + (1|state) + (1|policy),
				   family = "binomial",
				   data   = cross)
summary(fit)

fit <- lme4::glmer(outcome ~ 1 + time + (1|state) + (1|policy),
				   family = "binomial",
				   data   = cross)
summary(fit)

## ---- eval=FALSE---------------------------------------------------------
## fit <- lme4::glmer(outcome ~ 1 + citi_ideo + legprofess + popdens +
##                    diffus + ideodist + ideo_zero + dem + time +
##                    (1|state) + (1|policy),
## 				   family = "binomial",
## 				   data   = cross)

## ---- eval = FALSE-------------------------------------------------------
## # poisson
## fit <- lme4::glmer(y ~ 1 + x1 + x2 + (1 | groupvar),
##                    family = "poisson",
##                    data = mydata)
## 

## ---- eval = FALSE-------------------------------------------------------
## # Negative binomial
## fit <- lme4::glmer.nb(y ~ 1 + x1 + x2 + (1 | groupvar)
##                       data = mydata)
## 

## ----read_nurses---------------------------------------------------------
# Reading in the data
nurses <- readRDS("mm_nursing.rds")

## ------------------------------------------------------------------------
# Checking the dataset
dim(nurses)

## ------------------------------------------------------------------------
nurses <- nurses[, c("patient", "satis", "assess", 
                     "n1st", "n2nd", "n3rd", "n4th",
                     "p1st", "p2nd", "p3rd", "p4th")]

## ------------------------------------------------------------------------
# Set a seed and randomly sample from rows 1-1000.
# The random sample here isn't important, only useful for displaying data.
set.seed(872017)
randomRows <- sample(1:1000,10)
# Print the randomly sampled rows
kable(head(nurses[randomRows,], n = 10))

## ----fitting_brm, cache=TRUE---------------------------------------------
# model syntax
# This will spit out lots of information about the model fitting process
fit_mm <- brm(satis ~ 1 + assess + (1 | mm(n1st, n2nd, n3rd, n4th, 
                                  		   weights = cbind(p1st, p2nd, p3rd, p4th))),
			  data   = nurses,
			  warmup = 5000,
			  iter   = 10000)
# Printing the model parameters
summary(fit_mm)

## ---- cache=TRUE---------------------------------------------------------
# Plotting results
plot(fit_mm)
stanplot((fit_mm))

