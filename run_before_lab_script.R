# Install all of the following packages
install.packages("sjPlot")
install.packages("sjmisc")
install.packages("sjlabelled")
install.packages("nlme")
install.packages("lme4")
install.packages("brms")
install.packages("tidyverse")
install.packages("plotly")

# Last you need to make sure you have a C++ compiler
# you can try running the following model

library('brms')
data("epilepsy")
fit <- brm(count ~ 1 + (1|patient), data=epilepsy)
summary(fit)
# if you do not get any warnings fitting that model, then you should be fine.
# If you did get warnings, then you likely need a c++ compiler
# Mac: follow instructions here: https://github.com/stan-dev/rstan/wiki/RStan-Mac-OS-X-Prerequisite-Installation-Instructions#step-2--install-c-toolchain
# Windowns: follow instructions here: https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Windows#toolchain
# You can also poke around the Stan Help pages: http://mc-stan.org/users/interfaces/rstan.html

