rm(list = ls())
load('/Users/yangnay/elements/RawE/17_HuaDong/research/AI/RCodeEctrics/data-raw/regdata.rdata') # From TFP.R
library(pacman)
p_load(didWD,lmtest,plm, reshape2, devtools, ggplot2, tidyverse, matlab, did)
load_all()

regdata$id <- paste(regdata$indus, regdata$iso2c, sep = '_')
regdata$w <- 0
regdata$w[regdata$operationalstock > 100] <- 1

cn <- regdata[regdata$iso2c %in% c('CN') & regdata$year <= 2014,]

fit <- didWD(cn, 'id', 'year', y = 'TFP', w = 'w')
ans <- fit$data
