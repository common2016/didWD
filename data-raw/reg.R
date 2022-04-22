rm(list = ls())
library(pacman)
p_load(readstata13, tidyverse, plm, RStata, devtools, lmtest)
load_all()
# stg6 <- read.dta13('/Users/yangnay/elements/RawE/15_MySummary/异质性DID/Code/staggered_6.dta')
# usethis::use_data(stg6)

stgyr <- 2014:2016
allyr <- 2011:2016
dt <- cbind(stg6, gen_fst(stg6, 'year', allyr), gen_dq(stg6, 'id', 'w', stgyr))

# ---------无协变量估计------------
# 单因素回归
stata('xtset id year
xtreg logy c.d2014#c.f2014 c.d2014#c.f2015 c.d2014#c.f2016 ///
	c.d2015#c.f2015 c.d2015#c.f2016 ///
	c.d2016#c.f2016 ///
	f2014 f2015 f2016, fe vce(cluster id)', data.in = dt)

# 共同趋势检验
stata('xtset id year
xtreg logy c.d2014#c.f2012 c.d2014#c.f2013 c.d2014#c.f2014 c.d2014#c.f2015 c.d2014#c.f2016 ///
	 c.d2015#c.f2012 c.d2015#c.f2013 c.d2015#c.f2014 c.d2015#c.f2015 c.d2015#c.f2016 ///
	 c.d2016#c.f2012 c.d2016#c.f2013 c.d2016#c.f2014 c.d2016#c.f2015 c.d2016#c.f2016 ///
	f2012 f2013 f2014 f2015 f2016, fe vce(cluster id)', data.in = dt)

# 双因素回归
fit <- didWD(stg6, id = 'id', year = 'year', y = 'logy', w = 'w')
coeftest(fit$fit, vcov. = vcovHC, method = 'white2')

fit <- cttest(stg6, id = 'id', year = 'year', y = 'logy', w = 'w')

fit[['fit']] %>% coeftest(vcov. = vcovHC, method = 'white2')

names(coef(fit$fit))


# ----------add X--------------
# 对X组内去均值
stg6$x1_dm4 <- stg6$x1 - mean(stg6$x1[stg6$d2014==1])
stg6$x1_dm5 <- stg6$x1 - mean(stg6$x1[stg6$d2015==1])
stg6$x1_dm6 <- stg6$x1 - mean(stg6$x1[stg6$d2016==1])

stata('
xtset id year
xtreg logy c.w#c.d4#c.f2014 c.w#c.d4#c.f2015 c.w#c.d4#c.f2016 ///
	c.w#c.d5#c.f2015 c.w#c.d5#c.f2016 ///
	c.w#c.d6#c.f2016 ///
	c.w#c.d4#c.f2014#c.x1_dm4 c.w#c.d4#c.f2015#c.x1_dm4 c.w#c.d4#c.f2016#c.x1_dm4 ///
	c.w#c.d5#c.f2015#c.x1_dm5 c.w#c.d5#c.f2016#c.x1_dm5 ///
	c.w#c.d6#c.f2016#c.x1_dm6 ///
	f2014 f2015 f2016 c.f2014#c.x1 c.f2015#c.x1 c.f2016#c.x1, fe vce(cluster id)
      ', data.in = stg6)

plm(logy ~ d2014:f2014 + d2014:f2015 + d2014:f2016 + d2015:f2015 + d2015:f2016 +
      d2016:f2016 + d2014:f2014:x1_dm4 + d2014:f2015:x1_dm4 + d2014:f2016:x1_dm4 + d2015:f2015:x1_dm5 +
      d2015:f2016:x1_dm5 + d2016:f2016:x1_dm6 + f2014:x1 + f2015:x1 + f2016:x1, index = c('id','year'), effect = 'twoways',
    model = 'within', data = stg6) %>% summary()
