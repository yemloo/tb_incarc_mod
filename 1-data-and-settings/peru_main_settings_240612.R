#-------------------------------------------
# Author: Yiran Liu
# Date Modified: 6.26.24
# Country: Peru
# Description: This script also contains the fixed settings/assumptions for each
# country, namely those involving when time-varying parameters change, and which
# parameters are calibrated.
#-------------------------------------------

#####################################################
######## country-specific settings to adjust ########
#####################################################
country_p='Peru'
quasi_2000_tb=1000 # to make calibration more efficient (if we can achieve equilibrium sooner)

all.settings <- list(
  ####### incarceration-related settings #######
  change.incarc.start1=1995,
  change.incarc.end1=1999,
  change.incarc.start2=1999,
  change.incarc.end2=2003,
  change.incarc.start3=2011,
  change.incarc.end3=2014,
  change.incarc.start4=Inf,
  change.incarc.end4=Inf,
  change.r.start1=2001,
  change.r.end1=2007,
  change.r.start2=2009,
  change.r.end2=2014,
  covid.start=2020.25,
  covid.end=2023,
  covid.change.r=FALSE,
  state.emerg.start=Inf,
  state.emerg.end=Inf,
  intrvn.start=Inf,
  intrvn.end=Inf,
  intrvn.iE.factor=1,
  intrvn.iR.factor=1,
  intrvn.r.factor=1,
  ####### TB-related settings #######
  change.beta.start1=1990,
  change.beta.end1=2000,
  change.beta.start2=2000,
  change.beta.end2=2020,
  change.beta.2.factor=0.5,
  change.beta.start3=Inf,
  change.beta.end3=Inf,
  change.beta.3.factor=1,
  change.prog.start1=Inf,
  change.prog.end1=Inf,
  change.prog.start2=Inf,
  change.prog.end2=Inf,
  change.prog.2.factor=1,
  change.d.start1=1990,
  change.d.end1=2020,
  change.d.start2=Inf,
  change.d.end2=Inf,
  change.d.2.factor=1,
  change.d1.start1=Inf,
  change.d1.end1=Inf,
  change.d1.temp=FALSE,
  change.d1.exp=FALSE,
  spike.d1.start1=Inf,
  spike.d1.end1=Inf,
  spike.d1.factor=NA,
  change.betap.start1=Inf,
  change.betap.end1=Inf,
  change.betap.exp=FALSE,
  change.betap.start2=Inf,
  change.betap.end2=Inf,
  change.betap.2.factor=1,
  covid.change.beta.start=2020.25,
  covid.change.beta.end=2021,
  covid.change.d.start=2020.25,
  covid.change.d.end=2023)
