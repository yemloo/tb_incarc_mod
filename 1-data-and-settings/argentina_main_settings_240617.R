#-------------------------------------------
# Author: Yiran Liu
# Date Modified: 7.3.24
# Country: Argentina
# Description: This script also contains the fixed settings/assumptions for each
# country, namely those involving when time-varying parameters change, and which
# parameters are calibrated.
#-------------------------------------------

#####################################################
######## country-specific settings to adjust ########
#####################################################
country_p='Argentina'
quasi_2000_tb=1500 # to make calibration more efficient (if we can achieve equilibrium sooner)

all.settings <- list(
  ####### incarceration-related settings #######
  change.incarc.start1=1992,
  change.incarc.end1=2000,
  change.incarc.start2=2000,
  change.incarc.end2=2002,
  change.incarc.start3=2011,
  change.incarc.end3=2016,
  change.incarc.start4=Inf,
  change.incarc.end4=Inf,
  change.r.start1=2004,
  change.r.end1=2006,
  change.r.start2=Inf,
  change.r.end2=Inf,
  covid.start=2020.25,
  covid.end=2022,
  covid.change.r=FALSE, # previously TRUE but bug in code made this FALSE
  state.emerg.start=Inf,
  state.emerg.end=Inf,
  intrvn.start=Inf,
  intrvn.end=Inf,
  intrvn.iE.factor=1,
  intrvn.iR.factor=1,
  intrvn.r.factor=1,
  ####### TB-related settings #######
  change.beta.start1=2002,
  change.beta.end1=2010,
  change.beta.start2=2013,
  change.beta.end2=2020,
  change.beta.2.factor=-1,
  change.beta.start3=Inf,
  change.beta.end3=Inf,
  change.beta.3.factor=1,
  change.prog.start1=Inf, # previously 2002
  change.prog.end1=Inf, # previously 2010
  change.prog.start2=Inf, # previously 2013
  change.prog.end2=Inf, # previously 2020
  change.prog.2.factor=1, # previously -1
  change.d.start1=1992,
  change.d.end1=2000,
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
  change.betap.start1=2015,
  change.betap.end1=2020,
  change.betap.exp=FALSE,
  change.betap.start2=Inf,
  change.betap.end2=Inf,
  change.betap.2.factor=1,
  covid.change.beta.start=2020.25,
  covid.change.beta.end=2021,
  covid.change.d.start=2020.25,
  covid.change.d.end=2021.5)
