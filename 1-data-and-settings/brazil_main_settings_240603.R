#-------------------------------------------
# Author: Yiran Liu
# Date Modified: 06.04.24
# Country: Brazil
# Description: This script also contains the fixed settings/assumptions for each
# country, namely those involving when time-varying parameters change, and which
# parameters are calibrated.
#-------------------------------------------

#####################################################
######## country-specific settings to adjust ########
#####################################################
country_p='Brazil'
quasi_2000_tb=1500 # to make calibration more efficient (if we can achieve equilibrium sooner)

all.settings <- list(
  ####### incarceration-related settings #######
  change.incarc.start1=1990,
  change.incarc.end1=2020,
  change.incarc.start2=Inf,
  change.incarc.end2=Inf,
  change.incarc.start3=Inf,
  change.incarc.end3=Inf,
  change.incarc.start4=2021, # decrease
  change.incarc.end4=2023,
  change.r.start1=1990,
  change.r.end1=2015,
  change.r.start2=2015, # increase
  change.r.end2=2020,
  covid.start=2020.25,
  covid.end=2021,
  covid.change.r=TRUE,
  state.emerg.start=Inf,
  state.emerg.end=Inf,
  intrvn.start=Inf,
  intrvn.end=Inf,
  intrvn.iE.factor=1,
  intrvn.iR.factor=1,
  intrvn.r.factor=1,
  ####### TB-related settings #######,
  change.beta.start1=1990,
  change.beta.end1=2015,
  change.beta.start2=Inf,
  change.beta.end2=Inf,
  change.beta.2.factor=1,
  change.beta.start3=Inf,
  change.beta.end3=Inf,
  change.beta.3.factor=1,
  change.prog.start1=Inf,
  change.prog.end1=Inf,
  change.prog.start2=Inf,
  change.prog.end2=Inf,
  change.prog.2.factor=1,
  change.d.start1=1990,
  change.d.end1=2015,
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
  change.betap.end1=2018,
  change.betap.exp=FALSE,
  change.betap.start2=Inf,
  change.betap.end2=Inf,
  change.betap.2.factor=1,
  covid.change.beta.start=2020.25,
  covid.change.beta.end=2021,
  covid.change.d.start=2020.25,
  covid.change.d.end=2021.5)
  