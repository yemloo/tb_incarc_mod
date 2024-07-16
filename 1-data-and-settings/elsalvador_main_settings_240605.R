#-------------------------------------------
# Author: Yiran Liu
# Date Modified: 06.05.24
# Country: El Salvador
# Description: This script also contains the fixed settings/assumptions for each
# country, namely those involving when time-varying parameters change, and which
# parameters are calibrated.
#-------------------------------------------

#####################################################
######## country-specific settings to adjust ########
#####################################################
country_p='El Salvador'
quasi_2000_tb=1500 # to make calibration more efficient (if we can achieve equilibrium sooner)

all.settings <- list(
  ####### incarceration-related settings #######
  change.incarc.start1=2005,
  change.incarc.end1=2007,
  change.incarc.start2=2013,
  change.incarc.end2=2015,
  change.incarc.start3=2018,
  change.incarc.end3=2020,
  change.incarc.start4=Inf,
  change.incarc.end4=Inf,
  change.r.start1=1995,
  change.r.end1=2002,
  change.r.start2=Inf,
  change.r.end2=Inf,
  covid.start=Inf,
  covid.end=Inf,
  covid.change.r=FALSE,
  # state.emerg.start=2022.25,
  # state.emerg.end=2023,
  state.emerg.start=2022.25,
  state.emerg.start2=2022.5,
  state.emerg.start3=2023.67,
  intrvn.start=Inf,
  intrvn.end=Inf,
  intrvn.iE.factor=1,
  intrvn.iR.factor=1,
  intrvn.r.factor=1,
  intrvn='Continue SoE', # "Continue SoE", "End SoE", or "Worsen",
  ####### TB-related settings #######,
  change.beta.start1=1990,
  change.beta.end1=2000,
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
  change.d.end1=2000,
  change.d.start2=Inf,
  change.d.end2=Inf,
  change.d.2.factor=1,
  change.d1.start1=2012,
  change.d1.end1=2017,
  change.d1.temp=FALSE,
  change.d1.exp=FALSE,
  spike.d1.start1=2017.5,
  spike.d1.end1=2020.25,
  change.betap.start1=2005,
  change.betap.end1=2017,
  change.betap.exp=TRUE,
  change.betap.start2=2018,
  change.betap.end2=2022.25,
  change.betap.2.factor=-0.25,
  covid.change.beta.start=2020.25,
  covid.change.beta.end=2021,
  covid.change.d.start=Inf,
  covid.change.d.end=Inf)
  