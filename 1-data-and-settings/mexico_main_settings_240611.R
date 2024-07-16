#-------------------------------------------
# Author: Yiran Liu
# Date Modified: 06.11.24
# Country: Mexico
# Description: This script also contains the fixed settings/assumptions for each
# country, namely those involving when time-varying parameters change, and which
# parameters are calibrated.
#-------------------------------------------

#####################################################
######## country-specific settings to adjust ########
#####################################################
country_p='Mexico'
quasi_2000_tb=2000 # can't do earlier bc mexico has lower incidence, takes longer to reach equilibrium

all.settings <- list(
  ####### incarceration-related settings #######,
  change.incarc.start1=1995,
  change.incarc.end1=2002,
  change.incarc.start2=2014,
  change.incarc.end2=2016,
  change.incarc.start3=2019,
  change.incarc.end3=2021,
  change.incarc.start4=Inf,
  change.incarc.end4=Inf,
  change.r.start1=1995,
  change.r.end1=2002,
  change.r.start2=2013,
  change.r.end2=2017,
  covid.change.r=FALSE,
  covid.start=Inf,
  covid.end=Inf,
  covid.change.r=FALSE,
  state.emerg.start=Inf,
  state.emerg.end=Inf,
  intrvn.start=Inf,
  intrvn.end=Inf,
  intrvn.iE.factor=1,
  intrvn.iR.factor=1,
  intrvn.r.factor=1,
  ####### TB-related settings #######
  change.beta.start1=1994, # based on extreme poverty stats...
  change.beta.end1=1996,
  change.beta.start2=1996, # based on extreme poverty stats...
  change.beta.end2=2002,
  # change.beta.2.factor=1 # for Mexico this is CALIBRATED
  change.beta.start3=2006, # bc of steadily rising community incidence and notifications...
  change.beta.end3=2020,
  # change.beta.3.factor=1 # for Mexico this is CALIBRATED,
  change.prog.start1=Inf,
  change.prog.end1=Inf,
  change.prog.start2=Inf,
  change.prog.end2=Inf,
  change.prog.2.factor=1,
  change.d.start1=1995,
  change.d.end1=1998,
  change.d.start2=Inf,
  change.d.end2=Inf,
  change.d.2.factor=1,
  change.d1.start1=2012,
  change.d1.end1=2020,
  change.d1.temp=FALSE,
  change.d1.exp=FALSE,
  spike.d1.start1=Inf,
  spike.d1.end1=Inf,
  spike.d1.factor=1,
  change.betap.start1=2016,
  change.betap.end1=2020,
  change.betap.exp=FALSE,
  change.betap.start2=Inf,
  change.betap.end2=Inf,
  change.betap.2.factor=NA,
  covid.change.beta.start=2020.25,
  covid.change.beta.end=2021,
  covid.change.d.start=2020.25,
  covid.change.d.end=2022.5)
