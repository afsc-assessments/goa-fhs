## comparing transition between v3.24 and v3.30.16
## I used the ss_trans.exe but had to modify the "fishery timing" in the dat file for the survey to 0
## wasn't positive that was accurate...but it autoinput an NA

require(r4ss)
require(here)

mod17 <- SS_output(here('flathead_2021','2017-mod'))
mod17_trans <-  SS_output(here('flathead_2021','converted_3.30.16'))

SSplotComparisons(SSsummarize(list(mod17,mod17_trans)),
                  legendlabels  = c('2017 Model','Transitioned Model'))

## It looks like the years somehow got extended in the transitioned model. I think the issue
## is happening in the recruits (there are 5 extra years in recruits). In convertedv2 I mod'd the forecast
## to be all zeroes for Fcast years and Firstyear for Caps @ 2021
mod17_transv2 <-  SS_output(here('flathead_2021','converted_3.30.16_v2'))
SSplotComparisons(SSsummarize(list(mod17,mod17_trans,mod17_transv2)),
                  legendlabels  = c('2017 Model','Transitioned Model','Transitioned Model v2'))
