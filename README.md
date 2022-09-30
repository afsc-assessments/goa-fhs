# goa-fhs-2022

Gulf of Alaska flathead sole stock assessment, 2022. Maia Sosa Kapur [maia.kapur\@noaa.gov](mailto:maia.kapur@noaa.gov){.email}

Code here builds upon work by C. McGilliard and C. Monnohan. Document dependency on [safe](https://github.com/BenWilliams-NOAA/safe) and apportionments via [rema](https://github.com/afsc-assessments/rema) both v0.1.0.

##  Things to Consider for Future Cycles

-   The mean length and age is declining for the last few years, along with the survey biomass. All three of these aren't well captured in the fits from the base model. Possible that survey timing could fix this.

-   Pearson residuals are wrong. Cole pointed us to a package that can correct these; need to check it out especially for length comps, which aren't looking credit.

-   In the data-prep scripts I inherited, catches are summed across all gears even though the document mentions Trawl only. Either change text to state that they were summed, or remove the HKL/POT gears from the data (it's a very small fraction).

-   Not exactly clear why the VAST index is so much higher than the DB.

-   Late in the game I read that "as for previous assessments, the availability of the survey biomass in 2001 was assumed to be 0.9 to account for the biomass in the eastern region of the Gulf" -- which might explain why the 2001 value was inflated? Too late for me to go back, and bridging indicated this wasn't a huge deal. Instead going to use what's on AKFIN. Sure enough 153594/0.9 = 170660.

-   Make all surveys & associated comps midyear. They're from the same survey and indeed occur midyear, we just didn't touch it this time to encourage continuity with last year. See Bridging for more details.

-   Change to Francis tuning. Suggested weights (for a pre-GPT model) are in `m0_8-newFrancis` and do a slightly better job fitting the survey data. They also down weight all comp data.

-   explore M estimated as offset with males/females.

-   Consider modeling discards explicitly.

-   In tandem with above, revisit Dirichlet or some alternative for the nhauls on the comps data (this is an ongoing area of research, see Pete Hulson).

-   Revisit treatment of $\sigma_R$. Right now it is fixed quite low.

-   Survey biomass data: both in the 2017 model and 2022, the SSB time series seems to ignore the down years from the recent D-B survey. The VAST survey data shows an uptick, but even if I fit to that data instead, the survey fits are too high for the last two years. This tells me there's another data source which is driving the model upwards; **only introducing Francis weights (heavily down-weighting all comps) brought the estimated biomass within the CI of the observations**. Take a look at the profiles to see where the conflict & information lies in this model.
