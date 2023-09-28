#V3.30.17.00;_2021_06_11;_safe;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_12.3
#Stock Synthesis (SS) is a work of the U.S. Government and is not subject to copyright protection in the United States.
#Foreign copyrights may apply. See copyright.txt for more information.
#_user_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_user_info_available_at:https://vlab.noaa.gov/group/stock-synthesis
#_data_and_control_files: data.ss // control.ss
0  # 0 means do not read wtatage.ss; 1 means read and use wtatage.ss and also read and use growth parameters
1  #_N_Growth_Patterns (Growth Patterns, Morphs, Bio Patterns, GP are terms used interchangeably in SS)
1 #_N_platoons_Within_GrowthPattern 
#_Cond 1 #_Platoon_within/between_stdev_ratio (no read if N_platoons=1)
#_Cond  1 #vector_platoon_dist_(-1_in_first_val_gives_normal_approx)
#
4 # recr_dist_method for parameters:  2=main effects for GP, Area, Settle timing; 3=each Settle entity; 4=none (only when N_GP*Nsettle*pop==1)
1 # not yet implemented; Future usage: Spawner-Recruitment: 1=global; 2=by area
1 #  number of recruitment settlement assignments 
0 # unused option
#GPattern month  area  age (for each settlement assignment)
 1 1 1 0
#
#_Cond 0 # N_movement_definitions goes here if Nareas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#
1 #_Nblock_Patterns
 1 #_blocks_per_pattern 
# begin and end years of blocks
 1977 1977
#
# controls for all timevary parameters 
1 #_time-vary parm bound check (1=warn relative to base parm bounds; 3=no bound check); Also see env (3) and dev (5) options to constrain with base bounds
#
# AUTOGEN
 1 1 1 1 1 # autogen: 1st element for biology, 2nd for SR, 3rd for Q, 4th reserved, 5th for selex
# where: 0 = autogen time-varying parms of this category; 1 = read each time-varying parm line; 2 = read then autogen if parm min==-12345
#
#_Available timevary codes
#_Block types: 0: P_block=P_base*exp(TVP); 1: P_block=P_base+TVP; 2: P_block=TVP; 3: P_block=P_block(-1) + TVP
#_Block_trends: -1: trend bounded by base parm min-max and parms in transformed units (beware); -2: endtrend and infl_year direct values; -3: end and infl as fraction of base range
#_EnvLinks:  1: P(y)=P_base*exp(TVP*env(y));  2: P(y)=P_base+TVP*env(y);  3: P(y)=f(TVP,env_Zscore) w/ logit to stay in min-max;  4: P(y)=2.0/(1.0+exp(-TVP1*env(y) - TVP2))
#_DevLinks:  1: P(y)*=exp(dev(y)*dev_se;  2: P(y)+=dev(y)*dev_se;  3: random walk;  4: zero-reverting random walk with rho;  5: like 4 with logit transform to stay in base min-max
#_DevLinks(more):  21-25 keep last dev for rest of years
#
#_Prior_codes:  0=none; 6=normal; 1=symmetric beta; 2=CASAL's beta; 3=lognormal; 4=lognormal with biascorr; 5=gamma
#
# setup for M, growth, wt-len, maturity, fecundity, (hermaphro), recr_distr, cohort_grow, (movement), (age error), (catch_mult), sex ratio 
#_NATMORT
0 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate;_5=BETA:_Maunder_link_to_maturity
  #_no additional input for selected M option; read 1P per morph
#
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K_incr; 4=age_specific_K_decr; 5=age_specific_K_each; 6=NA; 7=NA; 8=growth cessation
2 #_Age(post-settlement)_for_L1;linear growth below this
999 #_Growth_Age_for_L2 (999 to use as Linf)
-999 #_exponential decay for growth above maxage (value should approx initial Z; -999 replicates 3.24; -998 to not allow growth above maxage)
0  #_placeholder for future growth feature
#
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
0 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
#
2 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
3 #_First_Mature_Age
1 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
1 #_parameter_offset_approach for M, G, CV_G:  1- direct, no offset**; 2- male=fem_parm*exp(male_parm); 3: male=female*exp(parm) then old=young*exp(parm)
#_** in option 1, any male parameter with value = 0.0 and phase <0 is set equal to female parameter
#
#_growth_parms
#_ LO HI INIT PRIOR PR_SD PR_type PHASE env_var&link dev_link dev_minyr dev_maxyr dev_PH Block Block_Fxn
# Sex: 1  BioPattern: 1  NatMort
 0.1 0.3 0.2 0 0.8 0 -3 0 0 0 0 0 0 0 # NatM_uniform_Fem_GP_1
# Sex: 1  BioPattern: 1  Growth
 2 20 9.47332 0 0.2 0 2 0 0 0 0 0 0 0 # L_at_Amin_Fem_GP_1
 25 80 44.3984 0 0.2 0 3 0 0 0 0 0 0 0 # L_at_Amax_Fem_GP_1
 0.01 2 0.187698 0 0.8 0 2 0 0 0 0 0 0 0 # VonBert_K_Fem_GP_1
 0.0001 2 0.106762 2 0.8 0 3 0 0 0 0 0 0 0 # CV_young_Fem_GP_1
 0.0001 2 0.0945479 0.2 0.8 0 4 0 0 0 0 0 0 0 # CV_old_Fem_GP_1
# Sex: 1  BioPattern: 1  WtLen
 0 0.5 4.28e-06 4.8e-06 0.2 0 -3 0 0 0 0 0 0 0 # Wtlen_1_Fem_GP_1
 2.5 4.5 3.2298 3.2298 0.2 0 -3 0 0 0 0 0 0 0 # Wtlen_2_Fem_GP_1
# Sex: 1  BioPattern: 1  Maturity&Fecundity
 0 20 8.74 8.74 0.8 0 -3 0 0 0 0 0 0 0 # Mat50%_Fem_GP_1
 -3 3 -0.773 -0.773 0.8 0 -3 0 0 0 0 0 0 0 # Mat_slope_Fem_GP_1
 -3 3 1 1 0.8 0 -3 0 0 0 0 0 0 0 # Eggs/kg_inter_Fem_GP_1
 -3 4 0 0 0.8 0 -3 0 0 0 0 0 0 0 # Eggs/kg_slope_wt_Fem_GP_1
# Sex: 2  BioPattern: 1  NatMort
 0.1 0.3 0.2 0 0.8 0 -3 0 0 0 0 0 0 0 # NatM_uniform_Mal_GP_1
# Sex: 2  BioPattern: 1  Growth
 1 45 9.54299 10.17 0.8 0 2 0 0 0 0 0 0 0 # L_at_Amin_Mal_GP_1
 20 70 36.8597 37.36 0.8 0 3 0 0 0 0 0 0 0 # L_at_Amax_Mal_GP_1
 0.05 0.35 0.25617 0.204 0.8 0 2 0 0 0 0 0 0 0 # VonBert_K_Mal_GP_1
 0.0001 2 0.127792 2 0.8 0 3 0 0 0 0 0 0 0 # CV_young_Mal_GP_1
 0.0001 2 0.0806053 0.2 0.8 0 4 0 0 0 0 0 0 0 # CV_old_Mal_GP_1
# Sex: 2  BioPattern: 1  WtLen
 0 0.5 4.28e-06 4.8e-06 0.2 0 -3 0 0 0 0 0 0 0 # Wtlen_1_Mal_GP_1
 2.5 4.5 3.2298 3.2298 0.2 0 -3 0 0 0 0 0 0 0 # Wtlen_2_Mal_GP_1
# Hermaphroditism
#  Cohort growth dev base
 -4 4 1 1 0 0 -4 0 0 0 0 0 0 0 # CohortGrowDev
#  Movement
#  Age Error from parameters
#  catch multiplier
#  fraction female, by GP
 1e-06 0.999999 0.5 0.5 0.5 0 -99 0 0 0 0 0 0 0 # FracFemale_GP_1
#
#_no timevary MG parameters
#
#_seasonal_effects_on_biology_parms
 0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_ LO HI INIT PRIOR PR_SD PR_type PHASE
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
3 #_Spawner-Recruitment; Options: 1=NA; 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm; 8=Shepherd_3Parm; 9=RickerPower_3parm
0  # 0/1 to use steepness in initial equ recruitment calculation
0  #  future feature:  0/1 to make realized sigmaR a function of SR curvature
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn #  parm_name
           6.5          13.5       12.8219          11.5            10             0          1          0          0          0          0          0          0          0 # SR_LN(R0)
           0.2             1             1         0.999          0.05             0         -4          0          0          0          0          0          0          0 # SR_BH_steep
             0             2           0.6           0.6           0.8             0         -4          0          0          0          0          0          0          0 # SR_sigmaR
            -5             5             0             0             1             0         -4          0          0          0          0          0          0          0 # SR_regime
             0             0             0             0             0             0        -99          0          0          0          0          0          0          0 # SR_autocorr
#_no timevary SR parameters
1 #do_recdev:  0=none; 1=devvector (R=F(SSB)+dev); 2=deviations (R=F(SSB)+dev); 3=deviations (R=R0*dev; dev2=R-f(SSB)); 4=like 3 with sum(dev2) adding penalty
1984 # first year of main recr_devs; early devs can preceed this era
2020 # last year of main recr_devs; forecast devs start in following year
1 #_recdev phase 
1 # (0/1) to read 13 advanced options
 1955 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
 6 #_recdev_early_phase
 -1 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
 1 #_lambda for Fcast_recr_like occurring before endyr+1
 1950.3 #_last_yr_nobias_adj_in_MPD; begin of ramp
 1992.6 #_first_yr_fullbias_adj_in_MPD; begin of plateau
 2015.8 #_last_yr_fullbias_adj_in_MPD
 2021.6 #_end_yr_for_ramp_in_MPD (can be in forecast to shape ramp, but SS sets bias_adj to 0.0 for fcast yrs)
 0.9342      #_max_bias_adj_in_MPD (typical ~0.8; -3 sets all years to 0.0; -2 sets all non-forecast yrs w/ estimated recdevs to 1.0; -1 sets biasadj=1.0 for all yrs w/ recdevs)
 0 #_period of cycles in recruitment (N parms read below)
 -15 #min rec_dev
 15 #max rec_dev
 0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
# all recruitment deviations
#  1955E 1956E 1957E 1958E 1959E 1960E 1961E 1962E 1963E 1964E 1965E 1966E 1967E 1968E 1969E 1970E 1971E 1972E 1973E 1974E 1975E 1976E 1977E 1978E 1979E 1980E 1981E 1982E 1983E 1984R 1985R 1986R 1987R 1988R 1989R 1990R 1991R 1992R 1993R 1994R 1995R 1996R 1997R 1998R 1999R 2000R 2001R 2002R 2003R 2004R 2005R 2006R 2007R 2008R 2009R 2010R 2011R 2012R 2013F 2014F 2015F 2016F 2017F 2018F
#  -0.133107 -0.158474 -0.188052 -0.222045 -0.26058 -0.304113 -0.352696 -0.404997 -0.459757 -0.515654 -0.56782 -0.619444 -0.672305 -0.728674 -0.787799 -0.842782 -0.880363 -0.888981 -0.854594 -0.759807 -0.564077 -0.179325 0.840761 0.102658 -0.271309 -0.116284 -0.103677 -0.0897352 -0.0585079 -0.0471107 -0.25126 -0.235255 -0.133315 -0.20653 0.206564 -0.345473 -0.161509 0.319765 -0.165696 -0.0849989 -0.278824 -0.502941 0.190432 -0.0346204 0.378986 -0.239601 -0.00968389 -0.0468538 0.300117 -0.00444008 0.262154 -0.1528 -0.0162397 -0.232948 0.147495 0.576348 0.488272 0.279967 0 0 0 0 0 0
#
#Fishing Mortality info 
0 # F ballpark value in units of annual_F
-1984 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
4 # max F or harvest rate, depends on F_Method
# no additional F input needed for Fmethod 1
# if Fmethod=2; read overall start F value; overall phase; N detailed inputs to read
# if Fmethod=3; read N iterations for tuning for Fmethod 3
4  # N iterations for tuning F in hybrid method (recommend 3 to 7)
#
#_initial_F_parms; for each fleet x season that has init_catch; nest season in fleet; count = 1
#_for unconstrained init_F, use an arbitrary initial catch and set lambda=0 for its logL
#_ LO HI INIT PRIOR PR_SD  PR_type  PHASE
 0 2 0.00650716 0 99 0 5 # InitF_seas_1_flt_1Fishery
#
# F rates by fleet x season
# Yr:  1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018
# seas:  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# Fishery 0.00497964 0.00190214 0.0250532 0.01353 0.0175751 0.0135924 0.00645694 0.00339321 0.00139841 0.00130175 0.00421829 0.00589779 0.011353 0.00970256 0.0182449 0.0225501 0.0204923 0.0179487 0.0256472 0.0205344 0.0145252 0.00744499 0.012688 0.0155755 0.0174782 0.020171 0.0199072 0.021374 0.0263413 0.0260357 0.0283263 0.0298417 0.0311464 0.0219619 0.0171673 0.0219842 0.0197653 0.01457 0.0178649 0.010627 0.010627
#
#_Q_setup for fleets with cpue or survey data
#_1:  fleet number
#_2:  link type: (1=simple q, 1 parm; 2=mirror simple q, 1 mirrored parm; 3=q and power, 2 parm; 4=mirror with offset, 2 parm)
#_3:  extra input for link, i.e. mirror fleet# or dev index number
#_4:  0/1 to select extra sd parameter
#_5:  0/1 for biasadj or not
#_6:  0/1 to float
#_   fleet      link link_info  extra_se   biasadj     float  #  fleetname
         2         1         0         0         0         0  #  Survey
-9999 0 0 0 0 0
#
#_Q_parms(if_any);Qunits_are_ln(q)
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
            -1             1             0             0           0.5             0         -2          0          0          0          0          0          0          0  #  LnQ_base_Survey(2)
#_no timevary Q parameters
#
#_size_selex_patterns
#Pattern:_0;  parm=0; selex=1.0 for all sizes
#Pattern:_1;  parm=2; logistic; with 95% width specification
#Pattern:_2;  parm=6; modification of pattern 24 with improved sex-specific offset
#Pattern:_5;  parm=2; mirror another size selex; PARMS pick the min-max bin to mirror
#Pattern:_11; parm=2; selex=1.0  for specified min-max population length bin range
#Pattern:_15; parm=0; mirror another age or length selex
#Pattern:_6;  parm=2+special; non-parm len selex
#Pattern:_43; parm=2+special+2;  like 6, with 2 additional param for scaling (average over bin range)
#Pattern:_8;  parm=8; double_logistic with smooth transitions and constant above Linf option
#Pattern:_9;  parm=6; simple 4-parm double logistic with starting length; parm 5 is first length; parm 6=1 does desc as offset
#Pattern:_21; parm=2+special; non-parm len selex, read as pairs of size, then selex
#Pattern:_22; parm=4; double_normal as in CASAL
#Pattern:_23; parm=6; double_normal where final value is directly equal to sp(6) so can be >1.0
#Pattern:_24; parm=6; double_normal with sel(minL) and sel(maxL), using joiners
#Pattern:_25; parm=3; exponential-logistic in length
#Pattern:_27; parm=special+3; cubic spline in length; parm1==1 resets knots; parm1==2 resets all 
#Pattern:_42; parm=special+3+2; cubic spline; like 27, with 2 additional param for scaling (average over bin range)
#_discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead;_4=define_dome-shaped_retention
#_Pattern Discard Male Special
 0 0 0 0 # 1 Fishery
 0 0 0 0 # 2 Survey
#
#_age_selex_patterns
#Pattern:_0; parm=0; selex=1.0 for ages 0 to maxage
#Pattern:_10; parm=0; selex=1.0 for ages 1 to maxage
#Pattern:_11; parm=2; selex=1.0  for specified min-max age
#Pattern:_12; parm=2; age logistic
#Pattern:_13; parm=8; age double logistic
#Pattern:_14; parm=nages+1; age empirical
#Pattern:_15; parm=0; mirror another age or length selex
#Pattern:_16; parm=2; Coleraine - Gaussian
#Pattern:_17; parm=nages+1; empirical as random walk  N parameters to read can be overridden by setting special to non-zero
#Pattern:_41; parm=2+nages+1; // like 17, with 2 additional param for scaling (average over bin range)
#Pattern:_18; parm=8; double logistic - smooth transition
#Pattern:_19; parm=6; simple 4-parm double logistic with starting age
#Pattern:_20; parm=6; double_normal,using joiners
#Pattern:_26; parm=3; exponential-logistic in age
#Pattern:_27; parm=3+special; cubic spline in age; parm1==1 resets knots; parm1==2 resets all 
#Pattern:_42; parm=2+special+3; // cubic spline; with 2 additional param for scaling (average over bin range)
#Age patterns entered with value >100 create Min_selage from first digit and pattern from remainder
#_Pattern Discard Male Special
 20 0 3 0 # 1 Fishery
 20 0 3 0 # 2 Survey
#
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
# 1   Fishery LenSelex
# 2   Survey LenSelex
# 1   Fishery AgeSelex
             1            16       12.4161            15             5             0          1          0          0          0          0          0          0          0  #  Age_DblN_peak_Fishery(1)
            -5            50            30            30             5             0         -3          0          0          0          0          0          0          0  #  Age_DblN_top_logit_Fishery(1)
            -4            12       2.77241           3.2             5             0          2          0          0          0          0          0          0          0  #  Age_DblN_ascend_se_Fishery(1)
            -2            15             8           3.4             5             0         -3          0          0          0          0          0          0          0  #  Age_DblN_descend_se_Fishery(1)
           -12             5           -10          -999             5             0         -4          0          0          0          0          0          0          0  #  Age_DblN_start_logit_Fishery(1)
            -5             20          15           999             5             0         -4          0          0          0          0          0          0          0  #  Age_DblN_end_logit_Fishery(1)
           -15            15     -0.983588             0             5             0          2          0          0          0          0          0          0          0  #  AgeSel_1Male_Peak_Fishery
           -15            15     -0.115692             0             5             0          3          0          0          0          0          0          0          0  #  AgeSel_1Male_Ascend_Fishery
           -15            15             0             0             5             0         -3          0          0          0          0          0          0          0  #  AgeSel_1Male_Descend_Fishery
           -15            15             1             0             5             0         -3          0          0          0          0          0          0          0  #  AgeSel_1Male_Final_Fishery
           -15            15             1             0             5             0         -4          0          0          0          0          0          0          0  #  AgeSel_1Male_Scale_Fishery
# 2   Survey AgeSelex
             1            20       7.24603            11             5             0          1          0          0          0          0          0          0          0  #  Age_DblN_peak_Survey(2)
            -5            50            30            30             5             0         -3          0          0          0          0          0          0          0  #  Age_DblN_top_logit_Survey(2)
            -4            12       2.13572             4             5             0          2          0          0          0          0          0          0          0  #  Age_DblN_ascend_se_Survey(2)
            -2            15             8           3.4             5             0         -3          0          0          0          0          0          0          0  #  Age_DblN_descend_se_Survey(2)
         -1000             5           -10          -999             5             0         -4          0          0          0          0          0          0          0  #  Age_DblN_start_logit_Survey(2)
            -5            20           15           999             5             0         -4          0          0          0          0          0          0          0  #  Age_DblN_end_logit_Survey(2)
           -15            15     -0.671623             0             5             0          2          0          0          0          0          0          0          0  #  AgeSel_2Male_Peak_Survey
           -15            15     -0.303925             0             5             0          2          0          0          0          0          0          0          0  #  AgeSel_2Male_Ascend_Survey
           -15            15             0             0             5             0         -3          0          0          0          0          0          0          0  #  AgeSel_2Male_Descend_Survey
           -15            15             0             0             5             0         -3          0          0          0          0          0          0          0  #  AgeSel_2Male_Final_Survey
           -15            15             1             0             5             0         -4          0          0          0          0          0          0          0  #  AgeSel_2Male_Scale_Survey
#_No_Dirichlet parameters
#_no timevary selex parameters
#
0   #  use 2D_AR1 selectivity(0/1)
#_no 2D_AR1 selex offset used
#
# Tag loss and Tag reporting parameters go next
0  # TG_custom:  0=no read and autogen if tag data exist; 1=read
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
# no timevary parameters
#
#
# Input variance adjustments factors: 
 #_1=add_to_survey_CV
 #_2=add_to_discard_stddev
 #_3=add_to_bodywt_CV
 #_4=mult_by_lencomp_N
 #_5=mult_by_agecomp_N
 #_6=mult_by_size-at-age_N
 #_7=mult_by_generalized_sizecomp
#Data_type Fleet New_Var_adj hash Old_Var_adj New_Francis New_MI Francis_mult Francis_lo Francis_hi MI_mult Type Name Note
4 1 1.248057 # 1 0.215058 1.248057 0.215058 0.14288 0.425107 1.248057 len Fishery 
4 2 1.097507 # 1 0.470511 1.097507 0.470511 0.298054 1.284164 1.097507 len Survey 
5 2 0.327887 # 1 0.2159 0.327887 0.2159 0.133452 0.519184 0.327887 age Survey 
 -9999   1    0  # terminator
#
10 #_maxlambdaphase
1 #_sd_offset; must be 1 if any growthCV, sigmaR, or survey extraSD is an estimated parameter
# read 3 changes to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 9=init_equ_catch; 
# 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 17=F_ballpark; 18=initEQregime
#like_comp fleet  phase  value  sizefreq_method
 4 1 1 1 1
 4 2 1 1 1
 5 2 1 1 1
-9999  1  1  1  1  #  terminator
#
# lambdas (for info only; columns are phases)
#  0 0 0 0 0 0 0 0 0 0 #_CPUE/survey:_1
#  1 1 1 1 1 1 1 1 1 1 #_CPUE/survey:_2
#  1 1 1 1 1 1 1 1 1 1 #_lencomp:_1
#  1 1 1 1 1 1 1 1 1 1 #_lencomp:_2
#  0 0 0 0 0 0 0 0 0 0 #_agecomp:_1
#  1 1 1 1 1 1 1 1 1 1 #_agecomp:_2
#  1 1 1 1 1 1 1 1 1 1 #_init_equ_catch1
#  1 1 1 1 1 1 1 1 1 1 #_init_equ_catch2
#  1 1 1 1 1 1 1 1 1 1 #_recruitments
#  1 1 1 1 1 1 1 1 1 1 #_parameter-priors
#  1 1 1 1 1 1 1 1 1 1 #_parameter-dev-vectors
#  1 1 1 1 1 1 1 1 1 1 #_crashPenLambda
#  0 0 0 0 0 0 0 0 0 0 # F_ballpark_lambda
0 # (0/1/2) read specs for more stddev reporting: 0 = skip, 1 = read specs for reporting stdev for selectivity, size, and numbers, 2 = add options for M,Dyn. Bzero, SmryBio
 # 0 2 0 0 # Selectivity: (1) fleet, (2) 1=len/2=age/3=both, (3) year, (4) N selex bins
 # 0 0 # Growth: (1) growth pattern, (2) growth ages
 # 0 0 0 # Numbers-at-age: (1) area(-1 for all), (2) year, (3) N ages
 # -1 # list of bin #'s for selex std (-1 in first bin to self-generate)
 # -1 # list of ages for growth std (-1 in first bin to self-generate)
 # -1 # list of ages for NatAge std (-1 in first bin to self-generate)
999
