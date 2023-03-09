##############################
#
# Load obs data
#
# AWalker
# May 2021 (update Aug 2022)
#
#############################################

wd_obs  <- '/mnt/disk2/Research_Projects/FACE_modelling/Phase_3/data/processed_response'
setwd(wd_obs)



# load various observations, process, and calculate additional variables 
df_co2 <- read.csv('site_annual_CO2.csv')
df_co2$co2 <- as.factor(df_co2$co2)
levels(df_co2$co2) <- c('AMB','ELE')
names(df_co2)
names(df_co2)[1] <- 'YEAR'

ifile <- 'OBS_ax.rds'
df_obs <- readRDS(ifile)
str(df_obs)
names(df_obs)
df_obs <- df_obs %>% 
  left_join(df_co2, by=c('site','YEAR','co2'), suffix=c('','.real') ) %>%
  filter(site=='DUKE'|site=='ORNL')

df_obs$FROOT_alloc_frac <- df_obs$GR / df_obs$NPP
df_obs$LEAF_FROOT_ratio <- df_obs$CL / df_obs$CFR

ifile <- 'OBS_ase.rds'
df_obsse <- readRDS(ifile)
str(df_obsse)
df_obsse <- df_obsse %>% 
  filter(site=='DUKE'|site=='ORNL')

df_obsse$FROOT_alloc_frac <- df_obs$FROOT_alloc_frac * 
  ((df_obsse$GR/df_obs$GR)^2 + (df_obsse$NPP/df_obs$NPP)^2)^0.5
df_obsse$LEAF_FROOT_ratio <- df_obs$LEAF_FROOT_ratio * 
  ((df_obsse$CL/df_obs$CL)^2 + (df_obsse$CFR/df_obs$CFR)^2)^0.5



# function to extract variables from obs datasets for plotting
process_obs <- function(df_obs, df_obsse, fsite='DUKE', fvar='NPP', fco2='AMB', 
                        aco2.se=2.5, eco2.se=10 ) {
  
  # if(fsite=='DUKE') fsite_mod <- 'US-DUK'
  # if(fsite=='ORNL') fsite_mod <- 'US-ORN'
  # if(fco2=='AMB')   fsite_mod_co2 <- paste(fsite_mod,'aCO2',sep='_')
  # if(fco2=='ELE')   fsite_mod_co2 <- paste(fsite_mod,'eCO2',sep='_')
  # print(fsite_mod_co2)
  
  # process observed data and calculate responses
  dfose <- df_obsse %>% 
    select(site,YEAR,co2,fvar) %>%
    filter(site==fsite ) %>%
    rename(vy=fvar)
  
  dfo <- df_obs %>% 
    select(site,YEAR,co2,CO2.real,fvar) %>%
    filter(site==fsite ) %>% 
    rename(vy=fvar) %>%
    left_join(dfose, by=c('site','YEAR','co2'), suffix=c('','.se') ) %>%
    mutate(vy.serel=vy.se/vy)
  
  dfoa <- dfo %>% 
    filter(co2=='AMB' ) 
  
  dfoe <- dfo %>% 
    filter(co2=='ELE' ) 
  
  dfo_resp <- subset(dfo, co2=='AMB') %>%
    left_join(subset(dfo, co2=='ELE'), by=c('site','YEAR'), suffix=c('','.eco2') ) %>%
    mutate(
      resp_abs      = vy.eco2-vy,
      resp_abs.se   = (vy.se^2 + vy.se.eco2^2)^0.5,
      resp_pct      = 1e2*(vy.eco2/vy - 1 ),
      resp_pct.se   = resp_pct*(vy.serel^2 + vy.serel.eco2^2)^0.5,
      resp_ratio    = vy.eco2/vy,
      resp_ratio.se = resp_ratio*(vy.serel^2 + vy.serel.eco2^2)^0.5,
      co2_ratio     = CO2.real.eco2/CO2.real,
      # following Walker et al. 2021
      # assume a 10 ppm / ~1 Pa standard error in eCO2 data to account for variability from the mean treatment co2
      # but assume lower, 2.5 ppm, error for ambient treatment 
      co2_ratio.se  = co2_ratio*((aco2.se/CO2.real)^2 + (eco2.se/CO2.real.eco2)^2)^0.5,
      resp_beta     = beta_array(vy, vy.eco2, CO2.real, CO2.real.eco2 ),
      resp_beta.se  = abs(resp_beta)*( ((resp_ratio.se) / log(resp_ratio))^2 + 
                                         ((co2_ratio.se) / log(co2_ratio))^2)^0.5
    )
  
  # subet for non-missing values
  dfoa     <- dfoa[is.finite(dfoa$vy),]
  dfo_resp <- dfo_resp[is.finite(dfo_resp$vy),]
  
  list(dfo=dfo, dfoa=dfoa, dfoe=dfoe, dfo_resp=dfo_resp )
}


# xyplot(NPP~YEAR|site,df_obs,groups=co2)
# xyplot(NPP~YEAR|site,df_obsse,groups=co2)
# xyplot(T~YEAR|site,df_obs)
# xyplot(T~YEAR|site,df_obsse)



### END ###