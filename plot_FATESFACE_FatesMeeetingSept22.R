##############################
#
# Open ELM/CLM R arrays and plot 
#
# AWalker
# May 2021 (update Aug 2022)
#
##############################

rm(list=ls())

# data processing
library(dplyr)
library(tidyr)
library(stringr)

# plotting
library(lattice)
library(grid)
library(viridis)



# Plotting functions
############################

plot_ensemble_2d <- function(
    pvar='NPP', 
    case='RD', 
    fsite='DUKE', 
    plot_ensemble=T, 
    plotsetup=labs, 
    plot_obs_regression=T,
    df_obsp=df_obs,
    df_obspse=df_obsse
    ) {
  
  ylabs <- plotsetup[[pvar]]
  
  if(fsite=='DUKE') fsite_mod <- 'US-DUK'
  if(fsite=='ORNL') fsite_mod <- 'US-ORN'
  
  # obs processing
  obs <- process_obs(df_obsp, df_obspse, fvar=pvar, fsite=fsite )
  
  # model processing
  print(paste('a2',case,fsite_mod,'aCO2', sep='_' ))
  print(paste('a2',case,fsite_mod,'eCO2', sep='_' ))
  a2pa  <- get(paste('a2',case,fsite_mod,'aCO2', sep='_' ))
  a2pe  <- get(paste('a2',case,fsite_mod,'eCO2', sep='_' ))
  a2pea_abs <- a2pe - a2pa 
  a2pea_rel <- 1e2*(a2pe/a2pa - 1) 

  ylim      <- ylabs$ylim
  ylim_abs  <- ylabs$ylim_abs
  ylim_rel  <- ylabs$ylim_rel
  
  # plots  
  p1 <- 
    xyplot(a2pa[,pvar,] ~ rep(as.numeric(dimnames(a2pa)[[1]]),dim(a2pa)[3]), groups=rep(1:dim(a2pa)[3],each=dim(a2pa)[1]),
           type='l', 
           ylim=ylim, ylab=ylabs$base, xlab='Year',
           col=rev(viridis(dim(a2pa)[3])),
           panel=function(...) {
             panel.polygon(c(obs$dfoa$YEAR,rev(obs$dfoa$YEAR)),
                           c(obs$dfoa$vy+1.96*obs$dfoa$vy.se, rev(obs$dfoa$vy-1.96*obs$dfoa$vy.se)),
                           border=F, col='grey90')
             panel.polygon(c(obs$dfoa$YEAR,rev(obs$dfoa$YEAR)),
                           c(obs$dfoa$vy+obs$dfoa$vy.se, rev(obs$dfoa$vy-obs$dfoa$vy.se)),
                           border=F, col='grey80')
             panel.points(obs$dfoa$YEAR,obs$dfoa$vy, type='l', col='black', lwd=2 )
             panel.abline(h=0, col='grey80' )
             if(plot_ensemble) panel.xyplot(...)
           },
           key=list(x=0,y=1,corner=c(0,1),text=list(paste(fsite,case)))
           # , auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1, columns=3,
           #               border=T, cex=0.75, background='white')
    )
  
  p1a <- 
    xyplot(a2pe[,pvar,] ~ rep(as.numeric(dimnames(a2pa)[[1]]),dim(a2pa)[3]), groups=rep(1:dim(a2pa)[3],each=dim(a2pa)[1]),
           type='l', 
           ylim=ylim, ylab=ylabs$base, xlab='Year',
           col=rev(viridis(dim(a2pa)[3])),
           panel=function(...) {
             panel.polygon(c(obs$dfoe$YEAR[!is.na(obs$dfoe$vy)],rev(obs$dfoe$YEAR[!is.na(obs$dfoe$vy)])),
                           c(obs$dfoe$vy[!is.na(obs$dfoe$vy)]+1.96*obs$dfoe$vy.se[!is.na(obs$dfoe$vy)], rev(obs$dfoe$vy[!is.na(obs$dfoe$vy)]-1.96*obs$dfoe$vy.se[!is.na(obs$dfoe$vy)])),
                           border=F, col='grey90')
             panel.polygon(c(obs$dfoe$YEAR[!is.na(obs$dfoe$vy)],rev(obs$dfoe$YEAR[!is.na(obs$dfoe$vy)])),
                           c(obs$dfoe$vy[!is.na(obs$dfoe$vy)]+obs$dfoe$vy.se[!is.na(obs$dfoe$vy)], rev(obs$dfoe$vy[!is.na(obs$dfoe$vy)]-obs$dfoe$vy.se[!is.na(obs$dfoe$vy)])),
                           border=F, col='grey80')
             panel.points(obs$dfoe$YEAR,obs$dfoe$vy, type='l', col='black', lwd=2 )
             panel.abline(h=0, col='grey80' )
             if(plot_ensemble) panel.xyplot(...)
           },
           key=list(x=0,y=1,corner=c(0,1),text=list(paste(fsite,case)))
           # , auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1, columns=3,
           #               border=T, cex=0.75, background='white')
    )
  
  p2 <- 
    xyplot(a2pea_abs[,pvar,] ~ rep(as.numeric(dimnames(a2pa)[[1]]),dim(a2pa)[3]), groups=rep(1:dim(a2pa)[3],each=dim(a2pa)[1]),
           type='l',
           ylim=ylim_abs, ylab=ylabs$resp, xlab='Year',
           col=rev(viridis(dim(a2pa)[3])),
           panel=function(...) {
             panel.polygon(c(obs$dfo_resp$YEAR,rev(obs$dfo_resp$YEAR)),
                           c(obs$dfo_resp$resp_abs+1.96*obs$dfo_resp$resp_abs.se, rev(obs$dfo_resp$resp_abs-1.96*obs$dfo_resp$resp_abs.se)),
                           border=F, col='grey90')
             panel.polygon(c(obs$dfo_resp$YEAR, rev(obs$dfo_resp$YEAR)),
                           c(obs$dfo_resp$resp_abs+obs$dfo_resp$resp_abs.se, rev(obs$dfo_resp$resp_abs-obs$dfo_resp$resp_abs.se)),
                           border=F, col='grey80')
             panel.points(obs$dfo_resp$YEAR,obs$dfo_resp$resp_abs, type='l', col='black', lwd=2 )
             panel.abline(h=0, col='grey80' )
             if(plot_ensemble) panel.xyplot(...)
           },
           key=list(x=0,y=1,corner=c(0,1),text=list(paste(fsite,case)))
           )
  
  p3 <- 
    xyplot(a2pea_rel[,pvar,] ~ rep(as.numeric(dimnames(a2pa)[[1]]),dim(a2pa)[3]), groups=rep(1:dim(a2pa)[3],each=dim(a2pa)[1]),
           type='l', 
           ylim=ylim_rel, ylab=ylabs$rel, xlab='Year', 
           col=rev(viridis(dim(a2pa)[3])),
           panel=function(...) {
             panel.polygon(c(obs$dfo_resp$YEAR, rev(obs$dfo_resp$YEAR) ),
                           c(obs$dfo_resp$resp_pct+1.96*obs$dfo_resp$resp_pct.se, rev(obs$dfo_resp$resp_pct-1.96*obs$dfo_resp$resp_pct.se)),
                           border=F, col='grey90')
             panel.polygon(c(obs$dfo_resp$YEAR, rev(obs$dfo_resp$YEAR)),
                           c(obs$dfo_resp$resp_pct+obs$dfo_resp$resp_pct.se, rev(obs$dfo_resp$resp_pct-obs$dfo_resp$resp_pct.se)),
                           border=F, col='grey80')
             panel.points(obs$dfo_resp$YEAR,obs$dfo_resp$resp_pct, type='l', col='black', lwd=2 )
             if(plot_obs_regression) {
               y_start <- if(fsite=='DUKE') 2 else 1
               sub     <- y_start:length(obs$dfo_resp$YEAR) 
               print(sub)
               panel.lmline(obs$dfo_resp$YEAR[sub], obs$dfo_resp$resp_pct[sub], type='l', col='red', lwd=2, lty=2 )
             }
             panel.abline(h=0, col='grey80' )
             if(plot_ensemble) panel.xyplot(...)
           },
           key=list(x=0,y=1,corner=c(0,1),text=list(paste(fsite,case)))
           )
  
  list(p1, p1a, p2, p3 )
}


plot3d_ensemble <- function(a1, vid, na0=T, pa2sub=NULL, logt=F,
                            var2=l2$dim_variables$fates_levscls$vals,
                            var_deets=l2$variables[[dimnames(a1)$vars[vid]]],
                            xlab='Year', zmax=NA,
                            fates_dim='DBH Size Class [cm]',
                            var_label=NA, 
                            string3d=" by size class"
) {
  
  # dimnames(a1)$vars[vid]
  
  # determine values of second dimension 
  lv2      <- length(var2)
  row_vals <- apply(rbind(var2,c(var2[2:lv2],lv2)), 2, mean )
  if(is.na(var_label)) var_label <- paste0(var_deets$longname,' [',var_deets$units,']') 
  var_label1 <- var_label %>% 
    str_replace(string3d, "")
  
  # process data
  plot_array  <- aperm(a1[,,,dimnames(a1)$vars[vid],], c(2,1,3) )
  if(is.na(zmax)) zmax <- max(plot_array)
  print(zmax)
  zmin        <- min(plot_array)
  if(na0) plot_array[plot_array==0] <- NA
  
  # create 2D array
  plot_array2 <- aperm(plot_array, c(2,1,3))
  dim3d       <- dim(plot_array2)
  if(is.null(pa2sub)) pa2sub <- 1:dim3d[2]
  plot_array2 <- plot_array2[,pa2sub,]
  plot_2d     <- apply(plot_array, c(1,3), sum, na.rm=T )
  if(logt) {
    plot_array <- log(plot_array)
    zmax       <- log(zmax)
    zmin       <- log(zmin)
  }
  
  # plot  
  # 3d
  p3d_array <-
    levelplot(plot_array,
              cuts=50, col.regions=c('white',rev(magma(50))),
              # useRaster=T,
              main=var_label,
              ylim=c(0.5,13.5),
              # column.values=row_vals, 
              xlab=xlab,
              ylab=fates_dim,
              aspect=1,
              scales=list(tck=c(0.5,0), alternating=F,
                          y=list(at=c(0:12 + 0.5)[c(1,2,seq(3,13,3))], labels=var2[c(1,2,seq(3,13,3))]),
                          x=list(at=c(50,150), labels=c(50,150)) ), 
              as.table=T, fill='white', 
              layout=c(9,2),
              colorkey=list(at=(seq(0,zmax,zmax/50)), raster=T )
    )
  
  # 2d
  p2d1 <- 
    xyplot(plot_2d~rep(1:dim(plot_2d)[1],dim(plot_2d)[2]), groups=rep(dimnames(plot_2d)[[2]], each=dim(plot_2d)[1] ),
           xlab=xlab, ylab=var_label1,
           type='l', auto.key=list(space='right', points=F, lines=T ))
  
  p2d2 <- 
    xyplot(plot_2d~rep(1:dim(plot_2d)[1],dim(plot_2d)[2])|rep(dimnames(plot_2d)[[2]], each=dim(plot_2d)[1] ), 
           xlab=xlab, ylab=var_label1,
           type='l', as.table=T, layout=c(9,2)  )
  
  
  # 2d line
  p2d_array <- 
    xyplot((plot_array2)~(rep(row_vals, prod(dim3d[2:3]) )) | rep(dimnames(plot_array)[[3]], each=prod(dim3d[1:2]) ),
           groups=rep(rep(1:dim3d[2], each=dim3d[1] ), dim3d[3] ),
           col=viridis(dim3d[2]),
           xlab=fates_dim,
           ylab=var_label1,
           scales=list(x=list(log=logt), y=list(log=logt), alternating=F ),
           type='l', as.table=T, layout=c(9,2) )
  
  list(p3d_array, p2d_array, p2d1, p2d2 )
}



size_class_heatmap <- function(mat1=scls, vid=vid1, br=20, variables=rl1$variables,
                               xlab='days', ylab='DBH size class [cm]', logt=F ) {
  
  print('')
  print('plotting:')
  print(paste('  ', dimnames(mat1)$vars[vid]))
  
  mat2 <- 
    if(logt) {
      mat1[,,,dimnames(mat1)$vars[vid]]
      log(mat1[,,,dimnames(mat1)$vars[vid]])
    } else {
      mat1[,,,dimnames(mat1)$vars[vid]]
    }
  
  zmax <- ceiling(max(mat2))
  zmin <- min(0, floor(min(mat2)))
  
  var_deets <- variables[[dimnames(mat1)$vars[vid]]]
  
  p1 <- 
    levelplot(t(mat2), 
              cuts=br, col.regions=rev(magma(br+1)),
              main=paste0(var_deets$longname,' [',var_deets$units,']'),
              ylab=ylab, ylim=c(0,110), column.values=row_vals, aspect=1, xlab=xlab,
              scales=list(y=list(at=c(fates_levscls,100)), tck=c(0.5,0) ),
              colorkey=list(at=seq(zmin,zmax,zmax/br) )
    )
  
  p1
} 



# Initialise
############################

# paths
wd_obs  <- '/mnt/disk2/Research_Projects/FACE_modelling/Phase_3/data/processed_response'
root    <- '/Volumes/disk2/Research_Projects/FATES/runs/FACE/'
wd_src  <- paste(root,'FATESFACE_analysis', sep='/' )
moddate_ens <- '220207'
moddate     <- '220207'
wdo         <- paste0(root,moddate,'/plots')
if(!file.exists(wdo)) dir.create(wdo)
wdo_ens <- paste0(root,moddate_ens,'/plots')

moddate_ens_struct <- '211007'
caseidprefix_gen_ens_struct <- 'FACEbase_nutrients'
ensemble <- 'FACE_ensembleCNP_RD_processed'



# load functions etc
setwd(wd_src)
source('functions_FATESFACE.R')
# plotting settings - yet to be added into plotlist
# also figure out how to insert variables into expressions
colrs      <- rep(c(viridis(10)[3],viridis(10)[7]), each=2 ) 
case_colrs <- viridis 
ltypes     <- 1 
lalpha     <- c(0.5,1) 
leg_cols   <- 2
source('lists_plotting_FATESFACE.R')


# filename variables
# single runs
caseidprefix_gen     <- 'FACEbase_nutrients_rootdemnd_diffvmx' #  or could be an ensemble member 
caseidprefix_var <- c('C', 'CNP_ECA', 'CNP_RD' )
caseidprefix <- paste(caseidprefix_gen,caseidprefix_var,sep='_')
sites        <- c('US-DUK', 'US-ORN' )
cases        <- c('aCO2', 'eCO2' )
years        <- list(
  `US-DUK` = 1996:2007,
  `US-ORN` = 1998:2008
) 

# ensemble
caseidprefix_gen_ens <- 'FACE_ensembleCNP_RD-ECA'
caseidprefix_var_ens <- c('RD', 'ECA' )
caseidprefix_ens     <- paste(caseidprefix_gen_ens, caseidprefix_var_ens, sep='_' )
ensemble             <- paste0(caseidprefix_ens, '_processed' )
parl_file            <- paste('vmax', caseidprefix_var_ens, '_parm_list', sep='' )
parv_file            <- paste('vmax', caseidprefix_var_ens, '_parm_vals', sep='' )

# calculate additional variables from model output 
add_vars     <- T

# calculate additional variables from observations 
add_vars_obs <- T


# load obs data
#############################################

setwd(wd_obs)

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



# open RDS lists of arrays - these need to have been generated by ELM_postprocessR
#############################################

# # create a 3-level list of caseidprefix (level 1), site (level 2), & co2 (level 3) 
# l1 <- list()
# for(c in caseidprefix) {
#   setwd(paste(root,moddate,caseidprefix_gen,sep='/'))
#   setwd(paste0(c,'_processed'))
#   print('')
#   print(getwd())
#   
#   l2 <- list()
#   for(s in sites) {
#     l3 <- list()
#     for(co2 in cases) {
#       
#       # read annual data and extract lndgrd,time array 
#       fname <- paste(c,s,co2,'annual',sep='_')
#       print(fname)
#       rl1   <- readRDS(paste0(fname,'.RDS')) 
#       a1    <- rl1$data_arrays$`lndgrid,time`
#       # currently not added to any data structure
#       a2    <- rl1$data_arrays$`lndgrid,fates_levscls,time`
#       a3    <- rl1$data_arrays$`lndgrid,levgrnd,time`
#       
#       # add variables
#       if(add_vars) a1 <- add_vars_lndtime(a1, add_vars_list )
#       
#       # combine data
#       l3    <- c(l3,list(a1))
#       names(l3)[length(l3)] <- co2
#     }
#     l2    <- c(l2,list(l3))
#     names(l2)[length(l2)] <- s
#   }
#   l1    <- c(l1,list(l2))
#   names(l1)[length(l1)] <- c
# }
# 
# format(object.size(l1),units='Mb')
# names(l1)
# names(l1[[1]])
# names(l1[[1]][[1]])
# class(l1[[1]][[1]][[1]])
# dimnames(l1[[1]][[1]][[1]])
# head(l1[[1]][[1]][[1]])
# 
# 
# # names(rl1)
# # rl1$dimensions
# # rl1$dim_combinations
# # names(rl1$variables)
# rl1$variables
# rl1$variables['TLAI']
# names(rl1$data_arrays)
# dimnames(rl1$data_arrays$`lndgrid,time`)
# # rl1$data_arrays
# class(rl1$data_arrays)
# lapply(rl1$data_arrays, class )
# lapply(rl1$data_arrays, dim )
# lapply(rl1$data_arrays, dimnames )


# load ensemble
ens_wd <- paste(root,moddate_ens,caseidprefix_gen_ens,sep='/')
setwd(ens_wd)
# setwd(ensemble)
list.files()
ens_file <- list.files()[1]
# this will have to vary with the run being read and the ensemble type to be processed 
# ens_file  <- 'FACE_ensembleCNP_RD_US-ORN_spins_UQconcatenated.RDS'
l2 <- readRDS(ens_file)
names(l2)

par_vals <- list()

for(c in caseidprefix_var_ens) {
  print(c)
  for(s in sites) {
    print(s)
    for(ca in cases) {
      print(ca)
      
      # read file
      ens_file <- paste0(paste(caseidprefix_gen_ens, c, s, ca, 'UQconcatenated', sep='_' ), '.RDS' )
      l2 <- readRDS(ens_file)
      names(l2)
      
      # process file
      dc <-18
      dimcomb   <- names(l2$data_arrays)[dc]
      a2        <- l2$data_arrays[[dimcomb]]
      da2       <- dim(a2)
      a2_annual <- a2
      dim(a2_annual)      <- c(365, da2[2]/365, da2[3:4] ) 
      a2_annual           <- apply(a2_annual, c(2,3,4), sum )
      dimnames(a2_annual) <- list(year=years[[s]], vars=dimnames(a2)[[3]], uq=dimnames(a2)[[4]] )
      
      # add vars if requested
      if(add_vars) {
        # add_vars_lndtime expects vars to be third dimension of array 
        #for these arrays vars is second so permuting and then back again
        a2_annual <- add_vars_lndtime(aperm(a2_annual, c(1,3,2) ), add_vars_list, transpose=T )
        a2_annual <- aperm(a2_annual, c(1,3,2) )
      }
      
      assign(paste('a2', c, s, ca, sep='_' ), a2_annual )
    }
  }
  
  df1        <- read.table(paste0('vmax',c,'_parm_list'), header=F ) 
  df2        <- read.table(paste0('vmax',c,'_parm_vals'), header=F ) 
  names(df1) <- c('parname', 'PFTnumber', 'min', 'max' )
  names(df2) <- df1[,1]
  
  pvl        <- list(df2)
  par_vals   <- c(par_vals, pvl )
}

# df1       <- read.table(parl_file, header=F ) 
# df2       <- read.table(parv_file, header=F ) 
# df1
# df2
# names(df1) <- c('parname', 'PFTnumber', 'min', 'max' )
# names(df2) <- df1[,1]
# l2$ensemble_par_list <- tibble(df1)
# l2$ensemble_par_vals <- tibble(df2)



#############################################
# plot 

# plots_d_npp <-
#   plot_modobs(l1, df_obs, df_obsse, labs=var_labs$npp, fsite='DUKE', fco2='AMB',
#               sites=sites, years=years, caseidprefix=caseidprefix )


##################################
# ensemble plots

# a2p <- `a2_ECA_US-DUK_aCO2`
# a2p <- `a2_ECA_US-DUK_eCO2`
# a2p <- `a2_RD_US-DUK_aCO2`
# a2p <- `a2_RD_US-DUK_eCO2`
# ylim <- c(0,1900)
# 
# xyplot(a2p[,'NPP',] ~ rep(as.numeric(dimnames(a2p)[[1]]),dim(a2p)[3]), groups=rep(1:dim(a2p)[3],each=dim(a2p)[1]),
#        type='l', ylim=ylim, col=rev(viridis(dim(a2p)[3])) )
# 
# s <- 'US-DUK'
# s <- 'US-ORN'
# a2pa  <- get(paste('a2_ECA',s,'aCO2', sep='_' ))
# a2pe  <- get(paste('a2_ECA',s,'eCO2', sep='_' ))
# a2pa  <- get(paste('a2_RD',s,'aCO2', sep='_' ))
# a2pe  <- get(paste('a2_RD',s,'eCO2', sep='_' ))
# 
# a2pa[,'NPP',]
# 
# a2pea_abs <- a2pe - a2pa 
# ylim_abs  <- c(0,500)
# a2pea_rel <- 1e2*(a2pe/a2pa - 1) 
# ylim_rel  <- c(0,50)
# 
# xyplot(a2pa[,'NPP',] ~ rep(as.numeric(dimnames(a2p)[[1]]),dim(a2p)[3]), groups=rep(1:dim(a2p)[3],each=dim(a2p)[1]),
#        type='l', ylim=ylim, col=rev(viridis(dim(a2p)[3])) )
# xyplot(a2pea_abs[,'NPP',] ~ rep(as.numeric(dimnames(a2p)[[1]]),dim(a2p)[3]), groups=rep(1:dim(a2p)[3],each=dim(a2p)[1]),
#        type='l', ylim=ylim_abs, col=rev(viridis(dim(a2p)[3])) )
# xyplot(a2pea_rel[,'NPP',] ~ rep(as.numeric(dimnames(a2p)[[1]]),dim(a2p)[3]), groups=rep(1:dim(a2p)[3],each=dim(a2p)[1]),
#        type='l', ylim=ylim_rel, col=rev(viridis(dim(a2p)[3])) )
# 
# xyplot(a2pea_abs[,'NO3UPTAKE',] ~ rep(as.numeric(dimnames(a2p)[[1]]),dim(a2p)[3]), groups=rep(1:dim(a2p)[3],each=dim(a2p)[1]),
#        type='l', col=rev(viridis(dim(a2p)[3])) )
# xyplot(a2pea_abs[,'NH4UPTAKE',] ~ rep(as.numeric(dimnames(a2p)[[1]]),dim(a2p)[3]), groups=rep(1:dim(a2p)[3],each=dim(a2p)[1]),
#        type='l', col=rev(viridis(dim(a2p)[3])) )
# xyplot(I(a2pa[,'NO3UPTAKE',] + a2pa[,'NH4UPTAKE',]) ~ rep(as.numeric(dimnames(a2p)[[1]]),dim(a2p)[3]), groups=rep(1:dim(a2p)[3],each=dim(a2p)[1]),
#        type='l', 
#        # ylim=ylim, 
#        col=rev(viridis(dim(a2p)[3])) )
# xyplot(a2pea_abs[,'NPP_FROOT',] ~ rep(as.numeric(dimnames(a2p)[[1]]),dim(a2p)[3]), groups=rep(1:dim(a2p)[3],each=dim(a2p)[1]),
#        type='l', col=rev(viridis(dim(a2p)[3])) )
# xyplot(I(a2pa[,'NPP_FROOT',] / a2pa[,'NPP',]) ~ rep(as.numeric(dimnames(a2p)[[1]]),dim(a2p)[3]), groups=rep(1:dim(a2p)[3],each=dim(a2p)[1]),
#        type='l', 
#        # ylim=ylim, 
#        col=rev(viridis(dim(a2p)[3])) )
# 
# xyplot(I(a2pea_abs[,'NO3UPTAKE',] + a2pea_abs[,'NH4UPTAKE',]) ~ rep(as.numeric(dimnames(a2p)[[1]]),dim(a2p)[3]), groups=rep(1:dim(a2p)[3],each=dim(a2p)[1]),
#        type='l', 
#        col=rev(viridis(dim(a2p)[3])) )



# l2$data_arrays
# names(l2$data_arrays)
# l2$data_arrays$`lndgrid,levgrnd`
# dim(l2$data_arrays$`lndgrid,time`)
# dimnames(l2$data_arrays$`lndgrid,time`)
# 
# dim(`a2_RD_US-DUK_aCO2`)
# `a2_RD_US-DUK_aCO2`[,'NUP',]
# `a2_RD_US-DUK_aCO2`[,'NO3UPTAKE',]
# `a2_RD_US-DUK_aCO2`[,'NH4UPTAKE',]
# `a2_RD_US-DUK_aCO2`[,'NPP',]
# 
# at <- aperm(`a2_RD_US-DUK_aCO2`, c(3,1,2) )
# dim(at)
# sum_vars <- c('NO3UPTAKE','NH4UPTAKE')
# asum     <- apply(at[,,sum_vars,drop=F], 2:1, sum )


labs <- list(
  NPP = list(
    base     = expression("NPP [gC " * m^-2 * " y"^-1 * "]"),
    resp     = expression("NPP response [gC " * m^-2 * " y"^-1 * "]"),
    rel      = expression("NPP response [%]"),
    ylim     = c(0,1900),
    ylim_abs = c(0,500),
    ylim_rel = c(0,50)
  ),
  
  NUP = list(
    base     = expression("N Uptake [gN " * m^-2 * " y"^-1 * "]"),
    resp     = expression("N Uptake response [gN " * m^-2 * " y"^-1 * "]"),
    rel      = expression("N Uptake response [%]"),
    ylim     = c(0,12),
    ylim_abs = c(-1,4),
    ylim_rel = c(-40,50)
  ),
  
  FROOT_alloc_frac = list(
    base     = expression("Fine Root alloc. frac. [-]"),
    resp     = expression("Fine Root alloc. frac. response [-]"),
    rel      = expression("Fine Root alloc. frac. response [%]"),
    ylim     = c(0,0.6),
    ylim_abs = c(-0.15,0.2),
    ylim_rel = c(-60,130)
  ),
  
  LEAF_FROOT_ratio = list(
    base     = expression("Leaf:Fine Root mass [-]"),
    resp     = expression("Leaf:Fine Root mass response [-]"),
    rel      = expression("Leaf:Fine Root mass response [%]"),
    ylim     = c(0.25,7),
    ylim_abs = c(-5,3),
    ylim_rel = c(-100,60)
  )
)


# npp_d_rd <- plot_ensemble_2d()
# npp_d_rd[[1]]
# npp_d_rd[[2]]
# npp_d_rd[[3]]
# 
# npp_d_rd <- plot_ensemble_2d(fsite='ORNL')
# npp_d_rd[[1]]
# npp_d_rd[[2]]
# npp_d_rd[[3]]
# 
# npp_d_rd <- plot_ensemble_2d(fsite='ORNL', case='ECA', plot_obs_regression=T )
# npp_d_rd[[1]]
# npp_d_rd[[2]]
# npp_d_rd[[3]]
# npp_d_rd[[4]]
# 
# npp_d_rd <- plot_ensemble_2d('NPP', fsite='DUKE', case='ECA', plot_obs_regression=T )
# npp_d_rd[[1]]
# 
# 
# 
# nup_d_rd <- plot_ensemble_2d( 'NUP', plotsetup=labs )
# nup_d_rd[[1]]
# nup_d_rd[[2]]
# nup_d_rd[[3]]
# nup_d_rd[[4]]
# 
# fr_d_rd <- plot_ensemble_2d( 'FROOT_alloc_frac', plotsetup=labs )
# fr_d_rd[[1]]
# fr_d_rd[[2]]
# fr_d_rd[[3]]
# fr_d_rd[[4]]
# 
# fr_d_rd <- plot_ensemble_2d( 'LEAF_FROOT_ratio', plotsetup=labs )
# fr_d_rd[[1]]
# fr_d_rd[[2]]
# fr_d_rd[[3]]
# fr_d_rd[[4]]



setwd('figures')
plot_vars <- c('NPP', 'NUP', 'FROOT_alloc_frac', 'LEAF_FROOT_ratio' )
width     <- 6 
height    <- 4 

for(v in plot_vars) {
  i <- 1
  for(c in caseidprefix_var_ens) {
    print(c)
    for(s in c('DUKE', 'ORNL' )) {
      print(s)

      plot_data <- plot_ensemble_2d(pvar=v, fsite=s, case=c, plot_obs_regression=T, plotsetup=labs  )
      plot_list <- if(i==1) list(plot_data) else  c(plot_list, list(plot_data) ) 
      
      # pdf(paste0(paste(v,s,c,sep='_'),'.pdf'), width=width, height=height )
      # print(plot_data[[1]])
      # print(plot_data[[2]])
      # print(plot_data[[3]])
      # print(plot_data[[4]])
      # dev.off()
      
      if(c == caseidprefix_var_ens[1]) {
        plot_data <- plot_ensemble_2d(pvar=v, fsite=s, plot_ensemble=F, plotsetup=labs )
        
        pdf(paste0(paste(v,s,'obs',sep='_'),'.pdf'), width=width, height=height)
        print(plot_data[[1]])
        print(plot_data[[2]])
        print(plot_data[[3]])
        print(plot_data[[4]])
        dev.off()
      }
      
      i <- i + 1
    }
  }
  
  pdf(paste0(v,'.pdf'), width=2*width, height=2*height )
  
  p <- 1
  print(plot_list[[1]][[p]], split=c(1,1,2,2), more=T )
  print(plot_list[[2]][[p]], split=c(2,1,2,2), more=T )
  print(plot_list[[3]][[p]], split=c(1,2,2,2), more=T )
  print(plot_list[[4]][[p]], split=c(2,2,2,2), more=F )
  
  p <- 2
  print(plot_list[[1]][[p]], split=c(1,1,2,2), more=T )
  print(plot_list[[2]][[p]], split=c(2,1,2,2), more=T )
  print(plot_list[[3]][[p]], split=c(1,2,2,2), more=T )
  print(plot_list[[4]][[p]], split=c(2,2,2,2), more=F )

  p <- 3
  print(plot_list[[1]][[p]], split=c(1,1,2,2), more=T )
  print(plot_list[[2]][[p]], split=c(2,1,2,2), more=T )
  print(plot_list[[3]][[p]], split=c(1,2,2,2), more=T )
  print(plot_list[[4]][[p]], split=c(2,2,2,2), more=F )

  p <- 4
  print(plot_list[[1]][[p]], split=c(1,1,2,2), more=T )
  print(plot_list[[2]][[p]], split=c(2,1,2,2), more=T )
  print(plot_list[[3]][[p]], split=c(1,2,2,2), more=T )
  print(plot_list[[4]][[p]], split=c(2,2,2,2), more=F )
  
  dev.off()
  
}

dimnames(`a2_ECA_US-DUK_aCO2`)

df_obs$CO2.real[df_obs$co2=='ELE']/
  df_obs$CO2.real[df_obs$co2=='AMB']

df_obs$CO2.real[df_obs$site=='ORNL'&df_obs$co2=='ELE']
mean(df_obs$CO2.real[df_obs$site=='ORNL'&df_obs$co2=='ELE'])
mean(df_obs$CO2.real[df_obs$site=='ORNL'&df_obs$co2=='AMB'])

df_obs$GR[df_obs$site=='ORNL'&df_obs$co2=='AMB']
df_obs$CFR[df_obs$site=='ORNL'&df_obs$co2=='AMB']


names(df_obs)

xyplot(CO2.real~YEAR|site, df_obs, groups=co2 )
xyplot(GR~YEAR|site, df_obs, groups=co2 )
xyplot(CFR~YEAR|site, df_obs, groups=co2 )


xyplot(1:10~1:10, key=list(x=0,y=1,corner=c(0,1),text=list('ORNL ECA')))






###############################################
# Load structure ensemble
wdo_ens_struct <- paste(root,moddate_ens_struct,'plots',sep='/')  

setwd(paste(root,moddate_ens_struct,caseidprefix_gen_ens_struct,sep='/'))
setwd(ensemble)
list.files()
ens_file <- 'FACE_ensembleCNP_RD_US-ORN_spins_UQconcatenated.RDS'
l2 <- readRDS(ens_file)
names(l2)
parl_file <- 'structure1_parm_list'
parv_file <- 'structure1_parm_vals'
df1       <- read.table(parl_file, header=F ) 
df2       <- read.table(parv_file, header=F ) 
df1
df2
names(df1) <- c('parname', 'PFTnumber', 'min', 'max' )
names(df2) <- df1[,1]
l2$ensemble_par_list <- tibble(df1)
l2$ensemble_par_vals <- tibble(df2)

names(l2$data_arrays)
names(l2$data_arrays)[19]
dimcomb <- names(l2$data_arrays)[19]
a1    <- l2$data_arrays[[dimcomb]]
dim(a1)
dimnames(a1)
dimcomb <- names(l2$data_arrays)[18]
a2    <- l2$data_arrays[[dimcomb]]
dim(a2)
dimnames(a2)

p1 <- 
  xyplot(a2[,,'TLAI',]~rep(1:dim(a2)[2],dim(a2)[4]), groups=rep(1:dim(a2)[3],each=dim(a2)[2]),
         xlab='year', ylab='LAI',
         type='l'  )

pdf('TLAI.pdf', 6, 5 )
print(p1)
dev.off()


# create LAI_SCLS variable
a1[,,,'DDBH_CANOPY_SCLS',] <- 
  a1[,,,'LAI_CANOPY_SCLS',] +
  a1[,,,'LAI_UNDERSTORY_SCLS',]
dimnames(a1)$vars[3] <- 'LAI_SCLS'

# ensemble par vals
names(l2$ensemble_par_vals)
tibble(l2$ensemble_par_vals) 


a1   <- a1
vid  <- 2
na0  <- T 
log  <- F
pa2sub    <- NULL
var2      <- l2$dim_variables$fates_levscls$vals
var_deets <- l2$variables[[dimnames(a1)$vars[vid]]]
fates_dim <- names(dimnames(a1)[2])


dimnames(a1)$vars
which(dimnames(a1)$vars == paste0('LAI','_SCLS'))

plots <- plot3d_ensemble(a1, 3, var_label="LAI by size class [m2/m2]", zmax=9 )
plots[[1]]
plots[[2]]
plots[[4]]
plots[[3]]


setwd(wdo_ens_struct)
vars_lab   <- c('AGB', 'LAI', 'LAI_CANOPY', 'LAI_UNDERSTORY', 'BA', 'NPLANT' )
zmaxs      <- c(NA, 9, 9, 9, NA, NA )
var_labels <- c(NA, "LAI by size class [m2/m2]", "Canopy LAI by size class [m2/m2]", "Understory LAI by size class [m2/m2]", NA, NA )
for(v1 in 1:length(vars_lab)) {

  var1 <- vars_lab[v1]
  print(var1)
  
  logt <- F
  if(var1%in%c('NPLANT')) logt <- T
  plots <- plot3d_ensemble(a1, which(dimnames(a1)$vars == paste0(var1,'_SCLS')), logt=logt, var_label=var_labels[v1], zmax=zmaxs[v1] )
  
  pdf(paste0('Size_class_',var1,'.pdf'), 14, 10)
  print(plots[[1]])
  dev.off()

  pdf(paste0('Size_class_',var1,'_2d.pdf'), 6, 5 )
  print(update(plots[[3]], legend=NULL ))
  dev.off()
}


tibble(l2$ensemble_par_vals) 
names(l2$ensemble_par_vals)







# # met data
# # for PI meeting poster
# ##################################
# 
# 
# setwd(paste0(root,moddate,'/plots'))
# # pdf('NPP_reponse_Duke.pdf', width=6.5, height=3.5 )
# # plot_response(lp_resp, subset(dfo_npp_co2, site=='DUKE'), site='US-DUK' )
# # dev.off()
# 
# # pdf('NPP_reponse_ORNL.pdf', width=6.5, height=3.5 )
# # plot_response(lp_resp, subset(dfo_npp_co2, site=='ORNL'), site='US-ORN' )
# # dev.off()
# 
# data_func = 'calc_responseplot_vectors'
# resp_func = 'percent_array'
# plot_func = 'plot_co2response_sites'
# var  = 'GPP'
# ylab = expression("GPP response [%]")
# 
# lp <- calc_plot_vectors(l1, 'NPP', caseidprefix, sites, years )
# plot_co2response_sites(lp, colrs=rep(c('blue','red'),each=2), ltypes=c(1,2), leg_cols=2,
#                        ylab=expression("NPP [gC " * m^-2 * " y"^-1 * "]"))  
# 
# colrs <- magma(3)
# xyplot(l1[] ~ lp$vx | lp[[outpanel]], groups=lp[[inpanel]],
#        type='l', scales=list(tck=c(-0.5,-0.5), alternating=F ), as.table=T,
#        xlab='Year',
#        par.settings=simpleTheme(col=colrs, lwd=2, lty=ltypes ), 
#        strip=strip.custom(bg='grey90',par.strip.text=list(cex=0.75)),
#        auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1, columns=leg_cols,
#                      border=T, cex=0.75, background='white'))
# 
# 
# sitecolrs <- c('blue','red')
# 
# lp_rain <- calc_plot_vectors2(l1, 'RAIN', caseidprefix[1], sites, years )
# pr <- 
#   xyplot(lp_rain$vy ~ lp_rain$vx, groups=lp_rain$sites,
#          type='l', scales=list(tck=c(-0.5,-0.5), alternating=F, x=list(at=seq(1996,2008,4)) ),
#          xlab='Year', ylab=expression('Precipitation [mm '*y^-1*']'),
#          par.settings=simpleTheme(col=sitecolrs, lwd=2, lty=1 )#, 
#          # auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1, columns=1,
#          #               border=T, cex=0.75, background='white')
#   )
# pr
# 
# lp_temp <- calc_plot_vectors2(l1, 'TBOT', caseidprefix[1], sites, years )
# pt <- 
#   xyplot(lp_temp$vy ~ lp_temp$vx, groups=lp_temp$sites,
#          type='l', scales=list(tck=c(-0.5,-0.5), alternating=F, x=list(at=seq(1996,2008,4)) ),
#          xlab='Year', ylab=expression('Air Temperature ['*degree*'C]'),
#          par.settings=simpleTheme(col=sitecolrs, lwd=2, lty=1 ),
#          auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1, columns=1,
#                        border=T, cex=0.75, background='white')
#   )
# pt 
# 
# lp_swrad <- calc_plot_vectors2(l1, 'FSDS', caseidprefix[1], sites, years )
# psw <- 
#   xyplot(lp_swrad$vy ~ lp_swrad$vx, groups=lp_swrad$sites,
#          type='l', scales=list(tck=c(-0.5,-0.5), alternating=F, x=list(at=seq(1996,2008,4)) ),
#          xlab='Year', ylab=expression('SW Radiation ['*W*m^-2*']'),
#          par.settings=simpleTheme(col=sitecolrs, lwd=2, lty=1 )#, 
#          # auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1, columns=1,
#          #               border=T, cex=0.75, background='white')
#   )
# psw
# 
# setwd(paste0(root,moddate,'/plots'))
# pdf('met_data.pdf', width=10, height=3.5 )
# print(pt, split=c(1,1,3,1), more=T )
# print(pr, split=c(2,1,3,1), more=T )
# print(psw, split=c(3,1,3,1), more=F )
# dev.off()
# 
# 

### END ###