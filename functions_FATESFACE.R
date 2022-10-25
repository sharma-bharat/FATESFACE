############################
# functions




############################



# data processing functions
############################

# variables to add to a data array and how to calculate them
add_vars_list <- list(
  
  WUEveg = list(
    name = 'WUEveg',
    sum_vars = c('GPP'),
    div_sumvars = c('QVEGT')
  ),
  
  WUEecosystem = list(
    name = 'WUEecosystem',
    sum_vars = c('GPP'),
    div_sumvars = c('QVEGT','QVEGE','QSOIL')
  ),
  
  iWUE = list(
    name = 'iWUE',
    sum_vars = list(
      sum_vars=c('GPP'),
      scale=1e12/(86400*12) # convert g C umol-1 H2O to umol C mol-1 H2O
    ),
    div_prodvars = c('C_STOMATA','TLAI')
  ),

  GC = list(
    name = 'GC',
    product_vars=c('C_STOMATA','TLAI')
  ),
  
  Biomass_production = list(
    name = 'Biomass_production',
    sum_vars=c('NPP_LEAF','NPP_FROOT','NPP_CROOT','NPP_FROOT','NPP_STEM','NPP_SEED')
  ),
  
  Biomass_production_incStore = list(
    name = 'Biomass_production_incStore',
    sum_vars=c('NPP_LEAF','NPP_FROOT','NPP_CROOT','NPP_FROOT','NPP_STEM','NPP_SEED','NPP_STOR')
  ),
  
  NUP = list(
    name = 'NUP',
    sum_vars=c('NH4UPTAKE','NO3UPTAKE')
  ),
  
  FROOT_alloc_frac = list(
    name = 'FROOT_alloc_frac',
    sum_vars = c('NPP_FROOT'),
    div_sumvars=c('NPP_LEAF','NPP_FROOT','NPP_CROOT','NPP_FROOT','NPP_STEM','NPP_SEED')
    # div_sumvars = c('Biomass_production')
  ),
  
  LEAF_FROOT_ratio = list(
    name = 'LEAF_FROOT_ratio',
    sum_vars = c('ED_bleaf'),
    div_sumvars = c('ED_bfineroot')
  )
  
)


# function to calculate above described variables and add them to an array 
add_vars_lndtime <- function(a3d, vars_list, days=365, div_zero=1e-2, transpose=F ) {
  
  # create output array - extending vars dimension by length(vars_list)
  aodim      <- adim <- dim(a3d)
  aodimnames <- dimnames(a3d)
  vss        <- which(names(adim)=='vars')
  vdimlen    <- aodim[vss]
  aodim[vss] <- vdimlen + length(vars_list) 
  ao         <- array(dim=aodim)
  
  # add existing data to output array
  ao[1:adim[1],1:adim[2],1:adim[3]] <- a3d
  
  # loop over element sof vars_list
  for(i in 1:length(vars_list)) {
    
    # attach vars list data
    sum_vars      <- vars_list[[i]]$sum_vars
    product_vars  <- vars_list[[i]]$product_vars
    subtract_vars <- vars_list[[i]]$subtract_vars
    div_sumvars   <- vars_list[[i]]$div_sumvars
    div_prodvars  <- vars_list[[i]]$div_prodvars
    
    # scaling of sum_vars
    sum_vars.scale <- 1
    if(is.list(sum_vars)) {sum_vars.scale <- sum_vars$scale*1/days; sum_vars <- sum_vars$sum_vars }
    
    # averaging / summing
    asum    <- apply(a3d[,,sum_vars,drop=F]*sum_vars.scale, 2:1, sum )
    nv_name <- paste0('sum(',paste(sum_vars,collapse=','),')')
    
    if(!is.null(product_vars)) {
      asum    <- apply(a3d[,,product_vars,drop=F], 2:1, prod )
      nv_name <- paste0('prod(',paste(product_vars,collapse=','),')')
    }
    
    if(!is.null(subtract_vars)) {
      asub    <- apply(a3d[,,subtract_vars,drop=F], 2:1, sum )
      asum    <- asum[drop=F] - asub[drop=F]
      nv_name <- paste0(nv_name,' - sum(',paste(subtract_vars,collapse=','),')')
      
    } else if(!is.null(div_sumvars)) {
      adiv <- apply(a3d[,,div_sumvars,drop=F], 2:1, sum )
      adiv[adiv<div_zero] <- NA
      asum    <- asum[drop=F] / adiv[drop=F]
      nv_name <- paste0(nv_name,' / sum(',paste(div_sumvars,collapse=','),')')
      
    } else if(!is.null(div_prodvars)) {
      adiv <- apply(a3d[,,div_prodvars,drop=F], 2:1, prod )
      adiv[adiv<div_zero] <- NA
      asum    <- asum[drop=F] / adiv[drop=F]
      nv_name <- paste0(nv_name,' / prod(',paste(div_prodvars,collapse=','),')')
    }
    
    # add to array
    if(transpose) asum <- t(asum)
    ao[,,vdimlen+i] <- asum
    aodimnames[[vss]] <- c(aodimnames[[vss]], vars_list[[i]]$name )
    # aodimnames[[vss]] <- c(aodimnames[[vss]], names(vars_list[i]) )
    
    print(dim(ao))
    print(dimnames(ao))
    print(aodimnames)
  }   
  
  dimnames(ao) <- aodimnames
  ao
}


# CO2 response calculation functions
# beta after Walker et al 2021
beta_array <- function(av_aco2, av_eco2, ac_aco2, ac_eco2 ) {
  log(av_eco2/av_aco2) / log(ac_eco2/ac_aco2)
}

# percent
percent_array <- function(av_aco2, av_eco2, ... ) {
  100 * (av_eco2/av_aco2 - 1) 
}

# absolute
abs_array <- function(av_aco2, av_eco2, ... ) {
  av_eco2 - av_aco2 
}



# variable processing functions for figures
# # for a 2-level list calculate co2 responses and process for xyplot
# calc_responseplot_vectors_2lev <- function(l1, var, sites, time, func=beta_array ) {
#   vy <- numeric(length(unlist(time)))
#   vx <- numeric(length(unlist(time)))
#   vg <- numeric(length(unlist(time)))
#   
#   s1 <- 1
#   for(s in sites) {
#     av_aco2 <- l1[[s]]$aCO2[,,var]
#     av_eco2 <- l1[[s]]$eCO2[,,var]
#     ac_aco2 <- l1[[s]]$aCO2[,,'PCO2']
#     ac_eco2 <- l1[[s]]$eCO2[,,'PCO2']
#     
#     s2 <- s1 + length(time[[s]]) - 1
#     vy[s1:s2] <- func(av_aco2, av_eco2, ac_aco2, ac_eco2 )
#     vx[s1:s2] <- time[[s]]
#     vg[s1:s2] <- rep(s, length(time[[s]]) )
#     s1 <- s2 + 1
#   }
#   
#   list(vx=vx, vy=vy, vg=vg )
# }

# # for a 2-level list process for xyplot
# calc_plot_vectors_2lev <- function(l1, var, sites, time ) {
#   lengthv <- length(unlist(time))*2
#   vy <- numeric(lengthv)
#   vx <- numeric(lengthv)
#   vg <- numeric(lengthv)
#   
#   s1 <- 1
#   for(s in sites) {
#     av_aco2 <- l1[[s]]$aCO2[,,var]
#     av_eco2 <- l1[[s]]$eCO2[,,var]
#     
#     s2 <- s1 + 2*length(time[[s]]) - 1
#     vy[s1:s2] <- c(av_aco2, av_eco2 )
#     vx[s1:s2] <- rep(time[[s]], 2 )
#     vg[s1:s2] <- rep(paste(s, c('aCO2','eCO2'), sep='_'), each=length(time[[s]]) )
#     s1 <- s2 + 1
#   }
#   
#   list(vx=vx, vy=vy, vg=vg )
# }


# for a 3-level list calc co2 response and process for xyplot
calc_responseplot_vectors <- function(l1, var, caseidprefix, sites, time, func=NULL, ... ) {
  
  all_func <- F
  func <- if(is.null(func)) beta_array else if(func=='all') all_func <- T else get(func)
  
  lengthsites <- length(unlist(time))
  lengthv     <- lengthsites * length(caseidprefix)
  vy <- vy.1 <- vy.2 <- vx <- vg <- vp <- numeric(lengthv)
  
  c1 <- 1
  for(c in caseidprefix) {
    c2 <- c1 + lengthsites - 1
    vp[c1:c2] <- c
    
    s1 <- 1
    for(s in sites) {
      if(var%in%dimnames(l1[[c]][[s]]$aCO2)$vars ) {
        av_aco2 <- l1[[c]][[s]]$aCO2[,,var]
        av_eco2 <- l1[[c]][[s]]$eCO2[,,var]
      } else {
        av_aco2 <- rep(NA, length(time[[s]]) )
        av_eco2 <- rep(NA, length(time[[s]]) )
      }

      ac_aco2 <- l1[[c]][[s]]$aCO2[,,'PCO2']
      ac_eco2 <- l1[[c]][[s]]$eCO2[,,'PCO2']
      
      s2 <- s1 + length(time[[s]]) - 1
      if(!all_func) {
        vy[c1:c2][s1:s2]   <- func(av_aco2, av_eco2, ac_aco2, ac_eco2 )
      } else {
        vy[c1:c2][s1:s2]   <- abs_array(av_aco2, av_eco2, ac_aco2, ac_eco2 )
        vy.1[c1:c2][s1:s2] <- percent_array(av_aco2, av_eco2, ac_aco2, ac_eco2 )
        vy.2[c1:c2][s1:s2] <- beta_array(av_aco2, av_eco2, ac_aco2, ac_eco2 )
      }
      
      vx[c1:c2][s1:s2] <- time[[s]]
      vg[c1:c2][s1:s2] <- rep(s, length(time[[s]]) )
      s1 <- s2 + 1
    }
    c1 <- c2 + 1
  }
  
  olist <- 
    if(!all_func) {
      list(vx=vx, vy=vy, sites=vg, caseid=vp )
    } else {
      list(vx=vx, vy=vy, vy_perc=vy.1, vy_beta=vy.2, sites=vg, caseid=vp )
    }
  olist
}


calc_plot_vectors <- function(l1, var, caseidprefix, sites, time, ... ) {
  
  lengthsites <- 2 * length(unlist(time))
  lengthv     <- lengthsites * length(caseidprefix)
  vy <- numeric(lengthv)
  vx <- numeric(lengthv)
  vg <- numeric(lengthv)
  vp <- numeric(lengthv)
  
  c1 <- 1
  for(c in caseidprefix) {
    c2 <- c1 + lengthsites - 1
    vp[c1:c2] <- c
    
    s1 <- 1
    for(s in sites) {
      if(var%in%dimnames(l1[[c]][[s]]$aCO2)$vars ) {
        av_aco2 <- l1[[c]][[s]]$aCO2[,,var]
        av_eco2 <- l1[[c]][[s]]$eCO2[,,var]
      } else {
        av_aco2 <- rep(NA, length(time[[s]]) )
        av_eco2 <- rep(NA, length(time[[s]]) )
      }
       
      s2 <- s1 + 2*length(time[[s]]) - 1
      vy[c1:c2][s1:s2] <- c(av_aco2, av_eco2 )
      vx[c1:c2][s1:s2] <- rep(time[[s]], 2 )
      vg[c1:c2][s1:s2] <- rep(paste(s, c('aCO2','eCO2'), sep='_'), each=length(time[[s]]) )
      s1 <- s2 + 1
    }
    c1 <- c2 + 1
  }
  
  list(vx=vx, vy=vy, sites=vg, caseid=vp )
}


calc_plot_vectors2 <- function(l1, var, caseidprefix, sites, time, ... ) {
  
  lengthsites <- length(unlist(time))
  lengthv     <- lengthsites * length(caseidprefix)
  vy <- numeric(lengthv)
  vx <- numeric(lengthv)
  vg <- numeric(lengthv)
  vp <- numeric(lengthv)
  
  c1 <- 1
  for(c in caseidprefix) {
    c2 <- c1 + lengthsites - 1
    vp[c1:c2] <- c
    
    s1 <- 1
    for(s in sites) {
      if(var%in%dimnames(l1[[c]][[s]]$aCO2)$vars ) {
        av_aco2 <- l1[[c]][[s]]$aCO2[,,var]
        av_eco2 <- l1[[c]][[s]]$eCO2[,,var]
      } else {
        av_aco2 <- rep(NA, length(time[[s]]) )
        av_eco2 <- rep(NA, length(time[[s]]) )
      }
        
      s2 <- s1 + length(time[[s]]) - 1
      vy[c1:c2][s1:s2] <- av_aco2
      vx[c1:c2][s1:s2] <- time[[s]]
      vg[c1:c2][s1:s2] <- rep(s, each=length(time[[s]]) )
      s1 <- s2 + 1
    }
    c1 <- c2 + 1
  }
  
  list(vx=vx, vy=vy, sites=vg, caseid=vp )
}


# individual figure generation functions
# plot aCO2 and eCO2 treatments and all sites on the same panel with a panel for each caseidprefix 
# plot_aeco2_sites <- function(lp, ... ) {
#   xyplot(lp$vy ~ lp$vx | lp$caseid, groups=lp$sites,
#          type='l', scales=list(tck=c(-0.5,-0.5), alternating=F ),
#          xlab='Year',
#          par.settings=simpleTheme(col=vcol, lwd=2, lty = vlty ), 
#          auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1, columns=leg_cols, 
#                        border=T, cex=0.75, background='white' ),
#          ...)
# }


# plot model data
#################################

# plot a calculated co2 response with flexible specification of within-panel and across-panel factors 
plot_co2response_sites <- function(lp, inpanel=NULL, 
                                   colrs=NULL, ltypes=NULL, leg_cols=NULL, lalpha=NULL,
                                   ... ) {
  
  if(is.null(inpanel)) inpanel <- 'sites'
  outpanel <- if(inpanel=='sites') 'caseid' else 'sites'  
  colrs    <- if(is.null(colrs)) 'blue' else if(is.function(colrs)) colrs(length(unique(lp[[inpanel]]))) else colrs
  ltypes   <- if(is.null(ltypes)) 1 else ltypes
  lalpha   <- if(is.null(lalpha)) 1 else lalpha
  
  xyplot(lp$vy ~ lp$vx | lp[[outpanel]], groups=lp[[inpanel]],
         type='l', scales=list(tck=c(-0.5,-0.5), alternating=F ), as.table=T,
         xlab='Year',
         par.settings=simpleTheme(col=colrs, lwd=2, lty=ltypes, alpha=lalpha ), 
         strip=strip.custom(bg='grey90',par.strip.text=list(cex=0.75)),
         auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1, columns=leg_cols,
                       border=T, cex=0.75, background='white'),
         ...)
}


# plot generation and organisation functions
# generate a list of figures for ldata data and described by the plotlist list, see plotlist.R
make_figures <- function(ldata, plotlist, print2screen=F, ... ) {
  
  lapply(plotlist, function(l) {
    print('')
    print('Making figure:')
    print(l$var)
    
    ldata1 <- get(l$data_func)(ldata, l$var, func=l$resp_func, ... )
    p      <- get(l$plot_func)(ldata1, ylab=l$ylab, inpanel=l$inpanel, 
                               colrs=l$colrs, ltypes=l$ltypes, leg_cols=l$leg_cols, lalpha=l$lalpha )
    if(print2screen) print(p)
    p
  })
}

# create a pdf of figure - need to add png option
plot_figures <- function(plots, plotname='plots', func='pdf',
                         nper_page=3, width=8.5, height=11  ) {
  print('', quote=F )
  print(paste('Saving figures to:',plotname), quote=F )
  
  plotname <- paste0(plotname,'.',func)
  
  if(func=='pdf') {
    pdf(plotname, width=width, height=height )
  } else if(func=='png') {
    # png does not work with multiple pages
    png(plotname, width=width, height=height, units='in', res=226 )
  }
  
  lapply(1:length(plots), function(p) {
    print(plots[p], 
          split=c(1,p%%nper_page+if(p%%nper_page==0) nper_page else 0,1,nper_page), 
          more=p%%nper_page )
    numeric(0)
  })
  dev.off()
}


# plot model & obs data
#################################

fsite <- 'DUKE'
fvar  <- 'NPP'
fco2  <- 'AMB'

plot_modobs <- function(lmod=l1, df_obs, df_obsse, labs,
                        fsite, fvar=NULL, fvarmod=fvar, fco2, aco2.se=2.5, eco2.se=10,
                        sites, years, caseidprefix
                        ) {
  
  if(fsite=='DUKE') fsite_mod <- 'US-DUK'
  if(fsite=='ORNL') fsite_mod <- 'US-ORN'
  if(fco2=='AMB')   fsite_mod_co2 <- paste(fsite_mod,'aCO2',sep='_')
  if(fco2=='ELE')   fsite_mod_co2 <- paste(fsite_mod,'eCO2',sep='_')
  print(fsite_mod_co2)

  if(is.null(fvar)) {
    fvar    <- labs$fvar_obs
    fvarmod <- labs$fvar_mod
  }
  print(paste(fvar,fvarmod))
  
  
  # extract model data and calculate responses
  lp      <- calc_plot_vectors(lmod, fvarmod, caseidprefix, sites, years )
  lp_resp <- calc_responseplot_vectors(lmod, fvarmod, caseidprefix, sites, years, func='all' )
  
  
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
    filter(co2==fco2 ) 
  
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
  
  
  # plots - abient or elevate treatment ... could modify to do both.
  pamb <- 
    xyplot(lp$vy ~ lp$vx, groups=lp$caseid, subset=lp$sites==fsite_mod_co2,
           type='l', scales=list(tck=c(-0.5,-0.5), alternating=F, x=list(at=seq(1996,2008,4)) ),
           xlab='Year', xlim=c(1995,2009),
           ylab=labs$amb$ylab, ylim=labs$amb$ylim,
           par.settings=simpleTheme(col=viridis(3), lwd=2, lty=1 ),
           panel=function(...) {
             panel.polygon(c(dfoa$YEAR,rev(dfoa$YEAR)),
                           c(dfoa$vy+1.96*dfoa$vy.se, rev(dfoa$vy-1.96*dfoa$vy.se)),
                           border=F, col='grey90')
             panel.polygon(c(dfoa$YEAR,rev(dfoa$YEAR)),
                           c(dfoa$vy+dfoa$vy.se, rev(dfoa$vy-dfoa$vy.se)),
                           border=F, col='grey80')
             panel.points(dfoa$YEAR,dfoa$vy, type='l', col='black', lwd=2 )
             panel.xyplot(...)
           },
           auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1, columns=3,
                         border=T, cex=0.75, background='white')
    )
  pamb
  
  # absolute response
  presp1 <- 
    xyplot(lp_resp$vy ~ lp_resp$vx, groups=lp_resp$caseid, subset=lp_resp$sites==fsite_mod,
           type='l', scales=list(tck=c(-0.5,-0.5), alternating=F, x=list(at=seq(1996,2008,4)) ),
           xlab='Year', xlim=c(1995,2009),
           ylab=labs$resp$ylab, ylim=labs$resp$ylim,
           par.settings=simpleTheme(col=viridis(3), lwd=2, lty=1 ),
           panel=function(...) {
             panel.polygon(c(dfo_resp$YEAR,rev(dfo_resp$YEAR)),
                           c(dfo_resp$resp_abs+1.96*dfo_resp$resp_abs.se, rev(dfo_resp$resp_abs-1.96*dfo_resp$resp_abs.se)),
                           border=F, col='grey90')
             panel.polygon(c(dfo_resp$YEAR, rev(dfo_resp$YEAR)),
                           c(dfo_resp$resp_abs+dfo_resp$resp_abs.se, rev(dfo_resp$resp_abs-dfo_resp$resp_abs.se)),
                           border=F, col='grey80')
             panel.points(dfo_resp$YEAR,dfo_resp$resp_abs, type='l', col='black', lwd=2 )
             panel.xyplot(...)
           },
           auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1, columns=3,
                         border=T, cex=0.75, background='white')
    )
  presp1
  
  # percent response
  presp2 <- 
    xyplot(lp_resp$vy_perc ~ lp_resp$vx, groups=lp_resp$caseid, subset=lp_resp$sites==fsite_mod,
           type='l', scales=list(tck=c(-0.5,-0.5), alternating=F, x=list(at=seq(1996,2008,4)) ),
           xlab='Year', xlim=c(1995,2009), 
           ylab=labs$perc$ylab, ylim=labs$perc$ylim,
           par.settings=simpleTheme(col=viridis(3), lwd=2, lty=1 ),
           panel=function(...) {
             panel.polygon(c(dfo_resp$YEAR, rev(dfo_resp$YEAR) ),
                           c(dfo_resp$resp_pct+1.96*dfo_resp$resp_pct.se, rev(dfo_resp$resp_pct-1.96*dfo_resp$resp_pct.se)),
                           border=F, col='grey90')
             panel.polygon(c(dfo_resp$YEAR, rev(dfo_resp$YEAR)),
                           c(dfo_resp$resp_pct+dfo_resp$resp_pct.se, rev(dfo_resp$resp_pct-dfo_resp$resp_pct.se)),
                           border=F, col='grey80')
             panel.points(dfo_resp$YEAR,dfo_resp$resp_pct, type='l', col='black', lwd=2 )
             panel.xyplot(...)
           },
           auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1, columns=3,
                         border=T, cex=0.75, background='white')
    )
  presp2
  
  # beta response
  presp3 <- 
    xyplot(lp_resp$vy_beta ~ lp_resp$vx, groups=lp_resp$caseid, subset=lp_resp$sites==fsite_mod,
           type='l', scales=list(tck=c(-0.5,-0.5), alternating=F, x=list(at=seq(1996,2008,4)) ),
           xlab='Year', xlim=c(1995,2009), 
           ylab=labs$beta$ylab, ylim=labs$beta$ylim,
           par.settings=simpleTheme(col=viridis(3), lwd=2, lty=1 ),
           panel=function(...) {
             panel.polygon(c(dfo_resp$YEAR,rev(dfo_resp$YEAR)),
                           c(dfo_resp$resp_beta+1.96*dfo_resp$resp_beta.se, rev(dfo_resp$resp_beta-1.96*dfo_resp$resp_beta.se)),
                           border=F, col='grey90')
             panel.polygon(c(dfo_resp$YEAR, rev(dfo_resp$YEAR)),
                           c(dfo_resp$resp_beta+dfo_resp$resp_beta.se, rev(dfo_resp$resp_beta-dfo_resp$resp_beta.se)),
                           border=F, col='grey80')
             panel.points(dfo_resp$YEAR,dfo_resp$resp_beta, type='l', col='black', lwd=2 )
             panel.xyplot(...)
           },
           auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1, columns=3,
                         border=T, cex=0.75, background='white')
    )
  presp3
  
  
  list(pamb,presp1,presp2,presp3, dfo, dfo_resp )
}


plot_modobs_ensemble <- function(lmod=l1, df_obs, df_obsse, labs,
                                 fsite, fvar=NULL, fvarmod=fvar, fco2, aco2.se=2.5, eco2.se=10,
                                 sites, years, caseidprefix
) {
  
  if(fsite=='DUKE') fsite_mod <- 'US-DUK'
  if(fsite=='ORNL') fsite_mod <- 'US-ORN'
  if(fco2=='AMB')   fsite_mod_co2 <- paste(fsite_mod,'aCO2',sep='_')
  if(fco2=='ELE')   fsite_mod_co2 <- paste(fsite_mod,'eCO2',sep='_')
  print(fsite_mod_co2)
  
  if(is.null(fvar)) {
    fvar    <- labs$fvar_obs
    fvarmod <- labs$fvar_mod
  }
  print(paste(fvar,fvarmod))
  
  
  # extract model data and calculate responses
  lp      <- calc_plot_vectors(lmod, fvarmod, caseidprefix, sites, years )
  lp_resp <- calc_responseplot_vectors(lmod, fvarmod, caseidprefix, sites, years, func='all' )
  
  
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
    filter(co2==fco2 ) 
  
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
  
  # subset for non-missing values
  dfoa     <- dfoa[is.finite(dfoa$vy),]
  dfo_resp <- dfo_resp[is.finite(dfo_resp$vy),]
  
  
  # plots - abient or elevate treatment ... could modify to do both.
  pamb <- 
    xyplot(lp$vy ~ lp$vx, groups=lp$caseid, subset=lp$sites==fsite_mod_co2,
           type='l', scales=list(tck=c(-0.5,-0.5), alternating=F, x=list(at=seq(1996,2008,4)) ),
           xlab='Year', xlim=c(1995,2009),
           ylab=labs$amb$ylab, ylim=labs$amb$ylim,
           par.settings=simpleTheme(col=viridis(3), lwd=2, lty=1 ),
           panel=function(...) {
             panel.polygon(c(dfoa$YEAR,rev(dfoa$YEAR)),
                           c(dfoa$vy+1.96*dfoa$vy.se, rev(dfoa$vy-1.96*dfoa$vy.se)),
                           border=F, col='grey90')
             panel.polygon(c(dfoa$YEAR,rev(dfoa$YEAR)),
                           c(dfoa$vy+dfoa$vy.se, rev(dfoa$vy-dfoa$vy.se)),
                           border=F, col='grey80')
             panel.points(dfoa$YEAR,dfoa$vy, type='l', col='black', lwd=2 )
             panel.xyplot(...)
           },
           auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1, columns=3,
                         border=T, cex=0.75, background='white')
    )
  pamb
  
  # absolute response
  presp1 <- 
    xyplot(lp_resp$vy ~ lp_resp$vx, groups=lp_resp$caseid, subset=lp_resp$sites==fsite_mod,
           type='l', scales=list(tck=c(-0.5,-0.5), alternating=F, x=list(at=seq(1996,2008,4)) ),
           xlab='Year', xlim=c(1995,2009),
           ylab=labs$resp$ylab, ylim=labs$resp$ylim,
           par.settings=simpleTheme(col=viridis(3), lwd=2, lty=1 ),
           panel=function(...) {
             panel.polygon(c(dfo_resp$YEAR,rev(dfo_resp$YEAR)),
                           c(dfo_resp$resp_abs+1.96*dfo_resp$resp_abs.se, rev(dfo_resp$resp_abs-1.96*dfo_resp$resp_abs.se)),
                           border=F, col='grey90')
             panel.polygon(c(dfo_resp$YEAR, rev(dfo_resp$YEAR)),
                           c(dfo_resp$resp_abs+dfo_resp$resp_abs.se, rev(dfo_resp$resp_abs-dfo_resp$resp_abs.se)),
                           border=F, col='grey80')
             panel.points(dfo_resp$YEAR,dfo_resp$resp_abs, type='l', col='black', lwd=2 )
             panel.xyplot(...)
           },
           auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1, columns=3,
                         border=T, cex=0.75, background='white')
    )
  presp1
  
  # percent response
  presp2 <- 
    xyplot(lp_resp$vy_perc ~ lp_resp$vx, groups=lp_resp$caseid, subset=lp_resp$sites==fsite_mod,
           type='l', scales=list(tck=c(-0.5,-0.5), alternating=F, x=list(at=seq(1996,2008,4)) ),
           xlab='Year', xlim=c(1995,2009), 
           ylab=labs$perc$ylab, ylim=labs$perc$ylim,
           par.settings=simpleTheme(col=viridis(3), lwd=2, lty=1 ),
           panel=function(...) {
             panel.polygon(c(dfo_resp$YEAR, rev(dfo_resp$YEAR) ),
                           c(dfo_resp$resp_pct+1.96*dfo_resp$resp_pct.se, rev(dfo_resp$resp_pct-1.96*dfo_resp$resp_pct.se)),
                           border=F, col='grey90')
             panel.polygon(c(dfo_resp$YEAR, rev(dfo_resp$YEAR)),
                           c(dfo_resp$resp_pct+dfo_resp$resp_pct.se, rev(dfo_resp$resp_pct-dfo_resp$resp_pct.se)),
                           border=F, col='grey80')
             panel.points(dfo_resp$YEAR,dfo_resp$resp_pct, type='l', col='black', lwd=2 )
             panel.xyplot(...)
           },
           auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1, columns=3,
                         border=T, cex=0.75, background='white')
    )
  presp2
  
  # beta response
  presp3 <- 
    xyplot(lp_resp$vy_beta ~ lp_resp$vx, groups=lp_resp$caseid, subset=lp_resp$sites==fsite_mod,
           type='l', scales=list(tck=c(-0.5,-0.5), alternating=F, x=list(at=seq(1996,2008,4)) ),
           xlab='Year', xlim=c(1995,2009), 
           ylab=labs$beta$ylab, ylim=labs$beta$ylim,
           par.settings=simpleTheme(col=viridis(3), lwd=2, lty=1 ),
           panel=function(...) {
             panel.polygon(c(dfo_resp$YEAR,rev(dfo_resp$YEAR)),
                           c(dfo_resp$resp_beta+1.96*dfo_resp$resp_beta.se, rev(dfo_resp$resp_beta-1.96*dfo_resp$resp_beta.se)),
                           border=F, col='grey90')
             panel.polygon(c(dfo_resp$YEAR, rev(dfo_resp$YEAR)),
                           c(dfo_resp$resp_beta+dfo_resp$resp_beta.se, rev(dfo_resp$resp_beta-dfo_resp$resp_beta.se)),
                           border=F, col='grey80')
             panel.points(dfo_resp$YEAR,dfo_resp$resp_beta, type='l', col='black', lwd=2 )
             panel.xyplot(...)
           },
           auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1, columns=3,
                         border=T, cex=0.75, background='white')
    )
  presp3
  
  
  list(pamb,presp1,presp2,presp3, dfo, dfo_resp )
}



print_modobs <- function(plots_d, plots_o, var ) {
  
  pdf(paste0('FACE_modobs_',var,'.pdf'), width=8.5, height=11 )
  
  print(update(plots_d[[1]],legend=NULL), split=c(1,1,2,3), more=T )  
  print(update(plots_d[[3]],legend=NULL), split=c(1,2,2,3), more=T )    
  print(update(plots_d[[4]],legend=NULL), split=c(1,3,2,3), more=T )    
  
  print(update(plots_o[[1]],legend=NULL), split=c(2,1,2,3), more=T )    
  print(update(plots_o[[3]],legend=NULL), split=c(2,2,2,3), more=T )    
  print(update(plots_o[[4]],legend=NULL), split=c(2,3,2,3), more=F )    
  
  dev.off()
  
}


print_modobs8 <- function(plots, var ) {
  
  pdf(paste0('FACE_modobs8_',var,'.pdf'), width=14, height=7 )
  
  print(update(plots[[1]],legend=NULL), split=c(1,1,4,2), more=T )  
  print(update(plots[[2]],legend=NULL), split=c(2,1,4,2), more=T )    
  print(update(plots[[3]],legend=NULL), split=c(3,1,4,2), more=T )    
  print(update(plots[[4]],legend=NULL), split=c(4,1,4,2), more=T )    
  
  print(update(plots[[5]],legend=NULL), split=c(1,2,4,2), more=T )    
  print(update(plots[[6]],legend=NULL), split=c(2,2,4,2), more=T )    
  print(update(plots[[7]],legend=NULL), split=c(3,2,4,2), more=T )    
  print(update(plots[[8]],legend=NULL), split=c(4,2,4,2), more=F )    
  
  dev.off()
  
}



make_modobs <- function(var, lab ) {
  plots_d <- 
    plot_modobs(l1, df_obs, df_obsse, labs=var_labs[[var]], fsite='DUKE', fco2='AMB', 
                sites=sites, years=years, caseidprefix=caseidprefix )
  plots_o <- 
    plot_modobs(l1, df_obs, df_obsse, labs=var_labs[[var]], fsite='ORNL', fco2='AMB', 
                sites=sites, years=years, caseidprefix=caseidprefix )
  print_modobs(plots_d, plots_o, lab )
  
  list(duke=plots_d, ornl=plots_o )
}


make_modobs_onesite <- function(vars, lab, site, pid=4 ) {

  plots <- 
    lapply(vars, function(var) {
      plots1 <- 
        plot_modobs(l1, df_obs, df_obsse, labs=var_labs[[var]], fsite=site, fco2='AMB', 
                    sites=sites, years=years, caseidprefix=caseidprefix )
      plots1[[pid]]
    })
  
  print_modobs8(plots, lab )
  
  list(plots=plots )
}



### END ###