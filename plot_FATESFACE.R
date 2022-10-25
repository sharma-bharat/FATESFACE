##############################
#
# Open ELM/CLM R arrays and plot 
#
# AWalker
# May 2021
#
##############################

rm(list=ls())

# data processing
library(dplyr)
library(tidyr)

# plotting
library(lattice)
library(grid)
library(viridis)



# Initialise
############################

# paths
wd_obs  <- '/mnt/disk2/Research_Projects/FACE_modelling/Phase_3/data/processed_response'
root    <- '/Volumes/disk2/Research_Projects/FATES/runs/FACE/'
wd_src  <- paste(root,'FATESFACE_analysis', sep='/' )
moddate <- '210802'
moddate <- '211007'
moddate_ens <- '211007'
moddate <- '220207'
wdo     <- paste0(root,moddate,'/plots')
if(!file.exists(wdo)) dir.create(wdo)
wdo_ens <- paste0(root,moddate_ens,'/plots')



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
caseidprefix_gen_ens <- 'FACEbase_nutrients'
caseidprefix_gen     <- 'FACEbase_nutrients_rootdemnd'
caseidprefix_var     <- c('C', 'CNP_ECA', 'CNP_RD' )
caseidprefix <- paste(caseidprefix_gen,caseidprefix_var,sep='_')
sites        <- c('US-DUK', 'US-ORN' )
cases        <- c('aCO2', 'eCO2' )
years        <- list(
  `US-DUK` = 1996:2007,
  `US-ORN` = 1998:2008
) 

ensemble <- 'FACE_ensembleCNP_RD_processed'


# root    <- '/Volumes/disk2/Research_Projects/FATES/runs/tests_main/plantation/FACE_plantation1p0_spread0p2t1_CNP_RD_processed'
# caseidprefix <- c('FACE_C')
# sites        <- c('US-ORN' )
# cases        <- c()
# years        <- list(
#   `US-ORN` = 1998:2008
# ) 


# calculate additional variables from model output 
add_vars <- T



# plotting functions
#############################################

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



plot3d_ensemble <- function(a1, vid, na0=T, pa2sub=NULL, logt=F,
                            var2=l2$dim_variables$fates_levscls$vals,
                            var_deets=l2$variables[[dimnames(a1)$vars[vid]]],
                            xlab='Year',
                            fates_dim='DBH Size Class [cm]',
                            var_label=NULL
) {
  
  # dimnames(a1)$vars[vid]
  
  # determine values of second dimension 
  lv2      <- length(var2)
  row_vals <- apply(rbind(var2,c(var2[2:lv2],lv2)), 2, mean )
  if(is.null(var_label)) var_label <- paste0(var_deets$longname,' [',var_deets$units,']') 
  
  # process data
  plot_array  <- aperm(a1[,,,dimnames(a1)$vars[vid],], c(2,1,3) )
  zmax        <- max(plot_array)
  zmin        <- min(plot_array)
  if(na0) plot_array[plot_array==0] <- NA
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
           xlab=xlab, ylab=var_label,
           type='l', auto.key=list(space='right', points=F, lines=T ))
  
  p2d2 <- 
    xyplot(plot_2d~rep(1:dim(plot_2d)[1],dim(plot_2d)[2])|rep(dimnames(plot_2d)[[2]], each=dim(plot_2d)[1] ), 
           xlab=xlab, ylab=var_label,
           type='l', as.table=T, layout=c(9,2)  )
  
  
  # 2d line
  p2d_array <- 
    xyplot((plot_array2)~(rep(row_vals, prod(dim3d[2:3]) )) | rep(dimnames(plot_array)[[3]], each=prod(dim3d[1:2]) ),
           groups=rep(rep(1:dim3d[2], each=dim3d[1] ), dim3d[3] ),
           col=viridis(dim3d[2]),
           xlab=fates_dim,
           ylab=var_label,
           scales=list(x=list(log=logt), y=list(log=logt), alternating=F ),
           type='l', as.table=T, layout=c(9,2) )
  
  list(p3d_array, p2d_array, p2d1, p2d2 )
}



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


ifile <- 'OBS_ase.rds'
df_obsse <- readRDS(ifile)
str(df_obsse)
df_obsse <- df_obsse %>% 
  filter(site=='DUKE'|site=='ORNL')

# xyplot(NPP~YEAR|site,df_obs,groups=co2)
# xyplot(NPP~YEAR|site,df_obsse,groups=co2)
# xyplot(T~YEAR|site,df_obs)
# xyplot(T~YEAR|site,df_obsse)



# open RDS lists of arrays - these need to have been generated by ELM_postprocessR
#############################################

# create a 3-level list of caseidprefix (level 1), site (level 2), & co2 (level 3) 
l1 <- list()
for(c in caseidprefix) {
  setwd(paste(root,moddate,caseidprefix_gen,sep='/'))
  setwd(paste0(c,'_processed'))
  print('')
  print(getwd())
  
  l2 <- list()
  for(s in sites) {
    l3 <- list()
    for(co2 in cases) {
      
      # read annual data and extract lndgrd,time array 
      fname <- paste(c,s,co2,'annual',sep='_')
      print(fname)
      rl1   <- readRDS(paste0(fname,'.RDS')) 
      a1    <- rl1$data_arrays$`lndgrid,time`
      
      # currently not added to any data structure
      a2    <- rl1$data_arrays$`lndgrid,fates_levscls,time`
      a3    <- rl1$data_arrays$`lndgrid,levgrnd,time`
      
      # add variables
      if(add_vars) a1 <- add_vars_lndtime(a1, add_vars_list )
      
      # combine data
      l3    <- c(l3,list(a1))
      names(l3)[length(l3)] <- co2
    }
    l2    <- c(l2,list(l3))
    names(l2)[length(l2)] <- s
  }
  l1    <- c(l1,list(l2))
  names(l1)[length(l1)] <- c
}

format(object.size(l1),units='Mb')
names(l1)
names(l1[[1]])
names(l1[[1]][[1]])
class(l1[[1]][[1]][[1]])
dimnames(l1[[1]][[1]][[1]])
head(l1[[1]][[1]][[1]])


# names(rl1)
# rl1$dimensions
# rl1$dim_combinations
# names(rl1$variables)
rl1$variables
rl1$variables['TLAI']
names(rl1$data_arrays)
dimnames(rl1$data_arrays$`lndgrid,time`)
# rl1$data_arrays
class(rl1$data_arrays)
lapply(rl1$data_arrays, class )
lapply(rl1$data_arrays, dim )
lapply(rl1$data_arrays, dimnames )


# load ensemble
setwd(paste(root,moddate_ens,caseidprefix_gen_ens,sep='/'))
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



#############################################
# plot -- site runs

lv <- 4
var <- 'GPP'
vcol <- c('blue', 'red' )
vlty <- rep(1:2, each=2 )
leg_cols <- 2

if(is.null(vcol)) vcol <- viridis(lv)
leg_cols <- min(lv,leg_cols)


# # test
lp <- calc_plot_vectors(l1, 'NPP', caseidprefix, sites, years )
plot_co2response_sites(lp, colrs=c('blue','red'), ltypes=c(1,1,2,2), leg_cols=2,
                         ylab=expression("NPP [gC " * m^-2 * " y"^-1 * "]") )
plot_co2response_sites(lp, colrs=rep(c('blue','red'),each=2), ltypes=c(1,2), leg_cols=2,
                       ylab=expression("NPP [gC " * m^-2 * " y"^-1 * "]"))

labvar <- 'nuts'
unitse <- expression("[gC " * m^-2 * " y"^-1 * "]")
plot_co2response_sites(lp, colrs=rep(c(viridis(10)[3],viridis(10)[7]),each=2), ltypes=1, lalpha=c(0.6,1), leg_cols=2,
                       ylab=parse(text=paste0("'NPP ' * ",unitse)) )
plot_co2response_sites(lp, colrs=rep(c(viridis(10)[3],viridis(10)[7]),each=2), ltypes=1, lalpha=c(0.6,1), leg_cols=2,
                       ylab=parse(text=paste0("'",labvar," ' * ",unitse)) )


# lp_resp <- calc_responseplot_vectors(l1, 'NPP', caseidprefix, sites, years, func='percent_array' )
# plot_co2response_sites(lp_resp, inpanel='caseid',
#                        colrs=viridis, 
#                        ltypes=1, 
#                        leg_cols=3,
#                        ylab=expression("NPP [gC " * m^-2 * " y"^-1 * "]"))  
# lp <- calc_plot_vectors(l1, 'NPP', caseidprefix, sites, years )
# lp <- calc_plot_vectors2(l1, 'NPP', caseidprefix, sites, years )


# plot
dimnames(l1[[1]][[1]][[1]])
dimnames(l1[[2]][[1]][[1]])
rl1$variables[['NPP_STEM']]


setwd(wdo)
plots <- make_figures(l1, plotlist_co2, caseidprefix=caseidprefix, sites=sites, time=years, print2screen=T )
plot_figures(plots, paste0(fname,'_CO2'), 'pdf', width=11, height=7, nper_page=2 )
# png does not work with multiple pages
# plot_figures(plots, paste0(fname,'_CO2'), 'png', width=11, height=7, nper_page=2 )
names(plots)
print(plots$p1)
print(plots$p9)

# need to make a list, perhaps use var_labs list
names(rl1)
rl1$variables
names(rl1$variables)
rl1$variables[['C_STOMATA']]
rl1$variables[['TLAI']]
rl1$variables[['TLAI']]
rl1$variables[['ED_bsapwood']]

names(df_obs)
df_obs
df_obs$GFR

# NGL, NGW, NGR for N  in production obs, not mod results on that yet.
# Need a NULL switch for missing data in plotting function
# Need to fix CIs when response switches from +ve to -ve


class(df_obs)
df_co2

plots <- 
  plot_modobs(l1, df_obs, df_obsse, labs=var_labs$lai, fsite='ORNL', fco2='AMB', 
              sites=sites, years=years, caseidprefix=caseidprefix
  )
# plot 1 is amb/ele, 2 is abs response, 3 is % response, 4 is beta response, 5 & 6 are parsed data  
plots[[1]]  
plots[[2]]  
plots[[3]]  
plots[[4]]  
plots[[5]]  
plots[[6]]  


plots_d_npp <- 
  plot_modobs(l1, df_obs, df_obsse, labs=var_labs$npp, fsite='DUKE', fco2='AMB', 
              sites=sites, years=years, caseidprefix=caseidprefix )
plots_o_npp <- 
  plot_modobs(l1, df_obs, df_obsse, labs=var_labs$npp, fsite='ORNL', fco2='AMB', 
              sites=sites, years=years, caseidprefix=caseidprefix )
print_modobs(plots_d_npp, plots_o_npp, 'NPP' )


out <- make_modobs('npp', 'NPP' )
out <- make_modobs('lai', 'LAI' )
out <- make_modobs('wood', 'wood' )
out <- make_modobs('nup', 'Nuptake' )
out <- make_modobs('bp', 'BP' )
out <- make_modobs('bp_store', 'BPiStore' )
out <- make_modobs('leaf_prod', 'leaf_prod' )
out <- make_modobs('wood_prod', 'wood_prod' )
out <- make_modobs('root_prod', 'root_prod' )
out <- make_modobs('leaf_prod_n', 'leaf_prodN' )
out <- make_modobs('wood_prod_n', 'wood_prodN' )
out <- make_modobs('root_prod_n', 'root_prodN' )

out$ornl[[5]]
out$ornl[[6]]

o6hold <- out$ornl[[6]]
o6hold

vars <- c('npp', 'leaf_prod', 'root_prod', 'wood_prod', 'nup', 'leaf_prod_n', 'root_prod_n', 'wood_prod_n' )
out  <- make_modobs_onesite(vars=vars, lab='DUKE_BPcomponents_beta', site='DUKE', pid=4 )
out  <- make_modobs_onesite(vars=vars, lab='ORNL_BPcomponents_beta', site='ORNL', pid=4 )
out  <- make_modobs_onesite(vars=vars, lab='DUKE_BPcomponents_amb', site='DUKE', pid=1 )
out  <- make_modobs_onesite(vars=vars, lab='ORNL_BPcomponents_amb', site='ORNL', pid=1 )



# ensemble plots
##################################

names(l2$data_arrays)
names(l2$data_arrays)[19]
dimcomb <- names(l2$data_arrays)[19]
a1    <- l2$data_arrays[[dimcomb]]
dim(a1)
dimnames(a1)

# creat LAI_SCLS variable
a1[,,,'DDBH_CANOPY_SCLS',] <- 
  a1[,,,'LAI_CANOPY_SCLS',] +
  a1[,,,'LAI_UNDERSTORY_SCLS',]
dimnames(a1)$vars[3] <- 'LAI_SCLS'

tibble(l2$ensemble_par_vals) 
names(l2$ensemble_par_vals)


a1   <- a1
vid  <- 2
na0  <- T 
log  <- F
pa2sub    <- NULL
var2      <- l2$dim_variables$fates_levscls$vals
var_deets <- l2$variables[[dimnames(a1)$vars[vid]]]
fates_dim <- names(dimnames(a1)[2])


vars_lab <- ''
which(dimnames(a1)$vars == paste0('LAI','_SCLS'))

plots <- plot3d_ensemble(a1, 3 )

setwd(wdo)
pdf('Size_class_BA.pdf', 14, 10)
plots[[1]]
dev.off()

pdf('Size_class_BA_2d.pdf', 6, 5 )
update(plots[[3]], legend=NULL )
dev.off()

plots[[1]]
plots[[2]]
plots[[4]]
plots[[3]]

setwd(wdo_ens)
vars_lab <- c('AGB', 'LAI', 'BA', 'NPLANT' )
for(var1 in vars_lab ) {
  
  logt <- F
  if(var1%in%c('NPLANT')) logt <- T
  plots <- plot3d_ensemble(a1, which(dimnames(a1)$vars == paste0(var1,'_SCLS')), logt=logt )
  
  pdf(paste0('Size_class_',var1,'.pdf'), 14, 10)
  print(plots[[1]])
  dev.off()
  
  pdf(paste0('Size_class_',var1,'_2d.pdf'), 6, 5 )
  print(update(plots[[3]], legend=NULL ))
  dev.off()
}


tibble(l2$ensemble_par_vals) 
names(l2$ensemble_par_vals)







# met data
# for PI meeting poster
##################################


setwd(paste0(root,moddate,'/plots'))
# pdf('NPP_reponse_Duke.pdf', width=6.5, height=3.5 )
# plot_response(lp_resp, subset(dfo_npp_co2, site=='DUKE'), site='US-DUK' )
# dev.off()

# pdf('NPP_reponse_ORNL.pdf', width=6.5, height=3.5 )
# plot_response(lp_resp, subset(dfo_npp_co2, site=='ORNL'), site='US-ORN' )
# dev.off()

data_func = 'calc_responseplot_vectors'
resp_func = 'percent_array'
plot_func = 'plot_co2response_sites'
var  = 'GPP'
ylab = expression("GPP response [%]")

lp <- calc_plot_vectors(l1, 'NPP', caseidprefix, sites, years )
plot_co2response_sites(lp, colrs=rep(c('blue','red'),each=2), ltypes=c(1,2), leg_cols=2,
                       ylab=expression("NPP [gC " * m^-2 * " y"^-1 * "]"))  

colrs <- magma(3)
xyplot(l1[] ~ lp$vx | lp[[outpanel]], groups=lp[[inpanel]],
       type='l', scales=list(tck=c(-0.5,-0.5), alternating=F ), as.table=T,
       xlab='Year',
       par.settings=simpleTheme(col=colrs, lwd=2, lty=ltypes ), 
       strip=strip.custom(bg='grey90',par.strip.text=list(cex=0.75)),
       auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1, columns=leg_cols,
                     border=T, cex=0.75, background='white'))


sitecolrs <- c('blue','red')

lp_rain <- calc_plot_vectors2(l1, 'RAIN', caseidprefix[1], sites, years )
pr <- 
  xyplot(lp_rain$vy ~ lp_rain$vx, groups=lp_rain$sites,
         type='l', scales=list(tck=c(-0.5,-0.5), alternating=F, x=list(at=seq(1996,2008,4)) ),
         xlab='Year', ylab=expression('Precipitation [mm '*y^-1*']'),
         par.settings=simpleTheme(col=sitecolrs, lwd=2, lty=1 )#, 
         # auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1, columns=1,
         #               border=T, cex=0.75, background='white')
  )
pr

lp_temp <- calc_plot_vectors2(l1, 'TBOT', caseidprefix[1], sites, years )
pt <- 
  xyplot(lp_temp$vy ~ lp_temp$vx, groups=lp_temp$sites,
         type='l', scales=list(tck=c(-0.5,-0.5), alternating=F, x=list(at=seq(1996,2008,4)) ),
         xlab='Year', ylab=expression('Air Temperature ['*degree*'C]'),
         par.settings=simpleTheme(col=sitecolrs, lwd=2, lty=1 ),
         auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1, columns=1,
                       border=T, cex=0.75, background='white')
  )
pt 

lp_swrad <- calc_plot_vectors2(l1, 'FSDS', caseidprefix[1], sites, years )
psw <- 
  xyplot(lp_swrad$vy ~ lp_swrad$vx, groups=lp_swrad$sites,
         type='l', scales=list(tck=c(-0.5,-0.5), alternating=F, x=list(at=seq(1996,2008,4)) ),
         xlab='Year', ylab=expression('SW Radiation ['*W*m^-2*']'),
         par.settings=simpleTheme(col=sitecolrs, lwd=2, lty=1 )#, 
         # auto.key=list(lines=T, points=F, corner=c(0,0), x=0, y=1, columns=1,
         #               border=T, cex=0.75, background='white')
  )
psw

setwd(paste0(root,moddate,'/plots'))
pdf('met_data.pdf', width=10, height=3.5 )
print(pt, split=c(1,1,3,1), more=T )
print(pr, split=c(2,1,3,1), more=T )
print(psw, split=c(3,1,3,1), more=F )
dev.off()



### END ###