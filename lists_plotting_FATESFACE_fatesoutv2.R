##############################
#
# plot info
#
# AWalker
# May 2021
#
##############################



# general model co2 responses
plotlist_co2 <- list(
  p1 = list(
    data_func = 'calc_plot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'FATES_GPP',
    ylab = expression("FATES_GPP [gC " * m^-2 * " y"^-1 * "]"),
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  p2 = list(
    data_func = 'calc_plot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'FATES_NPP',
    ylab = expression("FATES_NPP [gC " * m^-2 * " y"^-1 * "]"),
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  p3 = list(
    data_func = 'calc_responseplot_vectors',
    resp_func = 'percent_array',
    plot_func = 'plot_co2response_sites',
    var  = 'FATES_GPP',
    colrs=colrs[c(1,3)],
    ylab = expression("FATES_GPP response [%]")
  ),
  p4 = list(
    data_func = 'calc_responseplot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'FATES_GPP',
    colrs=colrs[c(1,3)],
    ylab = expression("FATES_GPP response "*beta*" [-]")
  ),
  p5 = list(
    data_func = 'calc_responseplot_vectors',
    resp_func = 'percent_array',
    plot_func = 'plot_co2response_sites',
    var  = 'FATES_GPP',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("FATES_GPP response [%]")
  ),
  p6 = list(
    data_func = 'calc_responseplot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'FATES_GPP',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("FATES_GPP response "*beta*" [-]")
  ),
  p5a = list(
    data_func = 'calc_responseplot_vectors',
    resp_func = 'percent_array',
    plot_func = 'plot_co2response_sites',
    var  = 'FATES_NPP',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("FATES_NPP response [%]")
  ),
  p6a = list(
    data_func = 'calc_responseplot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'FATES_NPP',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("FATES_NPP response "*beta*" [-]")
  ),
  p7 = list(
    data_func = 'calc_plot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'TLAI',
    ylab = expression("LAI ["*m^2*" m"^-2*"]"),
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  p8 = list(
    data_func = 'calc_responseplot_vectors',
    resp_func = 'percent_array',
    plot_func = 'plot_co2response_sites',
    var  = 'TLAI',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("LAI response [%]")
  ),
  
  p9 = list(
    data_func = 'calc_plot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'iWUE',
    ylab = expression('iWUE FATES_GPP/'*g[c]*' ['*mu*mol*' C '*mol^-1*' '*H[2]*O*']'),
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  p10a = list(
    data_func = 'calc_responseplot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'iWUE',
    resp_func = 'abs_array',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression('iWUE FATES_GPP/'*g[c]*' response ['*mu*mol*' C '*mol^-1*' '*H[2]*O*']')
  ),
  p10b = list(
    data_func = 'calc_responseplot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'iWUE',
    resp_func = 'percent_array',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("iWUE response [%]")
  ),
  p10c = list(
    data_func = 'calc_responseplot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'iWUE',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("iWUE response "*beta*" [-]")
  ),

  p9.1 = list(
    data_func = 'calc_plot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'GC',
    ylab = expression(g[c]*' ['*mu*mol*' '*H[2]*O * m^-2 * " s"^-1 * ']'),
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  p10c.1 = list(
    data_func = 'calc_responseplot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'GC',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression(g[c]*" response "*beta*" [-]")
  ),
    
  p11 = list(
    data_func = 'calc_plot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'WUEveg',
    ylab  = expression('WUEveg [g.'*kg^-1*']'),
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  p12 = list(
    data_func = 'calc_responseplot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'WUEveg',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("WUEveg response "*beta*" [-]")
  ),
  p13 = list(
    data_func = 'calc_plot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'WUEecosystem',
    ylab  = expression('WUEecosystem [g.'*kg^-1*']'),
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  p14 = list(
    data_func = 'calc_responseplot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'WUEecosystem',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("WUEecosystem response "*beta*" [-]")
  ),
  p15 = list(
    data_func = 'calc_plot_vectors',
    plot_func = 'plot_co2response_sites',
    var   = 'BTRAN',
    ylab  = 'Plant water status [0-1]',
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  p16 = list(
    data_func = 'calc_responseplot_vectors',
    plot_func = 'plot_co2response_sites',
    var   = 'BTRAN',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("Plant water status response "*beta*" [-]")
  ),
  pbioma = list(
    data_func = 'calc_plot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'FATES_VEGC',
    ylab = expression("Biomass [gC " * m^-2 * "]"),
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  pbiomb = list(
    data_func = 'calc_responseplot_vectors',
    resp_func = 'percent_array',
    plot_func = 'plot_co2response_sites',
    var  = 'FATES_VEGC',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("Biomass response [%]")
  ),
  psapwa = list(
    data_func = 'calc_plot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'FATES_SAPWOODC',
    ylab = expression("Sapwood Biomass [gC " * m^-2 * "]"),
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  psapwb = list(
    data_func = 'calc_responseplot_vectors',
    resp_func = 'percent_array',
    plot_func = 'plot_co2response_sites',
    var  = 'FATES_SAPWOODC',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("Sapwood Biomass response [%]")
  ),
  pstorea = list(
    data_func = 'calc_plot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'FATES_STOREC',
    ylab = expression("Plant C Store [gC " * m^-2 * "]"),
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  pstoreb = list(
    data_func = 'calc_responseplot_vectors',
    resp_func = 'percent_array',
    plot_func = 'plot_co2response_sites',
    var  = 'FATES_STOREC',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("Plant C Store response [%]")
  ),
  ppnsa = list(
    data_func = 'calc_plot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'FATES_STOREN',
    ylab = expression("Plant N Store [gN " * m^-2 * "]"),
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  ppnsb = list(
    data_func = 'calc_responseplot_vectors',
    resp_func = 'percent_array',
    plot_func = 'plot_co2response_sites',
    var  = 'FATES_STOREN',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("Plant N Store response [%]")
  ),
  pnnmina = list(
    data_func = 'calc_plot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'NET_NMIN',
    ylab = expression("Net N Mineralisation [gN " * m^-2 * " y"^-1 * "]"),
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  pnnminb = list(
    data_func = 'calc_responseplot_vectors',
    resp_func = 'percent_array',
    plot_func = 'plot_co2response_sites',
    var  = 'NET_NMIN',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("Net N Mineralisation response [%]")
  ),
  ppnda = list(
    data_func = 'calc_plot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'FATES_NDEMAND',
    ylab = expression("Plant N Demand [gN " * m^-2 * " y"^-1 * "]"),
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  ppndb = list(
    data_func = 'calc_responseplot_vectors',
    resp_func = 'percent_array',
    plot_func = 'plot_co2response_sites',
    var  = 'FATES_NDEMAND',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("Plant N Demand response [%]")
  ),
  ppnua = list(
    data_func = 'calc_plot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'NUP',
    ylab = expression("Plant N Uptake [gN " * m^-2 * " y"^-1 * "]"),
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  ppnub = list(
    data_func = 'calc_responseplot_vectors',
    resp_func = 'percent_array',
    plot_func = 'plot_co2response_sites',
    var  = 'NUP',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("Plant N Uptake response [%]")
  )
)



# variable labels for modobs plots
var_labs <- list(
  
  # FATES_NPP
  npp = list(
    fvar_mod = 'FATES_NPP',
    fvar_obs = 'NPP',
    amb = list(
      ylab = expression('NPP [gC '*m^-2*y^-1*']'),
      ylim = c(500,1500)
    ),
    resp = list(
      ylab = expression('NPP response [gC '*m^-2*y^-1*']'),
      ylim = c(0,500)
    ),
    perc = list(
      ylab = 'NPP response [%]',
      ylim = c(0,50)
    ),
    beta = list(
      ylab = expression('NPP '*beta*' [-]'),
      ylim = c(0,1.3)
    )
  ),
  
  # BP
  # - need to calculate these
  bp = list(
    fvar_mod = 'Biomass_production',
    fvar_obs = 'NPP',
    amb = list(
      ylab = expression('Biomass Production [gC '*m^-2*y^-1*']'),
      ylim = c(500,1500)
    ),
    resp = list(
      ylab = expression('Biomass Production response [gC '*m^-2*y^-1*']'),
      ylim = c(0,500)
    ),
    perc = list(
      ylab = 'Biomass Production response [%]',
      ylim = c(0,50)
    ),
    beta = list(
      ylab = expression('Biomass Production '*beta*' [-]'),
      ylim = c(0,1.3)
    )
  ),
  
  # BP inc Store C
  # - need to calculate these
  bp_store = list(
    fvar_mod = 'Biomass_production_incStore',
    fvar_obs = 'NPP',
    amb = list(
      ylab = expression('BP inc. store [gC '*m^-2*y^-1*']'),
      ylim = c(500,1500)
    ),
    resp = list(
      ylab = expression('BP inc. store response [gC '*m^-2*y^-1*']'),
      ylim = c(0,500)
    ),
    perc = list(
      ylab = 'BP inc. store response [%]',
      ylim = c(0,50)
    ),
    beta = list(
      ylab = expression('BP inc. store '*beta*' [-]'),
      ylim = c(0,1.3)
    )
  ),
  
  # N uptake
  nup = list(
    fvar_mod = 'NUP',# 'SMINN_TO_PLANT',
    fvar_obs = 'NUP',
    amb = list(
      ylab = expression('Plant N uptake [gN '*m^-2*y^-1*']'),
      ylim = c(0,15)
    ),
    resp = list(
      ylab = expression('Plant N uptake response [gN '*m^-2*y^-1*']'),
      ylim = c(-5,5)
    ),
    perc = list(
      ylab = 'Plant N uptake response [%]',
      ylim = c(-10,50)
    ),
    beta = list(
      ylab = expression('Plant N uptake '*beta*' [-]'),
      ylim = c(-1,1.5)
    )
  ),
  
  # LAI
  lai = list(
    fvar_mod = 'TLAI',
    fvar_obs = 'LAIpeak',
    amb = list(
      ylab = expression('LAI ['*m^-2*m^-2*']'),
      ylim = c(0,8)
    ),
    resp = list(
      ylab = expression('LAI response ['*m^-2*m^-2*']'),
      ylim = c(-1,2)
    ),
    perc = list(
      ylab = 'LAI response [%]',
      ylim = c(-10,25)
    ),
    beta = list(
      ylab = expression('LAI '*beta*' [-]'),
      ylim = c(0,1)
    )
  ),
  
  # Wood biomass
  wood = list(
    fvar_mod = 'FATES_SAPWOODC',
    fvar_obs = 'CW',
    amb = list(
      ylab = expression('Wood [gC '*m^-2*']'),
      ylim = c(0,15000)
    ),
    resp = list(
      ylab = expression('Wood response [gC '*m^-2*']'),
      ylim = c(0,4000)
    ),
    perc = list(
      ylab = 'Wood response [%]',
      ylim = c(-1,40)
    ),
    beta = list(
      ylab = expression('Wood '*beta*' [-]'),
      ylim = c(-0.2,1.2)
    )
  ),
    
  # Wood biomass production
  wood_prod = list(
    fvar_mod = 'FATES_STEM_ALLOC',
    fvar_obs = 'GW',
    amb = list(
      ylab = expression('Wood production [gC '*m^-2*y^-1*']'),
      ylim = c(0,1000)
    ),
    resp = list(
      ylab = expression('Wood production response [gC '*m^-2*y^-1*']'),
      ylim = c(-100,1000)
    ),
    perc = list(
      ylab = 'Wood production response [%]',
      ylim = c(-20,80)
    ),
    beta = list(
      ylab = expression('Wood production '*beta*' [-]'),
      ylim = c(-0.5,2)
    )
  ),
  
  # Leaf biomass production
  leaf_prod = list(
    fvar_mod = 'FATES_LEAF_ALLOC',
    fvar_obs = 'GL',
    amb = list(
      ylab = expression('Leaf production [gC '*m^-2*y^-1*']'),
      ylim = c(0,400)
    ),
    resp = list(
      ylab = expression('Leaf production response [gC '*m^-2*y^-1*']'),
      ylim = c(0,100)
    ),
    perc = list(
      ylab = 'Leaf production response [%]',
      ylim = c(-1,30)
    ),
    beta = list(
      ylab = expression('Leaf production '*beta*' [-]'),
      ylim = c(0,1.25)
    )
  ),
  
  # Fine-root biomass production
  root_prod = list(
    fvar_mod = 'FATES_FROOT_ALLOC',
    fvar_obs = 'GR',
    amb = list(
      ylab = expression('Fine-root production [gC '*m^-2*y^-1*']'),
      ylim = c(0,400)
    ),
    resp = list(
      ylab = expression('Fine-root production response [gC '*m^-2*y^-1*']'),
      ylim = c(0,100)
    ),
    perc = list(
      ylab = 'Fine-root production response [%]',
      ylim = c(-125,300)
    ),
    beta = list(
      ylab = expression('Fine-root production '*beta*' [-]'),
      ylim = c(-4,8)
    )
  ),

  # Fine-root biomass production fraction
  root_alloc = list(
    fvar_mod = 'FROOT_alloc_frac',
    fvar_obs = 'FROOT_alloc_frac',
    amb = list(
      ylab = expression('Fine-root allocation frac. [-]'),
      ylim = c(0,1)
    ),
    resp = list(
      ylab = expression('Fine-root allocation frac. response [-]'),
      ylim = c(-0.15,0.2)
    ),
    perc = list(
      ylab = expression('Fine-root allocation frac. response [%]'),
      ylim = c(-60,130)
    ),
    beta = list(
      ylab = expression('Fine-root allocation frac. '*beta*' [-]'),
      ylim = c(-2,4)
    )
  ),

  # Leaf : Fine-root biomass ratio 
  leaf_root_ratio = list(
    fvar_mod = 'LEAF_FROOT_ratio',
    fvar_obs = 'LEAF_FROOT_ratio',
    amb = list(
      ylab = expression('Leaf:Fine-root [-]'),
      ylim = c(0.25,7)
    ),
    resp = list(
      ylab = expression('Leaf:Fine-root response [-]'),
      ylim = c(-5,3)
    ),
    perc = list(
      ylab = expression('Leaf:Fine-root response [%]'),
      ylim = c(-120,60)
    ),
    beta = list(
      ylab = expression('Leaf:Fine-root '*beta*' [-]'),
      ylim = c(-4,3)
    )
  )#,

  
  # - no organ N or P flux variables in oputput as far as I can tell 
#  # Wood biomass production N
#  wood_prod_n = list(
#    fvar_mod = 'FATES_STEMN_ALLOC',
#    fvar_obs = 'NGW',
#    amb = list(
#      ylab = expression('Wood production N [gN '*m^-2*y^-1*']'),
#      ylim = c(0,4)
#    ),
#    resp = list(
#      ylab = expression('Wood production N response [gN '*m^-2*y^-1*']'),
#      ylim = c(0,2)
#    ),
#    perc = list(
#      ylab = 'Wood production N response [%]',
#      ylim = c(-10,75)
#    ),
#    beta = list(
#      ylab = expression('Wood production N '*beta*' [-]'),
#      ylim = c(-0.25,1.75)
#    )
#  ),
#  
#  # Leaf biomass production N
#  leaf_prod_n = list(
#    fvar_mod = 'FATES_LEAFN_ALLOC',
#    fvar_obs = 'NGL',
#    amb = list(
#      ylab = expression('Leaf production N [gN '*m^-2*y^-1*']'),
#      ylim = c(0,12)
#    ),
#    resp = list(
#      ylab = expression('Leaf production N response [gN '*m^-2*y^-1*']'),
#      ylim = c(-2,2)
#    ),
#    perc = list(
#      ylab = 'Leaf production N response [%]',
#      ylim = c(-30,10)
#    ),
#    beta = list(
#      ylab = expression('Leaf production N '*beta*' [-]'),
#      ylim = c(-0.8,0.8)
#    )
#  ),
#  
#  # Fine-root biomass production N
#  root_prod_n = list(
#    fvar_mod = 'FATES_FROOTN_ALLOC',
#    fvar_obs = 'NGR',
#    amb = list(
#      ylab = expression('Fine-root production N [gN '*m^-2*y^-1*']'),
#      ylim = c(0,4)
#    ),
#    resp = list(
#      ylab = expression('Fine-root production N response [gN '*m^-2*y^-1*']'),
#      ylim = c(0,2)
#    ),
#    perc = list(
#      ylab = 'Fine-root production N response [%]',
#      ylim = c(-125,300)
#    ),
#    beta = list(
#      ylab = expression('Fine-root production N '*beta*' [-]'),
#      ylim = c(-2,4)
#    )
#  )

)


# general model diagnostics plot
plotlist <- list(
  vvars = c('FATES_GPP','FATES_NPP','FATES_AUTORESP'),
  ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']'),
  vvars = c('FATES_CROOT_ALLOC','FATES_FROOT_ALLOC','FATES_LEAF_ALLOC','FATES_SEED_ALLOC','FATES_STEM_ALLOC','FATES_STORE_ALLOC'),
  ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']'),
  vvars = c('FATES_NONSTRUCTC','FATES_STRUCTC','FATES_FROOTC','FATES_VEGC','FATES_LEAFC','FATES_SAPWOODC','FATES_STOREC'),
  ylab  = expression('C Pool [gC '*m^-2*']'),
  vvars = c('RAIN','QRUNOFF','QDRAI','QVEGT','QVEGE'),
  ylab  = expression('H2O Flux [kg '*H[2]*O*' '*m^-2*' timestep'^-1*']'),
  vvars = c('SOILWATER_10CM'),
  ylab  = 'Soil Water Content [%]',
  vvars = c('BTRAN'),
  ylab  = 'Plant water status [0-1]',
  vvars = c('TLAI','FATES_TRIMMING'),
  ylab  = expression('LAI ['*m^2*m^-2*'], LAI trimming [0-1]'),
  vvars = c('FATES_PROMOTION_CARBONFLUX','FATES_DEMOTION_CARBONFLUX','FATES_MORTALITY_CFLUX_CANOPY','FATES_MORTALITY_CFLUX_USTORY'),
  ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']'),
  vvars = c('FATES_FATES_DISTURBANCE_RATE_FIRE','FATES_DISTURBANCE_RATE_LOGGING','FATES_DISTURBANCE_RATE_P2P','FATES_DISTURBANCE_RATE_P2S',
            'FATES_FATES_DISTURBANCE_RATE_POTENTIAL','FATES_DISTURBANCE_RATE_S2S','FATES_DISTURBANCE_RATE_TREEFALL'),
  ylab  = 'Fraction of ground area disturbed [0-1]',  
  vvars = c('FATES_NCOHORTS','FATES_NPATCHES'),
  ylab  = 'FATES diagnostics [#]'
)



### END ###
