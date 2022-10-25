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
    var  = 'GPP',
    ylab = expression("GPP [gC " * m^-2 * " y"^-1 * "]"),
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  p2 = list(
    data_func = 'calc_plot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'NPP',
    ylab = expression("NPP [gC " * m^-2 * " y"^-1 * "]"),
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  p3 = list(
    data_func = 'calc_responseplot_vectors',
    resp_func = 'percent_array',
    plot_func = 'plot_co2response_sites',
    var  = 'GPP',
    colrs=colrs[c(1,3)],
    ylab = expression("GPP response [%]")
  ),
  p4 = list(
    data_func = 'calc_responseplot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'GPP',
    colrs=colrs[c(1,3)],
    ylab = expression("GPP response "*beta*" [-]")
  ),
  p5 = list(
    data_func = 'calc_responseplot_vectors',
    resp_func = 'percent_array',
    plot_func = 'plot_co2response_sites',
    var  = 'GPP',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("GPP response [%]")
  ),
  p6 = list(
    data_func = 'calc_responseplot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'GPP',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("GPP response "*beta*" [-]")
  ),
  p5a = list(
    data_func = 'calc_responseplot_vectors',
    resp_func = 'percent_array',
    plot_func = 'plot_co2response_sites',
    var  = 'NPP',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("NPP response [%]")
  ),
  p6a = list(
    data_func = 'calc_responseplot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'NPP',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("NPP response "*beta*" [-]")
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
    ylab = expression('iWUE GPP/'*g[c]*' ['*mu*mol*' C '*mol^-1*' '*H[2]*O*']'),
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
    ylab = expression('iWUE GPP/'*g[c]*' response ['*mu*mol*' C '*mol^-1*' '*H[2]*O*']')
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
    var  = 'ED_biomass',
    ylab = expression("Biomass [gC " * m^-2 * "]"),
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  pbiomb = list(
    data_func = 'calc_responseplot_vectors',
    resp_func = 'percent_array',
    plot_func = 'plot_co2response_sites',
    var  = 'ED_biomass',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("Biomass response [%]")
  ),
  psapwa = list(
    data_func = 'calc_plot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'ED_bsapwood',
    ylab = expression("Sapwood Biomass [gC " * m^-2 * "]"),
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  psapwb = list(
    data_func = 'calc_responseplot_vectors',
    resp_func = 'percent_array',
    plot_func = 'plot_co2response_sites',
    var  = 'ED_bsapwood',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("Sapwood Biomass response [%]")
  ),
  pstorea = list(
    data_func = 'calc_plot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'ED_bstore',
    ylab = expression("Plant C Store [gC " * m^-2 * "]"),
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  pstoreb = list(
    data_func = 'calc_responseplot_vectors',
    resp_func = 'percent_array',
    plot_func = 'plot_co2response_sites',
    var  = 'ED_bstore',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("Plant C Store response [%]")
  ),
  ppnsa = list(
    data_func = 'calc_plot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'STOREN',
    ylab = expression("Plant N Store [gN " * m^-2 * "]"),
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  ppnsb = list(
    data_func = 'calc_responseplot_vectors',
    resp_func = 'percent_array',
    plot_func = 'plot_co2response_sites',
    var  = 'STOREN',
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
    var  = 'PLANT_NDEMAND_COL',
    ylab = expression("Plant N Demand [gN " * m^-2 * " y"^-1 * "]"),
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  ppndb = list(
    data_func = 'calc_responseplot_vectors',
    resp_func = 'percent_array',
    plot_func = 'plot_co2response_sites',
    var  = 'PLANT_NDEMAND_COL',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("Plant N Demand response [%]")
  ),
  ppnua = list(
    data_func = 'calc_plot_vectors',
    plot_func = 'plot_co2response_sites',
    var  = 'SMINN_TO_PLANT',
    ylab = expression("Plant N Uptake [gN " * m^-2 * " y"^-1 * "]"),
    colrs=colrs,
    lalpha=lalpha,
    leg_cols=leg_cols
  ),
  ppnub = list(
    data_func = 'calc_responseplot_vectors',
    resp_func = 'percent_array',
    plot_func = 'plot_co2response_sites',
    var  = 'SMINN_TO_PLANT',
    inpanel='caseid',
    colrs=case_colrs,
    ylab = expression("Plant N Uptake response [%]")
  )
)



# variable labels for modobs plots
var_labs <- list(
  
  # NPP
  npp = list(
    fvar_mod = 'NPP',
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
    fvar_mod = 'SMINN_TO_PLANT',
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
    fvar_mod = 'ED_bsapwood',
    fvar_obs = 'CW',
    amb = list(
      ylab = expression('Wood [gC '*m^-2*']'),
      ylim = c(0,15000)
    ),
    resp = list(
      ylab = expression('Wood response [gC '*m^-2*']'),
      ylim = c(0,1000)
    ),
    perc = list(
      ylab = 'Wood response [%]',
      ylim = c(-1,25)
    ),
    beta = list(
      ylab = expression('Wood '*beta*' [-]'),
      ylim = c(0,1)
    )
  ),
    
  # Wood biomass production
  wood_prod = list(
    fvar_mod = 'NPP_STEM',
    fvar_obs = 'GW',
    amb = list(
      ylab = expression('Wood production [gC '*m^-2*y^-1*']'),
      ylim = c(0,1000)
    ),
    resp = list(
      ylab = expression('Wood production response [gC '*m^-2*y^-1*']'),
      ylim = c(0,1000)
    ),
    perc = list(
      ylab = 'Wood production response [%]',
      ylim = c(-10,75)
    ),
    beta = list(
      ylab = expression('Wood production '*beta*' [-]'),
      ylim = c(-0.25,1.75)
    )
  ),
  
  # Leaf biomass production
  leaf_prod = list(
    fvar_mod = 'NPP_LEAF',
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
    fvar_mod = 'NPP_FROOT',
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

  
  # Wood biomass production N
  wood_prod_n = list(
    fvar_mod = 'NPP_STEMN',
    fvar_obs = 'NGW',
    amb = list(
      ylab = expression('Wood production N [gN '*m^-2*y^-1*']'),
      ylim = c(0,4)
    ),
    resp = list(
      ylab = expression('Wood production N response [gN '*m^-2*y^-1*']'),
      ylim = c(0,2)
    ),
    perc = list(
      ylab = 'Wood production N response [%]',
      ylim = c(-10,75)
    ),
    beta = list(
      ylab = expression('Wood production N '*beta*' [-]'),
      ylim = c(-0.25,1.75)
    )
  ),
  
  # Leaf biomass production N
  leaf_prod_n = list(
    fvar_mod = 'NPPN_LEAF',
    fvar_obs = 'NGL',
    amb = list(
      ylab = expression('Leaf production N [gN '*m^-2*y^-1*']'),
      ylim = c(0,12)
    ),
    resp = list(
      ylab = expression('Leaf production N response [gN '*m^-2*y^-1*']'),
      ylim = c(-2,2)
    ),
    perc = list(
      ylab = 'Leaf production N response [%]',
      ylim = c(-30,10)
    ),
    beta = list(
      ylab = expression('Leaf production N '*beta*' [-]'),
      ylim = c(-0.8,0.8)
    )
  ),
  
  # Fine-root biomass production N
  root_prod_n = list(
    fvar_mod = 'NPPN_FROOT',
    fvar_obs = 'NGR',
    amb = list(
      ylab = expression('Fine-root production N [gN '*m^-2*y^-1*']'),
      ylim = c(0,4)
    ),
    resp = list(
      ylab = expression('Fine-root production N response [gN '*m^-2*y^-1*']'),
      ylim = c(0,2)
    ),
    perc = list(
      ylab = 'Fine-root production N response [%]',
      ylim = c(-125,300)
    ),
    beta = list(
      ylab = expression('Fine-root production N '*beta*' [-]'),
      ylim = c(-2,4)
    )
  )

)


# general model diagnostics plot
plotlist <- list(
  vvars = c('GPP','NPP','AR'),
  ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']'),
  vvars = c('NPP_CROOT','NPP_FROOT','NPP_LEAF','NPP_SEED','NPP_STEM','NPP_STOR'),
  ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']'),
  vvars = c('ED_balive','ED_bdead','ED_bfineroot','ED_biomass','ED_bleaf','ED_bsapwood','ED_bstore'),
  ylab  = expression('C Pool [gC '*m^-2*']'),
  vvars = c('RAIN','QRUNOFF','QDRAI','QVEGT','QVEGE'),
  ylab  = expression('H2O Flux [kg '*H[2]*O*' '*m^-2*' timestep'^-1*']'),
  vvars = c('SOILWATER_10CM'),
  ylab  = 'Soil Water Content [%]',
  vvars = c('BTRAN'),
  ylab  = 'Plant water status [0-1]',
  vvars = c('TLAI','TRIMMING'),
  ylab  = expression('LAI ['*m^2*m^-2*'], LAI trimming [0-1]'),
  vvars = c('DEMOTION_CARBONFLUX','MORTALITY_CARBONFLUX_CANOPY','MORTALITY_CARBONFLUX_UNDERSTORY'),
  ylab  = expression('C Flux [gC '*m^-2*' timestep'^-1*']'),
  vvars = c('DISTURBANCE_RATE_FIRE','DISTURBANCE_RATE_LOGGING','DISTURBANCE_RATE_P2P','DISTURBANCE_RATE_P2S',
            'DISTURBANCE_RATE_POTENTIAL','DISTURBANCE_RATE_S2S','DISTURBANCE_RATE_TREEFALL'),
  ylab  = 'Fraction of ground area disturbed [0-1]',  
  vvars = c('ED_NCOHORTS','ED_NPATCHES'),
  ylab  = 'FATES diagnostics [#]'
)



### END ###