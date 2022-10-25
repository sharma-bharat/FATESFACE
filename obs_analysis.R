###################################
#
# ORNL FACE - density and mortality plots
#
#
#
#
###################################

library(lattice)
library(dplyr)
library(viridis)
library(lubridate)
library(lme4)
library(AICcmodavg)

src <- '/mnt/disk2/script_library/R/function_mods'
setwd(src)
source('lattice_densitypoly.R')

increment      <- function(v) v[length(v)] - v[1]
increment_mean <- function(v) mean(v[(length(v)-1):length(v)]) - mean(v[1:2])
final          <- function(v) v[length(v)]
first          <- function(v) v[1]

wd  <- '/mnt/disk2/Proposals/FACE-MDS/DOE4/data'
wdd <- '/mnt/disk2/Research_Projects/FACE_modelling/Phase_3/data/processed_response'



# read data from proposal directory
setwd(wd)
ifname <- 'BA_and_mort_table.csv'
df1 <- read.csv(ifname)

df1a <- df1[-(6:7),]
df1a$co2 <- c('e','e','a','a','a')
df1a$mort_BA_frac_init   <- df1a$Mort_BA_all / df1a$Start_BA
df1a$mort_BA_frac        <- df1a$Mort_BA_all / df1a$End_BA
df1a$mort_BA_frac_center <- df1a$Mort_BA_all / (0.5*(df1a$End_BA + df1a$Start_BA))
df1a$mort_ind_frac <- df1a$Mort_dens / df1a$Start_dens

ifname <- 'wood_ts.csv'
df2 <- read.csv(ifname)
names(df2) 
df2_stack <- data.frame(RING=df2$RING, TREE=df2$TREE, stack(df2,3:94))
df2_stack$ptime <- strptime(df2_stack$ind, "X%Y.%m.%d")
names(df2_stack)[3] <- 'BA'
head(df2_stack)

# crown class data
# 1=Dom; 2=Co-Dom; 3= Intrerned; 4=Suppressed
ifname <- 'CrownClass.csv'
df3 <- read.csv(ifname)
names(df3)


# read data from FACE data directory
setwd(wdd)

ifname <- 'ORNL_a.csv'
dfd1 <- read.csv(ifname)
head(dfd1)
names(dfd1)

df1d_i <- dfd1 %>%
  group_by(plot) %>%
  summarise_at(c('CW','CCR','CL','CFR','NWOOD','NCR','NCANpeak','NFR','NUP'), increment )
df1d_i

df1d_im <- dfd1 %>%
  group_by(plot) %>%
  summarise_at(c('CW','CCR','CL','CFR','NWOOD','NCR','NCANpeak','NFR','NUP'), increment_mean )
df1d_im

df1d_first <- dfd1 %>%
  group_by(plot) %>%
  summarise_at(c('CW','CCR','CL','CFR','NWOOD','NCR','NCANpeak','NFR'), first )
df1d_first

df1d_final <- dfd1 %>%
  group_by(plot) %>%
  summarise_at(c('CW','CCR','CL','CFR','NWOOD','NCR','NCANpeak','NFR'), final )
df1d_final

df1d_c <- dfd1 %>%
  group_by(plot) %>%
  summarise_at(c('GW','GL','GR','GCR','NPP','NGW','NGL','NGR','NUP'), sum )
df1d_c
df1d_c$NPP


# process ORNL BA data - horribly disorganised!
# convert ptime
df2_stack$ptime <- as.POSIXct(df2_stack$ptime)

df2_stack$co2 <- ifelse(df2_stack$RING==1|df2_stack$RING==2,'ele','amb') 

year(df2_stack$ptime)
# combine ring and tree number
df2_stack <- df2_stack %>%
  mutate(treeid=paste(RING,TREE,sep='.'))

df2_stack_year <- df2_stack %>%
  mutate(year=year(ptime)) %>%
  group_by(RING,year,treeid) %>%
  summarize(BA=max(BA)) %>%
  mutate(co2=ifelse(RING==1|RING==2,'ele','amb'))

df2_stack_year_diff <- df2_stack_year %>%
  group_by(co2,RING,treeid) %>%
  summarise(cBAI=BA[length(BA)]-BA[1])

df2_stack_year_1997_diff <- df2_stack_year %>%
  filter(year==1997) %>%
  left_join(df2_stack_year_diff, by=c('RING','treeid')) # %>%
  # summarise(cBAI=BA[length(BA)]-BA[1])

df2_stack_year_1997_diff_nomort <- df2_stack_year_1997_diff %>%
  filter(cBAI>0)
  
print(df2_stack_year, n=100)  



# rough working for Rich's paper
names(df2_stack)
summary(df2_stack)
class(df2_stack$ptime)

face_ring_d <- 22

sz_bin_width <- 2
df2_stack_sizeclass <- df2_stack %>%
  mutate(dbh=2*(BA/pi)^0.5, size_class=sz_bin_width*(ceiling(dbh/sz_bin_width)-0.5) ) %>%
  group_by(ptime,RING,size_class) %>%
  summarise(BAsum=sum(BA) / (pi*(face_ring_d/2)^2), dbhmean=mean(dbh), n=n() )

xyplot(n~size_class|RING, df2_stack_sizeclass, groups=ptime,# subset=BAsum!=0,
       col=viridis(length(unique(df2_stack_sizeclass$ptime))), 
       type='l',pch=19, cex=1)#, xlim=c(0,500), breaks=seq(0,540,10) )
       
xyplot(BA~RING, df2_stack_sum, subset=ptime==ptime[1] )
xyplot(BA~RING, df2_stack_sum, subset=ptime==max(ptime) )
xyplot(BAsum~size_class|RING, df2_stack_sizeclass, groups=ptime,# subset=BAsum!=0,
       col=viridis(length(unique(df2_stack_sizeclass$ptime))), 
       type='l',pch=19, cex=1)#, xlim=c(0,500), breaks=seq(0,540,10) )

levelplot(BAsum~ptime*size_class|RING, df2_stack_sizeclass,
          cuts=50, col.regions=rev(magma(51)),
          ylim=c(0,25), fill=magma(51)[1] )
          
levelplot(BAsum~ptime*size_class, df2_stack_sizeclass, subset=RING==1,
          cuts=50, col.regions=rev(magma(51))#,
          # main=paste0(var_deets$longname,' [',var_deets$units,']'),
          # ylab=names(dimnames(scls)[2]), ylim=c(0,110), column.values=row_vals, aspect=1,
          # xlab=names(dimnames(scls)[3]),
          # scales=list(y=list(at=c(fates_levscls,100)), tck=c(0.5,0) ),
          # colorkey=list(at=seq(0,35,35/50) )
)

  
  
df2_stack_sum <- df2_stack %>%
  group_by(ptime,RING) %>%
  summarise(BA=sum(BA))

df2_stack_rank <- df2_stack %>%
  group_by(RING,ptime) %>%
  mutate(rank=ntile(BA,4), BAfrac=BA/sum(BA))

df2_stack_rank_sum <- df2_stack_rank %>%
  group_by(RING,ptime) %>%
  summarise(BAfrac=sum(BAfrac))

df2_stack$co2 <- ifelse(df2_stack$RING==1|df2_stack$RING==2,'ele','amb') 

df2_stack_year_rank <- df2_stack_year %>%
  group_by(RING,year) %>%
  mutate(rank=ntile(BA,4), BAfrac=BA/sum(BA))

df2_stack_year_rank_sum <- df2_stack_year_rank %>%
  group_by(RING,year) %>%
  summarise(BAfrac=sum(BAfrac))

df2_stack_year_rank_quartile <- df2_stack_year_rank %>%
  group_by(RING,co2,year,rank) %>%
  summarise(BAfrac=sum(BAfrac))

df2_stack_year_rank_quartile_diff <- df2_stack_year_rank_quartile %>%
  group_by(co2,RING,rank) %>%
  summarise(deltaBAfrac=BAfrac[length(BAfrac)]-BAfrac[1])
# trees that die
dead <- df2_stack$treeid[df2_stack$ptime==df2_stack$ptime[dim(df2_stack)[1]] & df2_stack$BA==0 ]

# id trees that die
df2_stack <- df2_stack %>%
  mutate(die=treeid%in%dead)

df2_stack_count <- df2_stack %>%
  group_by(ind) %>%
  count()
print(df2_stack_count, n=100 )

df2_stack_ring <- df2_stack %>%
  group_by(ind,RING) %>%
  summarise_at(c('BA'),sum)  
df2_stack_ring$ptime <- strptime(df2_stack_ring$ind, "X%Y.%m.%d")
print(df2_stack_ring, n=100 )



# Duke

ifname <- 'DUKE_a.csv'
dfd1_d <- read.csv(ifname)
head(dfd1_d)
names(dfd1_d)

wdd1 <- '/mnt/disk2/Research_Projects/FACE_modelling/Phase_3/data/DUKE'
setwd(wdd1)

ifname <- 'DUKE_response_all_mcharthyfinzi_byring_all.csv'
dfd1_d1 <- read.csv2(ifname, skip=1, na.strings='' )
head(dfd1_d1)
names(dfd1_d1)

class(dfd1_d1$GWPINE)
dfd1_d1$GWPINE
dfd1_d1$GWPINE[73]
as.numeric(as.character(dfd1_d1$GWPINE))

dfd1_d1$GW     <- as.numeric(as.character(dfd1_d1$GW))
dfd1_d1$GWPINE <- as.numeric(as.character(dfd1_d1$GWPINE))
dfd1_d1$GWHARD <- as.numeric(as.character(dfd1_d1$GWHARD))

dfd1_d1_i <- dfd1_d1 %>%
  group_by(CO2,YEAR) %>%
  summarise_at(c('GW','GWPINE','GWHARD'), mean )
dfd1_d1_i



### BASAL AREA & DBH PLOTS ###
histogram(~BA, df2_stack, subset=ptime==ptime[1] )
histogram(~I(2*(BA/pi)^0.5), df2_stack, subset=ptime==ptime[1] )
histogram(~I(2*(BA/pi)^0.5), df2_stack, subset=ptime==max(ptime) )

names(df2_stack)
densityplot(~BA|RING, df2_stack, groups=ptime, subset=BA!=0,
            col=viridis(length(unique(df2_stack$ptime))), pch=19, cex=0, xlim=c(0,500), breaks=seq(0,540,10) )
densityplot(~BA|co2, df2_stack, groups=ptime, subset=BA!=0,
            col=viridis(length(unique(df2_stack$ptime))), pch=19, cex=0, xlim=c(0,500), breaks=seq(0,540,10) )
densityplot(~I(2*(BA/pi)^0.5)|co2, df2_stack, groups=ptime, subset=BA!=0,
            col=viridis(length(unique(df2_stack$ptime))), pch=19, cex=0, xlim=c(0,30), breaks=seq(0,540,10) )


histogram(~BA|RING, df2_stack, subset=ptime==ptime[1], xlim=c(0,500), breaks=seq(0,540,10) )
histogram(~BA|RING, df2_stack, subset=ptime==ptime[dim(df2_stack)[1]]&BA!=0, xlim=c(0,500), breaks=seq(0,540,10) )
histogram(~BA|RING, df2_stack, type='count', subset=ptime==ptime[1], xlim=c(0,500), breaks=seq(0,540,10) )
histogram(~BA|RING, df2_stack, type='count', subset=ptime==ptime[dim(df2_stack)[1]]&BA!=0, xlim=c(0,500), breaks=seq(0,540,10) )

densityplot(~BA|RING, df2_stack, subset=ptime==ptime[1], xlim=c(0,500), breaks=seq(0,540,10) )
densityplot(~BA|RING, df2_stack, subset=ptime==ptime[dim(df2_stack)[1]]&BA!=0, xlim=c(0,500), breaks=seq(0,540,10) )

densityplot(~BA, df2_stack, groups=RING, subset=ptime==ptime[1], xlim=c(0,500), 
            breaks=seq(0,540,10), auto.key=list(space='right') )
densityplot(~BA, df2_stack, groups=RING, subset=ptime==ptime[dim(df2_stack)[1]]&BA!=0, xlim=c(0,500), 
            breaks=seq(0,540,10), auto.key=list(space='right') )

densityplot(~BA|RING, df2_stack, groups=ptime, subset=ptime==ptime[1]|ptime==ptime[dim(df2_stack)[1]]&BA!=0, xlim=c(0,500), breaks=seq(0,540,10), col=c('red','blue') )

p1 <- 
densityplot(~BA|RING, df2_stack, groups=ptime, subset=BA!=0,
            col=viridis(length(unique(df2_stack$ptime))), as.table=T,
            cex=0, xlim=c(0,500), breaks=seq(0,540,10),
            strip=strip.custom(strip.levels=T),
            sub='Progression of basal area distribution (pdf) over time in each ring. 
                 Colours represent sampling time, purple is first sampling point, yellow the last.',
            panel=function(...) {
              panel.abline(v=seq(100,400,100), col='grey80' )
              panel.densityplot(...)
            })
p1

densityplot(~BA|co2, df2_stack, groups=ptime, subset=BA!=0,
            col=viridis(length(unique(df2_stack$ptime))), pch=19, cex=0, xlim=c(0,500), breaks=seq(0,540,10) )
densityplot(~I((BA/pi)^0.5)|co2, df2_stack, groups=ptime, subset=BA!=0,
            col=viridis(length(unique(df2_stack$ptime))), pch=19, cex=0, xlim=c(0,20), breaks=seq(0,540,10) )


# A vs E first time point
densityplot(~BA, df2_stack, groups=co2, subset=BA!=0&ptime==ptime[1], auto.key=list(space='right'),
            par.settings=simpleTheme(col=viridis(2), pch=19, cex=0.5), xlim=c(0,500), breaks=seq(0,540,10) )
# A vs E last time point
densityplot(~BA, df2_stack, groups=co2, subset=BA!=0&ptime==ptime[dim(df2_stack)[1]], auto.key=list(space='right'),
            par.settings=simpleTheme(col=viridis(2), pch=19, cex=0.5), xlim=c(0,500), breaks=seq(0,540,10) )
# A vs E all time points
densityplot(~BA|ptime, df2_stack, groups=co2, subset=BA!=0, auto.key=list(space='right'),
            par.settings=simpleTheme(col=viridis(2), pch=19, cex=0.5), xlim=c(0,500), breaks=seq(0,540,10) )

# A vs E all years
p4 <- 
densityplot(~BA|year, df2_stack_year, groups=co2, subset=BA!=0, 
            auto.key=list(space='right'), lwd=3, fill=viridis(2), as.table=T,  alpha.fill=0.1,  
            par.settings=simpleTheme(col=viridis(2), pch=19, cex=0.5), xlim=c(0,500), breaks=seq(0,540,10),
            strip=strip.custom(strip.levels=T),
            sub='Combined basal area distribution by co2 treatment for each year',
            panel=function(...) panel.densityplot.poly(...) )
p4

p5 <- 
  densityplot(~BA|year, df2_stack_year, groups=co2, subset=BA!=0&(year==1997|year==2009), 
              auto.key=list(space='right'), lwd=3, fill=viridis(2), as.table=T,  alpha.fill=0.1,  
              par.settings=simpleTheme(col=viridis(2), pch=19, cex=0.5), xlim=c(0,500), breaks=seq(0,540,10),
              strip=strip.custom(strip.levels=T),
              sub='Combined basal area distribution by co2 treatment for each year',
              panel=function(...) panel.densityplot.poly(...) )
p5

# cBAI 
p6 <- 
  densityplot(~cBAI, df2_stack_year_diff, groups=co2, subset=cBAI!=0&RING!=4, 
              auto.key=list(space='right'), lwd=3, fill=viridis(2), as.table=T,  alpha.fill=0.1,  
              par.settings=simpleTheme(col=viridis(2), pch=19, cex=0.5), xlim=c(0,500), breaks=seq(0,540,10),
              strip=strip.custom(strip.levels=T),
              sub='Cumulative basal area increment distribution by co2 treatment',
              panel=function(...) panel.densityplot.poly(...) )
p6

head(df2_stack_year)
head(df2_stack_year_diff)


# A vs E each rings all years
cols <- c( rep(viridis(2)[2],2), rep(viridis(2)[1],3) )
densityplot(~BA|year, df2_stack_year, groups=RING, subset=BA!=0, 
            sub='subtitle',
            auto.key=list(space='right'), lwd=2, fill=cols, as.table=T, alpha.fill=0.1,   
            par.settings=simpleTheme(col=cols, pch=19, cex=0.5), xlim=c(0,500), breaks=seq(0,540,10),
            panel=function(...) panel.densityplot.poly(...) )


densityplot(~BA|year, df2_stack_year, groups=co2, subset=BA!=0&RING!=4, 
            auto.key=list(space='right'), lwd=3, fill=viridis(2), as.table=T, alpha.fill=0.1, 
            par.settings=simpleTheme(col=viridis(2), pch=19, cex=0.5), xlim=c(0,500), breaks=seq(0,540,10),
            panel=function(...) {
              panel.densityplot.poly(...)
              # panel.polygon(x,y,...)
            })

df3$co2 <- ifelse(df3$plot==1|df3$plot==2,'e','a')
bwplot(BAI~as.factor(CC),df3)
bwplot(as.factor(CC)~BAI,df3, groups=as.factor(co2), subset=BAI>0 )
bwplot(BAI~as.factor(CC)|co2,df3,subset=BAI>0)



summary(lm(deltaBAfrac~co2*rank,df2_stack_year_rank_quartile_diff ))
summary(lm(deltaBAfrac~co2+rank,df2_stack_year_rank_quartile_diff ))
summary(lm(deltaBAfrac~co2*rank,df2_stack_year_rank_quartile_diff, subset=RING!=4 ))

summary(lm(deltaBAfrac~co2*as.factor(rank),df2_stack_year_rank_quartile_diff ))
summary(lm(deltaBAfrac~co2*as.factor(rank),df2_stack_year_rank_quartile_diff, subset=RING!=4 ))




bwplot(BA~RING, df2_stack, groups=TREE, type='l', subset=ptime==ptime[1], horizontal=F )
bwplot(BA~RING, df2_stack, groups=TREE, type='l', subset=ptime==ptime[dim(df2_stack)[1]], horizontal=F )

p2 <- xyplot(BA~ptime|RING, df2_stack, groups=TREE, type='l',
             strip=strip.custom(strip.levels=T), as.table=T,
             sub='Basal area dynamic for all trees in each ring')
p3 <- xyplot(BA~ptime|RING, df2_stack, groups=TREE, type='l', subset=die,
             strip=strip.custom(strip.levels=T), as.table=T,
             sub='Basal area dynamic for trees that died in each ring')
xyplot(BA~ptime|RING, df2_stack, groups=TREE, type='l', subset=!die )

xyplot(BA~as.POSIXct(ptime), df2_stack_ring, groups=RING, type='l', auto.key=list(space='right') )


xyplot(BA~ptime|RING*rank, df2_stack_rank, groups=TREE, type='l' )


wdo <- '/mnt/disk2/Write_ups/co-authored papers/20_Norby_FACEstructure/data'
setwd(wdo)
pdf('Figure2_options_rough.pdf',height=11,width=8)
print(update(p1,main='1'))
print(update(p2,main='2'))
print(update(p3,main='3'))
print(update(p4,main='4'))
print(update(p5,main='5'))
print(update(p6,main='6'),split=c(1,2,1,2),more=F)
dev.off()



# BAI vs initial BA
xyplot(cBAI~BA, df2_stack_year_1997_diff, groups=co2.x )
xyplot(cBAI~BA, df2_stack_year_1997_diff_nomort, groups=co2.x, type=c('p','r') )
xyplot(cBAI~BA, df2_stack_year_1997_diff, groups=RING )
xyplot(cBAI~BA, df2_stack_year_1997_diff_nomort, groups=RING, type=c('p','r') )
xyplot(cBAI~BA, df2_stack_year_1997_diff, groups=co2.x, subset=cBAI>0 )
xyplot(log(cBAI)~BA, df2_stack_year_1997_diff, groups=RING, subset=cBAI>0 )
xyplot(log(cBAI)~log(BA), df2_stack_year_1997_diff, groups=RING, subset=cBAI>0, type=c('p','r') )
xyplot(log(cBAI)~log(BA), df2_stack_year_1997_diff_nomort, groups=RING, type=c('p','r') )

m1 <- lmer(log(cBAI)~log(BA)*co2.x + (1 + co2.x|RING), df2_stack_year_1997_diff_nomort, subset=cBAI>0 )
m2 <- lmer(log(cBAI)~log(BA)*co2.x + (1|RING), df2_stack_year_1997_diff_nomort, subset=cBAI>0 )
m3 <- lmer(log(cBAI)~log(BA)+co2.x + (1|RING), df2_stack_year_1997_diff_nomort, subset=cBAI>0 )
m4 <- lmer(log(cBAI)~log(BA) + (1|RING), df2_stack_year_1997_diff_nomort, subset=cBAI>0 )
aictab(list(m1,m2,m3,m4))

ma <- lmer(cBAI~BA*co2.x + (1 + co2.x|RING), df2_stack_year_1997_diff_nomort )
mb <- lmer(cBAI~BA*co2.x + (1|RING), df2_stack_year_1997_diff_nomort )
mc <- lmer(cBAI~BA*co2.x + (1|RING) + (BA|RING), df2_stack_year_1997_diff_nomort )
# md <- lmer(cBAI~BA*co2.x + (BA|RING), df2_stack_year_1997_diff_nomort )
# me <- lmer(cBAI~BA*co2.x + (co2.x|RING), df2_stack_year_1997_diff_nomort )
aictab(list(ma,mb,mc))
m1 <- update(mb, REML=F )
m2 <- lmer(cBAI~BA+co2.x + (1|RING), df2_stack_year_1997_diff_nomort, REML=F  )
m3 <- lmer(cBAI~BA + (1|RING), df2_stack_year_1997_diff_nomort, REML=F  )
mn <- lmer(cBAI~(1|RING), df2_stack_year_1997_diff_nomort, REML=F  )
aictab(list(m1,m2,m3,mn))
summary(m1)



# p3 <- 
# ### DUKE PLOTS ###
# xyplot(GWPINE~YEAR, dfd1_d1_i, groups=CO2, type='b' )
# xyplot(GWHARD~YEAR, dfd1_d1_i, groups=CO2, type='b' )
# 
# xyplot(GWPINE~YEAR, dfd1_d1, groups=Ring, type='b' )
# xyplot(GWHARD~YEAR, dfd1_d1, groups=Ring, type='b' )
# 
# 
# 
# ### ORNL PLOTS ###
# aov(Start_dens~co2,df2)
# t.test(Start_dens~co2,df2)
# t.test(End_dens~co2,df2)
# 
# xyplot(Mort_BA_all~Start_BA, df1a, groups=co2 )
# xyplot(Mort_BA_all~Start_dens, df1a, groups=co2 )
# 
# xyplot(Mort_BA_tree~Start_BA, df1a, groups=co2 )
# xyplot(Mort_BA_tree~Start_dens, df1a, groups=co2 )
# 
# xyplot(mort_BA_frac~Start_BA, df1a, groups=co2 )
# xyplot(mort_BA_frac~Start_dens, df1a, groups=co2 )
# 
# xyplot(df1d_i$CW ~ df1d_c$GW, groups=df1a$co2, abline=list(a=0,b=1))
# xyplot(I(df1d_i$CW) ~ df1d_c$GW, groups=df1a$co2, abline=list(a=0,b=1))
# 
# xyplot(df1d_i$CW ~ df1d_i$NWOOD, groups=df1a$co2, abline=list(a=0,b=1))
# xyplot(df1d_i$CW ~ df1d_first$CW, groups=df1a$co2, abline=list(a=0,b=1))
# xyplot(df1d_i$CW ~ df1d_first$NWOOD, groups=df1a$co2, abline=list(a=0,b=1))
# xyplot(df1d_i$CW ~ I(df1d_first$NWOOD+df1d_first$NCR+df1d_first$NFR), groups=df1a$co2, abline=list(a=0,b=1))
# 
# xyplot(df1d_i$CW~df1d_c$NPP, groups=df2$co2)
# xyplot(df1d_i$CW~Start_BA, df1a, groups=co2 )
# xyplot(df1d_i$CW~Start_dens, df1a, groups=co2 )
# xyplot(df1d_i$CW~mort_frac, df1a, groups=co2 )
# xyplot(df1d_i$CW~Mort_BA_all, df1a, groups=co2 )
# 
# xyplot(df1d_c$NUP~df1d_c$GR, df1a, groups=co2 )
# xyplot(df1d_c$NUP~df1d_c$GL, df1a, groups=co2 )
# xyplot(df1d_i$CW~df1d_c$NUP, df1a, groups=co2 )
# xyplot(df1d_i$CW~df1d_c$GR, df1a, groups=co2 )
# xyplot(df1d_i$CW~df1d_c$NGR, df1a, groups=co2 )
# xyplot(df1d_i$CW~df1d_c$NPP, df1a, groups=co2 )
# 
# xyplot(df1d_c$GR~df1d_first$CW, df1a, groups=co2 )
# 
# xyplot(df1d_i$CW~df1a$Start_BA, groups=df1a$co2 )
# xyplot(df1d_i$CW~df1a$Start_dens, groups=df1a$co2 )
# xyplot(df1d_i$NWOOD~df1a$Start_dens, groups=df1a$co2 )
# 
# xyplot(df1d_first$CW~df1a$Start_BA, groups=df1a$co2 )
# xyplot(df1d_first$CW~df1a$Start_dens, groups=df1a$co2 )
# xyplot(df1d_first$NWOOD~df1a$Start_dens, groups=df1a$co2 )
# 
# xyplot(df1d_first$CFR~df1a$Start_dens, groups=df1a$co2 )
# xyplot(df1d_c$GR~df1a$Start_dens, groups=df1a$co2 )
# 
# 
# 
# xyplot(df1d_first$CW ~ I(df1d_first$NWOOD+df1d_first$NCR+df1d_first$NFR), groups=df1a$co2, abline=list(a=0,b=1))
# 
# xyplot(NPP~YEAR|plot, dfd1 ,groups=co2)
# xyplot(GW~YEAR|plot, dfd1 ,groups=co2)
# xyplot(NGW~YEAR|plot, dfd1 ,groups=co2)
# xyplot(GR~YEAR|plot, dfd1 ,groups=co2)
# xyplot(NGR~YEAR|plot, dfd1 ,groups=co2)
# xyplot(CW~YEAR|plot, dfd1 ,groups=co2)
# xyplot(NWOOD~YEAR|plot, dfd1 ,groups=co2)
# 
# 
# summary(lm(df1d_i$CW~Start_BA, df2 ))
# summary(lm(df1d_i$CW ~ df1d_first$CW))
# summary(lm(df1d_i$CW ~ df1d_first$CW,subset=c(T,T,T,T,F)))
# summary(lm(df1d_i$CW ~ df1d_c$GR))
# 
# 
# # NUP vs. NWOOD
# xyplot(NUP~YEAR|plot, dfd1 ,groups=co2)
# xyplot(NWOOD~YEAR|plot, dfd1 ,groups=co2)
# df1d_i
# 
# xyplot(df1d_i$NUP~df1d_i$NWOOD, df1a, groups=co2 )
# summary(lm(NUP ~ NWOOD, df1d_i ))
# summary(lm(NUP ~ NWOOD - 1, df1d_i ))
# 
# xyplot(df1d_im$NUP~df1d_i$NWOOD, df1a, groups=co2 )
# summary(lm(df1d_im$NUP ~ NWOOD, df1d_i ))
# summary(lm(df1d_im$NUP ~ NWOOD - 1, df1d_i ))
# 
# xyplot(NUP~NWOOD, df1d_im, groups=df1a$co2 )
# summary(lm(NUP ~ NWOOD, df1d_im ))
# summary(lm(NUP ~ NWOOD - 1, df1d_im ))



# N uptake vs. root biomass 
names(dfd1)
xyplot(NUP~CFR,      dfd1, groups=co2, scales='free' )
xyplot(NUP~CFR|plot, dfd1, groups=co2, scales='free' )
xyplot(NUP~CFR|YEAR, dfd1, groups=co2, scales='free' )
xyplot(NUP~GR|plot,  dfd1, groups=co2, scales='free' )
xyplot(NUP~NGR|plot, dfd1, groups=co2, scales='free' )
xyplot(NUP~GR|YEAR,  dfd1, groups=co2, scales='free' )
xyplot(CFR~GR|YEAR,  dfd1, groups=co2, scales='free' )

names(dfd1_d)
class(dfd1_d$NUP)
class(dfd1_d$CFR)
dfd1_d$CFR <- as.numeric(as.character(dfd1_d$CFR))
dfd1_d$NUP <- as.numeric(as.character(dfd1_d$NUP))
dfd1_d$GR  <- as.numeric(as.character(dfd1_d$GR))
xyplot(NUP~CFR,      dfd1_d, groups=co2, scales='free' )
xyplot(NUP~CFR|plot, dfd1_d, groups=co2, scales='free' )
xyplot(NUP~CFR|YEAR, dfd1_d, groups=co2, scales='free' )
xyplot(NUP~GR,       dfd1_d, groups=co2, scales='free' )
xyplot(NUP~GR|plot,  dfd1_d, groups=co2, scales='free' )
xyplot(NUP~NGR|plot, dfd1_d, groups=co2, scales='free' )
xyplot(NUP~GR|YEAR,  dfd1_d, groups=co2, scales='free' )
xyplot(CFR~GR|YEAR,  dfd1_d, groups=co2, scales='free' )

xyplot(CFR~YEAR, dfd1, groups=co2, scales='free' )
xyplot(CCR~YEAR, dfd1, groups=co2, scales='free' )
xyplot(CW~YEAR, dfd1, groups=co2, scales='free' )
xyplot(CL~YEAR, dfd1, groups=co2, scales='free' )



# make root production vs net N mineralisation plot
df1d_i$co2 <- df1a$co2

ornl_rp <- lm(NUP ~ NWOOD - 1, df1d_i )
summary(ornl_rp)
xrange    <- 8:16
ornl_rp_bounds    <- predict(ornl_rp,newdata=data.frame(NWOOD=xrange),interval='confidence')     
ornl_rp_bounds50  <- predict(ornl_rp,newdata=data.frame(NWOOD=xrange),interval='confidence',level=0.682)     

p2 <- 
  xyplot(NUP~NWOOD,df1d_i,groups=co2,
         pch=17,col=c('blue','red'),
         ylab=list(expression(Delta*N[uptake]*' ['*gN*' '*m^-2*']'),cex=0.7),
         ylim=c(-6,-1),
         xlim=c(7,17),
         xlab=list(expression(Delta*N[wood]*' ['*gN*' '*m^-2*']'),cex=0.7),
         scales=list(tck=c(-1,0)),
         panel=function(...){
           panel.abline(v=0,col='grey90')
           panel.polygon(c(xrange,rev(xrange)),c(ornl_rp_bounds[,2],rev(ornl_rp_bounds[,3])),border=NA,col='grey70',alpha=0.2)
           panel.polygon(c(xrange,rev(xrange)),c(ornl_rp_bounds50[,2],rev(ornl_rp_bounds50[,3])),border=NA,col='grey70',alpha=0.3)
           panel.lines(x=xrange,y=ornl_rp_bounds[,1],lwd=1.5,col='grey50')               
           panel.xyplot(...) 
         })
p2

NWOODm <- mean(df1d_i$NWOOD)
df1d_i$NWOOD_cen <- df1d_i$NWOOD - NWOODm 
ornl_rp <- lm(NUP ~ NWOOD_cen, df1d_i)
summary(ornl_rp)
xrange    <- seq(-3.2,3.2,0.2)
ornl_rp_bounds    <- predict(ornl_rp,newdata=data.frame(NWOOD_cen=xrange),interval='confidence')     
ornl_rp_bounds50  <- predict(ornl_rp,newdata=data.frame(NWOOD_cen=xrange),interval='confidence',level=0.682)     

cex <- 1
p2 <- 
  xyplot(NUP~I(NWOOD_cen+NWOODm), df1d_i, groups=co2,
         pch=17,col=c('blue','red'),
         ylab=list(expression(Delta*N[uptake]*' ['*gN*' '*m^-2*']'),cex=cex),
         ylim=c(-5,-2),
         xlim=c(8,16),
         xlab=list(expression(Delta*N[wood]*' ['*gN*' '*m^-2*']'),cex=cex),
         scales=list(tck=c(-1,0)),
         panel=function(...){
           panel.abline(v=0,col='grey90')
           panel.polygon(c(xrange,rev(xrange))+NWOODm, c(ornl_rp_bounds[,2],rev(ornl_rp_bounds[,3])),     border=NA, col='grey70', alpha=0.2 )
           panel.polygon(c(xrange,rev(xrange))+NWOODm, c(ornl_rp_bounds50[,2],rev(ornl_rp_bounds50[,3])), border=NA, col='grey70', alpha=0.3 )
           panel.lines(x=xrange,y=ornl_rp_bounds[,1],lwd=1.5,col='grey50')
           panel.text(12.5, -2.5, labels=expression(R['adj.']^2*' = '*0.76*', P = '*0.035*'*') )
           panel.xyplot(...) 
         })
p2

df1d_i
df1d_final
df1d_first


### Print ###
setwd(wd)

pdf('Figure_BAdist_R4.pdf',height=3.3,width=4)
print(p1)
dev.off()

pdf('Figure_deltaNUP.pdf',height=3.8,width=4)
print(p2)
dev.off()




### END ###