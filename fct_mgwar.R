# https://doi.org/10.1016/j.regsciurbeco.2017.04.001

library(mgwrsar)


# ?mgwrsar::

sdp
# formula_vars = "ln_emig_pc ~ W_ln_emig_pc+ mean_salary + ln_higherEduc_pc + eci + is_coastal + region_"
formula_vars = "ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_"


W=kernel_matW(H=4,kernels='rectangle',coord_i=coords,NN=4,adaptive=TRUE,diagnull=TRUE,rowNorm=T)

ptm1<-proc.time()
model_GWR0<-MGWRSAR(
  formula = "ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", 
  data = dfs_shp,
  coords=as.matrix(dfs_shp[, c("centlng", "centlat")]), 
  fixed_vars=NULL,
  kernels=c('gauss'),
  H=0.13, 
  Model = 'GWR',
  control=list(SE=T,get_ts=TRUE)
)
(proc.time()-ptm1)[3]

summary_mgwrsar(model_GWR0)


# coords <- as.matrix(dfs_shp[, c("centlng", "centlat")])
# nb <- knearneigh(coords, k = 5)
# lw <- nb2listw(nb)
# lw_matrix <- listw2mat(lw)
# nb <- poly2nb(dfs_shp, row.names = dfs_shp$cd_micro)
# lw <- nb2listw(nb)
# print("I am Ready!")

w <- as.numeric(as.character(lw))
w <- as.numeric(lw)
w= as.matrix(as.character(lw))

# bandwidths_mgwrsar("ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", 
#                    dfs_shp,
#                    coords,
#                    fixed_vars='Intercept',
#                    Models='GWR',
#                    candidates_Kernels='bisq',
#                    control=list(),control_search=list())

m2<-MGWRSAR(
  formula = "ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_", 
  data = dfs_shp,
  fixed_vars="is_coastal",
  coords=as.matrix(dfs_shp[, c("centlng", "centlat")])  , 
  kernels=c('gauss'),
  H=0.13, 
  Model = 'MGWRSAR_0_kc_kv',
  control=list(SE=T,get_ts=TRUE,W=w)
)

# ----------

library(mgwrsar)
## loading data example
data(mydata)
coords=as.matrix(mydata[,c("x","y")])
mytab<-bandwidths_mgwrsar(formula = 'Y_gwr~X1+X2+X3', data = mydata,coords=coords,
                          fixed_vars=c('Intercept','X1'),Models=c('GWR','MGWR'),candidates_Kernels=c('bisq','gauss'),
                          control=list(NN=300,adaptive=TRUE),control_search=list())

names(mytab)
names(mytab[['GWR_bisq_adaptive']])
mytab[['GWR_bisq_adaptive']]$config_model
mytab[['GWR_bisq_adaptive']]$CV
summary(mytab[['GWR_bisq_adaptive']]$model$Betav)
mybestmodel=mytab[['GWR_gauss_adaptive']]$model
plot_mgwrsar(mybestmodel,type='B_coef',var='X2')

# ---------------

library(mgwrsar)
## loading data example
data(mydata)
coords=as.matrix(mydata[,c("x","y")])
## Creating a spatial weight matrix (sparce dgCMatrix)
## of 4 nearest neighbors with 0 in diagonal
W=kernel_matW(H=4,kernels='rectangle',coord_i=coords,NN=4,adaptive=TRUE,
              diagnull=TRUE,rowNorm=TRUE)
mgwrsar_0_kc_kv<-MGWRSAR(formula = 'Y_mgwrsar_0_kc_kv~X1+X2+X3', data = mydata,
                         coords=coords, fixed_vars='X2',kernels=c('gauss'),H=20, Model = 'MGWRSAR_0_kc_kv',
                         control=list(SE=FALSE,adaptive=TRUE,W=W))
summary_mgwrsar(mgwrsar_0_kc_kv)

# -----

library(mgwrsar)
formula_vars = as.formula("ln_emig_pc ~ mean_salary + ln_higherEduc_pc + ECI + is_coastal + region_")
coords=as.matrix(dfs_shp[,c("x","y")])
W=kernel_matW(
  H=4,
  kernels='rectangle',
  coord_i=as.matrix(dfs_shp[, c("centlng", "centlat")]),
  NN=4,
  adaptive=TRUE,
  diagnull=TRUE,
  rowNorm=TRUE)
mgwrsar_0_kc_kv<-MGWRSAR(
  formula = formula_vars, 
  data = dfs_shp,
  coords=as.matrix(dfs_shp[, c("centlng", "centlat")]), 
  # fixed_vars='is_coastal',
  fixed_vars=c("Intercept", "region_North", "region_Northeast", "region_South", "region_Southeast"),
  kernels=c('gauss'),
  H=20, 
  Model = 'MGWRSAR_0_kc_kv',
  control=list(SE=FALSE,adaptive=TRUE,W=W))
summary_mgwrsar(mgwrsar_0_kc_kv)

m2<-?MGWRSAR(
  formula = formula_vars, 
  data = dfs_shp,
  coords=as.matrix(dfs_shp[, c("centlng", "centlat")]), 
  # fixed_vars='is_coastal',
  # fixed_vars=c("Intercept", "region_North", "region_Northeast", "region_South", "region_Southeast"),
  kernels=c('gauss'),
  H=20, 
  Model = 'GWR',
  control=list(SE=FALSE,adaptive=TRUE,W=W)
  )
summary_mgwrsar(m2)
m2$Betav

# -------------

mgwrsar_1_kc_kv<-MGWRSAR(
  formula = formula_vars, 
  data = dfs_shp,
  coords=as.matrix(dfs_shp[, c("centlng", "centlat")]), 
  # fixed_vars='is_coastal',
  fixed_vars=c("Intercept", "is_coastal", "region_North", "region_Northeast", "region_South", "region_Southeast"),
  kernels=c('gauss'),
  H=20, 
  Model = 'MGWRSAR_1_kc_kv',
  control=list(SE=FALSE,adaptive=TRUE,W=W))
summary_mgwrsar(mgwrsar_1_kc_kv)
mgwrsar_1_kc_kv$data
mgwrsar_1_kc_kv$Betav
mgwrsar_1_kc_kv$Betac
mgwrsar_1_kc_kv$sev
mgwrsar_1_kc_kv$se
mgwrsar_1_kc_kv$Y
mgwrsar_1_kc_kv$isgcv
mgwrsar_1_kc_kv$edf
mgwrsar_1_kc_kv$tS
mgwrsar_1_kc_kv$AICc
mgwrsar_1_kc_kv$formula
mgwrsar_1_kc_kv$Method
mgwrsar_1_kc_kv$H
mgwrsar_1_kc_kv$fixed_vars
mgwrsar_1_kc_kv$kernels
mgwrsar_1_kc_kv$adaptive
mgwrsar_1_kc_kv$fit
mgwrsar_1_kc_kv$pred
mgwrsar_1_kc_kv$residuals
mgwrsar_1_kc_kv$RMSE
mgwrsar_1_kc_kv$RMSEn
mgwrsar_1_kc_kv$SSR
mgwrsar_1_kc_kv$Type
mgwrsar_1_kc_kv$S
mgwrsar_1_kc_kv$NN
mgwrsar_1_kc_kv$TP
mgwrsar_1_kc_kv$doMC
mgwrsar_1_kc_kv$ncore
mgwrsar_1_kc_kv$mycall
mgwrsar_1_kc_kv$ctime
# mgwrsar_1_kc_kv$



OlS 
SAR
GWR
MGWR
MGWAR 

