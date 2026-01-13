library(dplyr)
library(sf)
library(tidyr)

#setwd('~/BSU/diss/Owyhee/')

#csv = read.csv('./owyheeHUC12narrow.csv')

huc12narrow = st_read('./owyheeHUC12narrow.shp')
#huc12narrow = st_read(csv, geometry_column = ".geo")
huc12wide = st_read('./owyheeHUC12wide.shp')

huc12narrow.wider = huc12narrow %>% pivot_wider(names_from = 'date', values_from = 'mesicProp')
huc12wide.wider = huc12wide %>% pivot_wider(names_from = 'date', values_from = 'mesicProp')

## remove hucs w no data
huc12narrow.wider = huc12narrow.wider %>%
  drop_na('062017')
huc12wide.wider = huc12wide.wider %>%
  drop_na('062017')

## columns with mesic veg proportions
cols = seq(21, 48)

## remove geometry to manipulate
huc12narrow.wider.nogeom = st_drop_geometry(huc12narrow.wider)
huc12wide.wider.nogeom = st_drop_geometry(huc12wide.wider)
  
## add CV column

calc_cv = function(x){
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
}

##https://stackoverflow.com/questions/21783871/calculating-a-linear-trend-line-for-every-row-of-a-table-in-r
calc_slp = function(x) {
  y = t(x)
  y = y[!is.na(y)]
  #y = na.omit(y)
  len = length(y):1
  b = cov(y,len)/var(len)
  
  return(b)}

## add properties for CV, trend
huc12narrow.wider.nogeom =  huc12narrow.wider.nogeom %>% 
  mutate(cv = apply(huc12narrow.wider.nogeom[,cols], 1,calc_cv)) %>%
  mutate(trend = apply(huc12narrow.wider.nogeom[,cols],1,calc_slp))

huc12wide.wider.nogeom =  huc12wide.wider.nogeom %>% 
  mutate(cv = apply(huc12wide.wider.nogeom[, cols],1,calc_cv)) %>%
  mutate(trend = apply(huc12wide.wider.nogeom[,cols],1,calc_slp))

## reset geometry
huc12narrow.out = cbind(huc12narrow.wider.nogeom, huc12narrow.wider$geometry)
huc12wide.out = cbind(huc12wide.wider.nogeom, huc12wide.wider$geometry)

#plot_sf(huc12narrow.out$trend)

## write to SHP
st_write(huc12narrow.out, './out/owyheeHUC12narrowTrend.shp')
st_write(huc12wide.out, './out/owyheeHUC12wideTrend.shp')
