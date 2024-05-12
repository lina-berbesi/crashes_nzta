
library(dplyr,tidyr,data.table) # packages for processing data
library(ggspatial,ggplot2,gridExtra,viridis) # pacakges for visualizations
library(sf) # package for simple feature a standardized way to encode spatial vector data by Ebesma
library(spatstat) # package for spatial statistics by Baddeley
library(stars) # spatio temporal array package 
library(spgwr) # package for geographically weighted regressions by Bivand
library(spdep) # package for calculating moran's I
library(rjags,R2jags) # package for bayes modelling 
library(fitdistrplus)

# Loading shapefiles from crashes and administrative geographies

crashes_sf_tmp<-sf::st_read("Crash_Analysis_System_(CAS)_data/Crash_Analysis_System_(CAS)_data.shp")
ta_polygons_sf_tmp<-sf::st_read("statsnz-territorial-authority-2023-clipped-generalised-SHP/territorial-authority-2023-clipped-generalised.shp")
higher_geographies<-sf::st_read("statsnz-meshblock-higher-geographies-2023-high-definition-CSV/meshblock-higher-geographies-2023-high-definition.csv")
roads_sf<-sf::st_read("QGIS/roads_intersected_tas.shp")
pop_est_df<-data.table::fread("Estimated Resident Population for Territorial Authority Areas, at 30 June(1996+) (Annual-Jun).csv")

ta_polygons_sf<-ta_polygons_sf_tmp %>% filter(!TA2023_V1_ %in% c("067","999"))

higher_geographies_ta<-higher_geographies %>% dplyr::select(MB2023_V1_00,TA2023_V1_00,TA2023_V1_00_NAME,REGC2023_V1_00,REGC2023_V1_00_NAME) %>% 
                                              sf::st_drop_geometry() %>% unique() %>%
                                              mutate(meshblockI=as.numeric(MB2023_V1_00))

# Sub-setting crashes data set variables

crashes_sf<- crashes_sf_tmp %>% dplyr::select(areaUnitID,crashYear,crashLocat,meshblockI,region,crashSever,light,roadSurfac,roadLane,speedLimit) %>% mutate(row=row_number())

#  Adding geometry from crashes as lon and lat

latlon_crashes_sf<- crashes_sf %>% sf::st_transform(4326) %>% sf::st_coordinates() %>% as.data.frame() %>% mutate(row=row_number())
colnames(latlon_crashes_sf)<-c("lon","lat","row")

# Sub-setting crashes data set to a specific year

LAST_AVAILABLE_YEAR<-max(crashes_sf$crashYear)
crashes_sub_sf<-crashes_sf %>% left_join(latlon_crashes_sf,by="row")  %>%
                               left_join(higher_geographies_ta,by="meshblockI") %>%
                               filter(crashYear %in% seq(from=LAST_AVAILABLE_YEAR-5,to=LAST_AVAILABLE_YEAR,by=1) & !is.na(region) & !is.na(TA2023_V1_00)) #& TA2023_V1_00_NAME != "Chatham Islands Territory")

nrow(crashes_sub_sf)

View(crashes_sub_sf)

# Point-in-polygon analysis from crashes in Territorial Authorities

crs_ta_poly<-sf::st_crs(ta_polygons_sf)
pts_crashes_sf<-sf::st_transform(crashes_sub_sf,crs=crs_ta_poly) # making sure polygons and crashes have the same coordinate reference system

#pts_crashes_ta<-sf::st_intersection(ta_polygons_sf,pts_crashes_sf) %>% sf::st_drop_geometry() # perform the point-in-polygon analysis using the intersection function

# calculating the density - kernel density estimates - from the points data

start_time_ppp<-Sys.time()
pts_crashes_ppp<-as.ppp(pts_crashes_sf$geometry,W=as.owin(ta_polygons_sf)) # <1min
end_time_ppp<-Sys.time()
end_time_ppp-start_time_ppp

pts_crashes_stars<-st_as_stars(density(pts_crashes_ppp,dymx=600)) # dymx controls number of pixels of the density / higher number higher resolution

plot(pts_crashes_stars)

pts_crashes_density<-sf::st_as_sf(pts_crashes_stars) %>% sf::st_set_crs(crs_ta_poly)

# Plotting pts crashes 

nz_sf<-sf::st_union(ta_polygons_sf)

map_crashes_pts<- ggplot() +
              geom_sf(dat=nz_sf) +
              geom_sf(data=crashes_sub_sf %>% filter(crashYear==LAST_AVAILABLE_YEAR),color="black",shape=16,size=0.5) +
              xlab(expression(paste("Longitude (",degree,"E)"))) +
              ylab(expression(paste("Latitude (",degree,"S)"))) +
              scale_x_continuous(breaks=seq(170,184,by=2)) +
              labs(title=paste0("Crashes locations in ",LAST_AVAILABLE_YEAR),
                   subtitle=expression(atop(italic("CAS data")))) +
              ggspatial::annotation_scale(location="br") +
              ggspatial::annotation_north_arrow(location="tr",
                                                height=unit(1,"cm"),
                                                width=unit(0.5,"cm"),
                                                style=ggspatial::north_arrow_fancy_orienteering()) +
              theme(legend.position="none")

map_crashes_pts

ggsave(map_crashes_pts,file="Documents/NZTA/nz_crashes_points.png")

# choropleth map by TA YoY growth

crashes_grpd<-as.data.table(sf::st_drop_geometry(crashes_sub_sf))
crashes_grpd<-crashes_grpd[,.(crashes_nbr=.N),by=c("TA2023_V1_00_NAME","crashYear")][order(-TA2023_V1_00_NAME,-crashYear,decreasing=TRUE),]
crashes_grpd[,crashes_nbr_lag:=data.table::shift(crashes_nbr,1),by=c("TA2023_V1_00_NAME")]
crashes_grpd[,crashes_yoy_growth:=(crashes_nbr/crashes_nbr_lag)-1]

View(crashes_grpd)

crashes_by_ta<- ta_polygons_sf %>% left_join(crashes_grpd %>% filter(crashYear==2023) %>% select(TA2023_V1_00_NAME,crashes_nbr),by=c("TA2023_V_1"="TA2023_V1_00_NAME"))

map_crashes_choropleth<-ggplot() +
                      geom_sf(data=ta_polygons_sf,fill="black") +
                      geom_sf(data=crashes_by_ta %>% filter(TA2023_V_1!="Auckland"),aes(fill=crashes_nbr)) +
                      geom_sf(data=st_boundary(ta_polygons_sf),linewidth=0.20) +
                      xlab(expression(paste("Longitude (",degree,"E)"))) +
                      ylab(expression(paste("Latitude (",degree,"S)"))) +
                      scale_x_continuous(breaks=seq(170,184,by=2)) +
                      labs(title=paste0("Choropleth crashes in ",LAST_AVAILABLE_YEAR),
                           subtitle=expression(atop(italic("CAS data"))),
                           caption="Author: Lina Berbesi \n Source: Waka Kotahi(NZTA)",
                           fill="Crashes N") +
                      scale_fill_viridis_c(direction=-1,labels=scales::comma) +
                      ggspatial::annotation_scale(location="br") +
                      ggspatial::annotation_north_arrow(location="tr",
                                                        height=unit(1,"cm"),
                                                        width=unit(0.5,"cm"),
                                                        style=ggspatial::north_arrow_fancy_orienteering()) +
                      theme(legend.title = element_text( size=10), legend.text=element_text(size=9),
                            legend.key.height= unit(0.5, 'cm'),legend.key.width= unit(0.5, 'cm'))

map_crashes_choropleth

ggsave(map_crashes_choropleth,file="Documents/NZTA/nz_crashes_choropleth.png")

map_crashes_allplots<-gridExtra::grid.arrange(map_crashes_pts,map_crashes_choropleth,ncol=2,nrow=1)
map_crashes_allplots
ggsave(map_crashes_allplots,file="Documents/NZTA/nz_crashes_allplots.png")

# Local Moran's I

crashes_2023<- crashes_sub_sf %>% filter(crashYear==LAST_AVAILABLE_YEAR) %>% st_drop_geometry() %>% group_by(TA2023_V1_00_NAME) %>% summarize(crashes_nbr=n())

crashes_polygons_2023<- ta_polygons_sf %>% left_join(crashes_2023,by=c("TA2023_V_1"="TA2023_V1_00_NAME"))

crashes_sub_nb<- spdep::poly2nb(as_Spatial(crashes_polygons_2023), queen=TRUE)

crashes_sub_weights <- spdep::nb2listw(crashes_sub_nb, style="W", zero.policy=TRUE)

local_morans <- spdep::localmoran(crashes_polygons_2023$crashes_nbr, crashes_sub_weights, zero.policy=TRUE) %>% as.data.frame()

crashes_sub_localMorans <- cbind(local_morans, as.data.frame(crashes_polygons_2023)) %>% sf::st_sf() %>%
                            dplyr::select(`Traffic crash count` = crashes_nbr, 
                                          `Local Morans I` = Ii, 
                                          `P Value` = `Pr(z != E(Ii))`) %>%
                            mutate(`Significant Hotspots` = ifelse(`P Value` <= 0.005, 1, 0)) %>%
                            tidyr::gather(Variable, Value, -geometry)

vars <- unique(crashes_sub_localMorans$Variable)
varList <- list()

for(i in vars){
if(i!="Significant Hotspots"){
  varList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(crashes_sub_localMorans, Variable == i), 
            aes(fill = Value), colour=NA) +
    scale_fill_viridis(name="") +
    labs(title=i) + theme(legend.position="bottom")}
else{
  varList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(crashes_sub_localMorans, Variable == i), 
            aes(fill = Value), colour=NA) +
    scale_fill_viridis(name="") +
    labs(title=i) + labs(caption="Author: Lina Berbesi \n Source: Waka Kotahi(NZTA)") + theme(legend.position="bottom")
}}

do.call(grid.arrange,c(varList, ncol = 4))
ggsave(do.call(grid.arrange,c(varList, ncol = 4, top = "Local Morans I statistics on Crashes")),file="Documents/NZTA/local_morans_plots.png")

# Bayes Poisson model

pop_est_long<-pop_est_df %>% pivot_longer(cols=c(-Year),
                                          names_to="TA2023_V1_00_NAME",
                                          values_to="popest")

hghwy_counts<-roads_sf %>% sf::st_drop_geometry() %>% filter(!is.na(name)) %>% summarise(hghwy_counts=length(unique(grep("\\<HIGHWAY\\>",name))), .by = TA2023_V_1)

data_model<-crashes_sub_sf %>% sf::st_drop_geometry() %>% select(crashYear,roadSurfac,speedLimit,TA2023_V1_00_NAME) %>%
                                                          mutate(sealed=case_when(roadSurfac %in% c("Sealed","End of seal")~ 1,
                                                                                       TRUE ~ 0),
                                                                 speedlimit=as.integer(speedLimit)) %>%
                                                          group_by(crashYear,sealed,speedlimit,TA2023_V1_00_NAME) %>%
                                                          summarize(crashes_count=n()) %>% ungroup() %>%
                                                          left_join(pop_est_long,by=c("TA2023_V1_00_NAME","crashYear"="Year"),relationship="many-to-many") %>%
                                                          left_join(hghwy_counts,by=c("TA2023_V1_00_NAME"="TA2023_V_1")) %>%
                                                          as.data.frame() %>% na.omit() 

View(data_model)

# Crashes count distribution


plot(density(as.numeric(data_model$crashes_count)))

fitdistrplus::descdist(as.numeric(data_model$crashes_count), discrete = FALSE)

fitnormintercept<- fitdistrplus::fitdist(as.numeric(data_model$crashes_count), "norm")
fitnormintercept

# Speed limit distribution

plot(density(as.numeric(data_model$speedlimit)))

fitdistrplus::descdist(as.numeric(data_model$speedlimit), discrete = FALSE)

fitnormspeed<- fitdistrplus::fitdist(as.numeric(data_model$speedlimit), "norm")
fitnormspeed
plot(fitnormspeed)

# roads distribution

plot(density(as.numeric(data_model$hghwy_counts)))

fitdistrplus::descdist(as.numeric(data_model$hghwy_counts), discrete = FALSE)

fitnormhghwy<- fitdistrplus::fitdist(as.numeric(data_model$hghwy_counts), "norm")
fitnormhghwy

# Setting Model parameters

dat.list <- with(data_model,list(Y=crashes_count, X=sealed,Z=speedlimit,W=hghwy_counts))
poisson_model <- "model{
# Likelihood model for Y[i]
for(i in 1:length(Y)) {
Y[i] ~ dpois(l[i])
log(l[i]) <- beta0 + beta1*X[i] + beta2*Z[i] + beta3*W[i]
}
# Prior models for beta0,beta1,beta2
beta0 ~ dnorm(43.73782 , 211.2847 ^2)
beta1 ~ dnorm(0, 1)
beta2 ~ dnorm(0, 26.62074 ^2)
beta3 ~ dnorm(0,41.33789 ^2)
}"

writeLines(poisson_model,con='poisson_model.txt')

start_time_jags<-Sys.time()
poisson_bayes_model <- R2jags::jags(model='poisson_model.txt', data=dat.list, inits=NULL,
                     param=c('beta0','beta1','beta2','beta3'),
                     n.chain=3,
                     n.iter=20000, n.thin=10, n.burnin=10000)
end_time_jags<-Sys.time()
end_time_jags-start_time_jags

poisson_bayes_model

plot(as.mcmc(poisson_bayes_model))

autocorr.diag(as.mcmc(poisson_bayes_model))
























