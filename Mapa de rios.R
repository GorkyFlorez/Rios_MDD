#------------------------------------------------------------------------
library(RPostgres)
library(sf)
library(ggplot2)
library(ggspatial)
library(raster)
library(ggnewscale)
dvr         <- RPostgres::Postgres()
db          <- 'postgres'  ##Nombre de la BBDD
host_db     <- 'localhost'
db_port     <- '5432' 
db_user     <- 'postgres'  ##Tu usuario
db_password <- 'gflorezc' ##Tu contraseña 

# 3.0 Conexión
con <- dbConnect(dvr, dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)  
dbListTables(con)

Poligo_MDD_rio

Red_hidrografica<- st_read(con, layer = "Red_hidrografica")
Rio_secundario<- st_transform(Red_hidrografica ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Puerto_rios_MDD <- st_read(con, layer = "Rios_prin_MDD")
Puerto_rios_MDD<- st_transform(Puerto_rios_MDD  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Provi_MDD <- st_read(con, layer = "Provi_MDD")
Provi_MDD <- st_transform(Provi_MDD  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Per          <- getData('GADM', country='Peru', level=1) %>% st_as_sf()

Peru =ggplot()+
  geom_sf(data = Per, fill=NA, color="white", size=0.9)+
  geom_sf(data = Provi_MDD, fill="white", color="white", size=0.8)+
  theme_void()+
  annotate(geom = "text", x = -80 ,y = 0, hjust = 0, vjust = 1, 
           label = "Republic \nof Peru",size = 2, family="serif", color = 
             "white",  fontface="italic")
           


Mapa = ggplot()+
  geom_sf(data = Provi_MDD, fill="#08306B", color="#08306B")+
  geom_sf(data = Rio_secundario,  linewidth = 0.03, color="white")+
  geom_sf(data = Puerto_rios_MDD,  linewidth = 0.1, color="white")+
  theme_bw()+
  theme( plot.background = element_rect(fill = "#0F0D3E"),
         
         panel.grid.major = element_line(color = "#90e0ef", linetype = "dashed", size=0.0008, alpha=0.5),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1),
        axis.text.y  = element_text(angle = 90,face="bold", color="white",family="serif",size=8),
        axis.text.x  = element_text(face="bold", color="white", size=8,family="serif"),
        panel.background = element_rect(fill = "#0F0D3E"))+
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  annotate(geom = "text", x = -72.3, y = -13.0, hjust = 0, vjust = 1, 
           label = "Author: Gorky Florez Castillo \n     Produced Using R 4.2.2 ",size = 6, family="serif", color = 
             "white",  fontface="italic", face = "bold")+
             annotate(geom = "text", x = -70.5, y = -10, hjust = 0, vjust = 1, 
                      label = "       RIOS DE \nMADRE DE DIOS ",size = 5, family="serif", color = 
                        "white",  fontface="italic", face = "bold")+
  labs(x = '', y = '',  title="")
  
  

library(cowplot)
Expo = ggdraw() +
  coord_equal(xlim = c(0, 25), ylim = c(0, 25), expand = FALSE) +
  draw_plot(Mapa , width = 25, height = 25,x = 0, y = 0)+
  draw_plot(Peru , width = 5, height = 5,x = 18, y = 17)+
  
  theme(panel.background = element_rect(fill = "black"))
Expo

ggsave(plot=Expo ,"Mapa de rios MDD.png",units = "cm",width = 25, #ancho
       height = 25, #alargo
       dpi=1200)


