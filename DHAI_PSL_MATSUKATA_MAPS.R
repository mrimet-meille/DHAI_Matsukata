############## La cartographie avec R ############## 

library(tidyverse) 
library(questionr)
library(sf) # pour manipuler des données spatiales
library(sp) # pour manipuler des données spatiales
library(rnaturalearth) # pour importer le fond de carte

# Importer les jeux de données -------
orga_expo <- read.csv("data/organisation_expos.csv", encoding = "UTF-8")
villes_expo <- read.csv("data/villes_expo_wgs84.csv", encoding = "UTF-8")


glimpse(orga_expo)

## Recoder orga_expo$ville en orga_expo$ville_rec --------
orga_expo$ville_rec <- orga_expo$ville %>%
  fct_recode(
    "Aix-en-Provence" = "Aix-en-Provence et Nice",
    "Belle Fourche (Centre géographique des États-Unis)" = "Plusieurs villes",
    "Caracas" = "Caraca"
  )
# Les expositions itinérantes ne sont ici comptées qu'une seule fois

## Calculer le nombre d'expositions dans chaque ville -------
orga_expo2 <- orga_expo %>% distinct_at("ID_E", .keep_all = TRUE)

nb_expos <- table(orga_expo2$ville_rec, useNA = "always")
nb_expos <- as.data.frame(nb_expos)

colnames(nb_expos) <- c("ville", "nb_expo")

## Ajouter la longitude et la latitude pour chaque ville ------
nb_expos_complet <- merge(nb_expos, villes_expo, by = "ville")
nb_expos_complet <- nb_expos_complet[,c("id_ville", "ville", "nb_expo", "long", "lat")]

# Import du fond de carte  -----------

# Ici, le fond de carte est importé à partir du package rnaturalearth mais il est aussi possible d'essayer de trouver des données spatiales en libre accès qui sont plus appropriées à son objet
# Les formats shapefile (.shp) et geojson sont les plus utilisés pour les données spatiales. 
# Il est possible d'importer des données dans R avec les fonctions st_read() du package sf ou geojson_sf du package geojsonsf

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
ggplot(data = world) +
  geom_sf() +
  theme_bw()

st_crs(world) # Fonction pour connaître le système de projection utilisé par un objet spatial

# Choisir un système de projection approprié pour la visualisation des données --------

# Ici, on va utiliser le système de projection ETRS89 LAEA (Lambert Equal-Area projection) EPSG 3035
# Chaque système de projection possède un code EPSG
# Les deux systèmes de projection les plus utilisés sont le WGS84 EPSG4326 et le Lambert-93 EPSG 2154
# Le Lambert 93 est aujourd'hui le système de projection officiel utilisé pour les données française
# Si on ne connaît pas le code EPSG du système de projection, il est possible de passer par PROJ4 pour transformer le système de coordonnées

## Transformer les coordonnées --------
# On cherche à calculer les nouvelles coordonnées dans le nouveau système de projection

# On indique tout d'abord à quel système de projection appartiennent les coordonnées récupérées pour chaque ville ayant organisé une exposition avec la collection Matsukata
coord_cities <- villes_expo %>% select("long", "lat")
coordinates(coord_cities) <- ~ long + lat
proj4string(coord_cities) <- CRS("+proj=longlat +datum=WGS84")

# On effectue la transformation dans le nouveau système de coordonnées
projection <- spTransform(coord_cities, CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))

new_coord <- coordinates(projection)
new_coord <- as.data.frame(new_coord)
new_coord <- rowid_to_column(new_coord)

colnames(new_coord) <- c("id_ville", "long_proj", "lat_proj")

new_coord

## Rajouter les nouvelles coordonnées aux bases de données ---------
villes_expo <- merge(new_coord, villes_expo, by = "id_ville")

nb_expos_complet <- merge(new_coord, nb_expos_complet, by = "id_ville")
nb_expos_complet <- nb_expos_complet[,c("id_ville", "ville", "nb_expo", "long", "lat", "long_proj", "lat_proj")]
nb_expos_complet$nb_expo <- as.numeric(nb_expos_complet$nb_expo, ordered = T)

# Visualisation avec ggplot ---------
a <- ggplot(data = world) +
  geom_sf(fill = "white", color="black", size=.1) +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ") + 
  scale_size_continuous(range = c(2,3), breaks = seq(1,2, by = 1)) +
  scale_color_continuous(breaks = seq(1,10, by = 1)) +
  geom_point(nb_expos_complet, mapping = aes(long_proj,lat_proj, color = nb_expo, size = nb_expo), alpha = 0.7) +
  ggtitle("Villes ayant organisé au moins une exposition avec la collection Matsukata (1946-1959)")  +
  labs(x = "Longitude", y = "Latitude", size = "Nombre d'expositions", color = "Nombre d'expositions", caption = "Les expositions itinérantes ne sont comptabilisées qu'une seule fois.\nSources : Léa Saint-Raymond (2019)") +
  theme_bw() +
  guides(color = guide_legend(), size = guide_legend())

a

#Pour sauvegarder la visualisation
#ggsave("carte_expos_matsu.pdf",width=10,height=10) 


##### Sources: 

### Surles représentations graphiques avec r avec ggplot : 
# https://bookdown.org/content/b298e479-b1ab-49fa-b83d-a57c2b034d49/
# https://ggplot2-book.org/index.html
# https://r-graph-gallery.com/index.html

### Sur la cartographie avec R:
# https://r-spatial.org/r/2018/10/25/ggplot2-sf.html
# https://geocompr.robinlovelace.net/index.html