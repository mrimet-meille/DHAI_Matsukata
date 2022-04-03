############## La cartographie avec R ############## 

library(tidyverse) 
library(questionr)
library(ggrepel)
library(sf) # pour manipuler des données spatiales
library(sp) # pour manipuler des données spatiales
library(rnaturalearth) # pour importer le fond de carte




# Importer les jeux de données -------



liste_expo <- read.csv("data/liste_expositions.csv", encoding = "UTF-8")
liste_expo_itin <- read.csv("data/liste_expos_itinerantes.csv", encoding = "UTF-8") 
# La deuxième base de donnée est issue des opérations effectuées à partir de la première.
# Il est possible d'exporter une base de donnée dans R comme fichier csv avec la fonction write.csv()
# La deuxième base de donnée comporte uniquement les expositions itinérantes partageant un même ID_E

glimpse(liste_expo)
## Réorganiser les données

## Recodage de liste_expo$lieu
liste_expo$lieu <- liste_expo$lieu %>%
  fct_recode(
    "Londres" = "London"
  )

# Si on travaille uniquement à partir de la première base de donnée, il faut : 
# Extraire les expositions itinérantes en enlevant les valeurs uniques.
# Les expositions itinérantes partagent un même ID_E
# liste_expo_itin <- liste_expo %>% group_by(ID_E) %>% filter(n()>1)

# Créer une base de donnée pour l'ensemble des noeuds
nodes_cities <- liste_expo %>%
  distinct(lieu, .keep_all = T) %>% select(lieu, lieu_expo, long, lat)

nodes_cities <- rowid_to_column(nodes_cities, "id")


# Créer une base de donnée pour l'ensemble des liens 
glimpse(liste_expo_itin)
liste_expo_itin$ID_E <- as.factor(liste_expo_itin$ID_E)
liste_expo_itin <- liste_expo_itin %>% select(ID_E,lieu)


links_cities <- liste_expo_itin %>% group_by(ID_E) %>%
  do(data.frame(t(combn(.$lieu, 2)), stringsAsFactors=FALSE))

links_cities$type <- "undirected"

col_order <- c("X1", "X2", "ID_E")
links_cities <- links_cities[,col_order]

links_cities <- rename(links_cities, lieu = X1, to = X2)


## Recodage de links_cities$to
links_cities$to <- links_cities$to %>%
  fct_recode(
    "Besançon" = "Besancon"
  )


## Calculer le nombre d'expositions dans chaque ville -------

nb_expos <- table(liste_expo$lieu, useNA = "ifany")
nb_expos <- as.data.frame(nb_expos)

colnames(nb_expos) <- c("lieu", "nb_expo")




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
coord_cities <- nodes_cities %>% select("long", "lat")
coordinates(coord_cities) <- ~ long + lat
proj4string(coord_cities) <- CRS("+proj=longlat +datum=WGS84")


# On effectue la transformation dans le nouveau système de coordonnées
projection <- spTransform(coord_cities, CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))

new_coord <- coordinates(projection)
new_coord <- as.data.frame(new_coord)
new_coord <- rowid_to_column(new_coord)

colnames(new_coord) <- c("id", "long_proj", "lat_proj")

new_coord


## Rajouter les nouvelles coordonnées aux bases de données ---------


# A la base des noeuds
nodes_cities <- merge(new_coord, nodes_cities, by = "id")

# A la base avec le nombre d'expositions
nb_expos <- merge(nb_expos, nodes_cities, by = "lieu")
nb_expos <- nb_expos %>% select(id, lieu, long_proj, lat_proj, nb_expo)

# A la base des liens
links_cities <- merge(links_cities, nodes_cities, by = "lieu")

links_cities <- rename(links_cities, from = lieu, lieu = to, id_from = id, orig_long_proj = long_proj, orig_lat_proj = lat_proj)

links_cities <- merge(links_cities, nodes_cities, by = "lieu")
links_cities <- rename(links_cities, from = from, id_from = id_from, to = lieu, id_to = id, dest_long_proj = long_proj, dest_lat_proj = lat_proj)

links_cities <- links_cities %>% select(ID_E, id_from, from, id_to, to, orig_long_proj, orig_lat_proj, dest_long_proj, dest_lat_proj)

## Recodage de links_cities$ID_E
links_cities$ID_E <- links_cities$ID_E %>%
  fct_recode(
    "La peinture française moderne. Moderne französische Malerei" = "ID_E_1",
    "Exposicion de pintura francesa. De Manet hasta nuestros días" = "ID_E_11",
    "Les origines de l'art contemporain. La peinture française de Manet à Bonnard" = "ID_E_2",
    "French Drawings. Masterpieces from Five Centuries" = "ID_E_20",
    "Van Gogh's grote tijdgenoten" = "ID_E_22",
    "French drawings. Masterpieces from Seven Centuries" = "ID_E_29",
    "Peinture française du XIXe siècle" = "ID_E_31"
  )

nb_expos$nb_expo <- as.numeric(nb_expos$nb_expo)

# Visualisation avec ggplot ---------

a <- ggplot(data = world) +
  geom_sf(fill = "grey20", color="black", size=.075) +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ", xlim = c(-4884806.3, 6536131.7), ylim = c(-3239788,9920031)) + 
  geom_curve(links_cities, mapping = aes(x = orig_long_proj, y = orig_lat_proj, xend = dest_long_proj, yend = dest_lat_proj, color = ID_E), size = .5, alpha = 0.7, curvature = .66) +
  geom_curve(links_cities, mapping = aes(x = 3972498.7, y = 3262086, xend = 4034572.4, yend = 3228895), color = "#00BFC4", size = .5, alpha = 0.7, curvature = -3) + # pour redessiner le lien entre Amsterdam et Otterlo qui était invisible
  geom_point(nb_expos, mapping = aes(long_proj,lat_proj, size = nb_expo), shape = 21, fill = "white", color = "black", stroke = .3) +
  scale_size_continuous(range = c(1,2.70), breaks = seq(1, 3, by = 1)) +
  ggtitle("Villes ayant organisé au moins une exposition avec la collection Matsukata (1946-1959)")  +
  labs(x = "Longitude", y = "Latitude", size = "Nombre d'expositions", color = "Réseau des villes ayant accueilli la même exposition itinérante :", caption = "Sources : Léa Saint-Raymond (2019)") +
  theme_void() 

a

#Pour sauvegarder la visualisation
#ggsave("carte_expos_villes.pdf",width=15,height=8) 


##### Sources: 

### Surles représentations graphiques avec r avec ggplot : 
# https://bookdown.org/content/b298e479-b1ab-49fa-b83d-a57c2b034d49/
# https://ggplot2-book.org/index.html
# https://r-graph-gallery.com/index.html

### Sur la cartographie avec R:
# https://r-spatial.org/r/2018/10/25/ggplot2-sf.html
# https://geocompr.robinlovelace.net/index.html