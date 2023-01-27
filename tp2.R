library(sf)
library(dplyr)
setwd("~/work/tp2-3/fonds")

comm_metro <- st_read('commune_francemetro_2021.shp', options = "ENCODING=WINDOWS-1252")
summary(comm_metro)
View(comm_metro)

syst_proj <- st_crs(comm_metro)

comm_bretonne <- filter(comm_metro, reg == "53")
comm_bretonne <- comm_bretonne %>% select('dep', 'code','libelle', 'epc', 'surf')

head(comm_bretonne)

plot(comm_bretonne, lwd = 0.1)

plot(st_geometry(comm_bretonne), lwd = 0.3)

comm_bretonne <- comm_bretonne %>% mutate(surf2=st_area(geometry))
str(comm_bretonne$surf2)

comm_bretonne <- comm_bretonne %>% mutate(surf2 = units::set_units(surf2, km*km))
View(comm_bretonne)

#Q11
#Pas les mêmes valeurs parce que calcul numérique pour surf2 alors que c'est une valeur de base pour surf

#Q12
dept_bretagne <- comm_bretonne %>% 
            group_by (dep) %>% 
            summarise (surf_dept = sum(surf))
View(dept_bretagne)

plot(st_geometry(dept_bretagne), lwd = 0.3)

#Q13
dept_bretagne2 <- comm_bretonne %>% 
            group_by (dep) %>% 
            summarise (geometry = st_union(geometry))
View(dept_bretagne2)

plot(st_geometry(dept_bretagne2), lwd = 0.5, axes = TRUE)

#Q14
centroide_dept_bretagne <- dept_bretagne %>% 
            mutate (centroide = st_centroid(geometry))

plot(st_geometry(centroide_dept_bretagne))
plot(centroide_dept_bretagne$centroide, add = TRUE, pch = 16, col = 'red')
nom_departement <- c("Cotes d'Armor", "Finistère", "Ille et Vilaine", "Morbihan")

centroide_dept_bretagne <- centroide_dept_bretagne %>% 
            mutate (nom_dept = nom_departement)%>% 
            mutate(coord = st_coordinates(centroide)) %>%
            st_drop_geometry(centroide_dept_bretagne)
#Pour ajouter un label à une place donnée
text(x = centroide_dept_bretagne$coord, labels = centroide_dept_bretagne$nom_dept, pos = 1, col = 'red')

#Q15
res <- st_intersects(comm_bretonne, dept_bretagne)
summary(res)
which(lengths(res)>1)
centroide_dept_bretagne2 <- centroide_dept_bretagne %>% select(dep, coord)
res2 <- st_intersects(comm_bretonne, centroide_dept_bretagne2)
