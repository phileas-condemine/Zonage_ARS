# Zonage médical des territoires de vie-santé

[https://drees.shinyapps.io/Zonage_ARS/](https://drees.shinyapps.io/Zonage_ARS/)

## Contexte

Cet outil développé avec R Shiny devrait permettre d'uniformiser la saisie du zonage médical des territoires de vie-santé (TVS) par les Agences Régionales de Santé (ARS).

En plus de la vocation d'uniformisation, cet outil se veut ergonomique avec une interaction entre le tableau de saisie et une carte ainsi que la possibilité de rappeler à l'usager que certains TVS ne devraient pas être édités par la région si le TVS dépend d'une autre région (code département relatif à un département d'une autre région).

### Premier aperçu de l'application à l'ouverture 

![Ouverture de l'application](www/usage_debut.gif)

- Choisissez votre région en cliquant sur la zone de la carte ou en sélectionnant dans le menu déroulant situé au dessu. Les DROM ont été rapprochés de la métropole pour des raisons ergononiques, d'où l'absence d'un fond de carte esthétique.
- Choisissez les zones à affecter aux TVS
- Si un TVS est en sélection nationale ou qu'il ne dépend pas de la région, un sigle "interdiction" apparaîtra. Pour le faire disparaître, cliquer ailleurs sur la ligne et sélectionner "forcer l'édition".
- Pour sélectionner un TVS à partir de la carte, cliquer dessus.
- Les données sont régulièrement (toutes les 20 secondes) synchronisées automatiquement, cette étape peut générer un ralentissement pendant 2-3 secondes.

### Pour récupérer le tableau des communes avec le zonage choisi

![Ouverture de l'application](www/get_excel.gif)


### Pour récupérer la carte statique représentant ce zonage

![Ouverture de l'application](www/get_carte.gif)

### Pour récupérer une structure d'arrêté à éditer

![Ouverture de l'application](www/get_arrêté.gif)

## Pour rapporter des erreurs, bugs, suggestions d'améliorations

Remplir un [issue](https://gitlab.com/DREES_code/formulaire_zonage_ars/issues) sur Gitlab.

## Références


Cette documentation n'aurait pas été possible sans l'outil formidable ScreenToGif https://www.screentogif.com/


La persistence des données s'appuie sur dropbox.com et Google Sheets.


L'application est hébergée grâce au service shinyapps.io


De nombreux packages R ont été nécessaires à ce développement : 

- shiny
- data.table
- DT
- leaflet
- ggplot2
- ggrepel
- sp
- sf
- shinyalert
- dplyr
- readxl
- flexdashboard
- jsonlite
- curl
- googlesheets
- colourvalues
- bsplus
- rdrop2
- rgdal
- rgeos
- htmlwidgets
- lubridate
- openxlsx
- knitr

