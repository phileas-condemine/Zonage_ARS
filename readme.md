# Zonage médical des territoires de vie-santé

[https://drees.shinyapps.io/Zonage_ARS/](https://drees.shinyapps.io/Zonage_ARS/)

## Contexte

Cet outil développé avec R Shiny devrait permettre d'uniformiser la saisie du zonage médical et paramédical (sages-femmes & infirmiers) des territoires de vie-santé (TVS) et des Bassins de Vie Canton Ville (BVCV) par les Agences Régionales de Santé (ARS).

En plus de la vocation d'uniformisation, cet outil se veut ergonomique avec une interaction entre le tableau de saisie et une carte ainsi que la possibilité de rappeler à l'usager que certains TVS/BVCV ne devraient pas être édités par la région si le TVS/BVCV dépend d'une autre région (population de la zone principalement située dans l'autre région).

### Premier aperçu de l'application à l'ouverture 

![Ouverture de l'application](www/usage_debut.gif)

- Choisissez votre région à l'aide du menu déroulant.
- Choisissez la profession dont vous souhaitez modifier le zonage : médecins généralistes, infirmiers ou sage-femmes.
- Si un TVS/BVCV est en sélection nationale ou qu'il ne dépend pas de la région, un sigle "interdiction" apparaîtra. Pour le faire disparaître, cliquer ailleurs sur la ligne et sélectionner "forcer l'édition".
- Pour sélectionner un TVS à partir de la carte, cliquer dessus. Le filtre peut ensuite être supprimé en cliquant sur "Vider" au dessus du tableau à gauche de la barre de recherche.
- Les modifications sont enregistrées automatiquement à intervalles réguliers mais vous pouvez aussi utiliser le bouton "Sauvegarder" situé en bas à gauche du tableau.

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


La persistance des données s'appuie sur dropbox.com et ~~Google Sheets~~ (API beaucoup trop lente).


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
- colourvalues
- bsplus
- rdrop2
- rgdal
- rgeos
- htmlwidgets
- lubridate
- openxlsx
- knitr
- kableExtra
- flextable
- ggsn
- shinydashboard
- shinyWidgets
- plotly
- slackr
- shinyjs

