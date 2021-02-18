
jour_nommois_annee=function(d){
  message("func : jour_nommois_annee")
  paste(day(d),mois_noms[month(d)],year(d))
}