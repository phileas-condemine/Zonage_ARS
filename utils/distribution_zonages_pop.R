# distribution_zonages_pop = function(input,output,session){

  
  output$dist_zonages = renderPlotly({
    req(!is.null(vals_reac()))
    infos=merge(tableau_reg()[,c("agr","population","is_majoritaire")],
                vals_reac(),by="agr",all.x=T)
    
    infos[,is_majoritaire:=ifelse(is_majoritaire,"majoritaire","minoritaire")]
    if(input$choix_ps%in%c("sf","inf")){
      infos[,picked_zonage:=case_when(
        picked_zonage=="VUD"~"Très sous-doté",
        picked_zonage=="UD"~"Sous-doté",
        picked_zonage=="Int"~"Intermédiaire",
        picked_zonage=="VD"~"Très doté",
        picked_zonage=="OD"~"Sur-doté")]
    }
    infos[,zonage_majoritaire:=paste0(picked_zonage," - ",is_majoritaire)]
    zonage_pop=data.table(infos)[
      ,list(pop=sum(population,na.rm=T)),
      by="zonage_majoritaire"]
    print("zones x majoritaire sur piechart")
    print(unique(zonage_pop$zonage_majoritaire))
    
    if(input$choix_ps=="mg"){
      zonage_pop[,zonage_majoritaire:=factor(zonage_majoritaire,levels=c("NA - majoritaire","NA - minoritaire",
                                                                         "HV - majoritaire","HV - minoritaire",
                                                                         "ZV - majoritaire", "ZV - minoritaire",
                                                                         "ZAC - majoritaire","ZAC - minoritaire",
                                                                         "ZIP - majoritaire","ZIP - minoritaire"
      ))]
      my_colors = data.table(zonage_majoritaire = c("NA - majoritaire","NA - minoritaire",
                                                    "HV - majoritaire","HV - minoritaire",
                                                    "ZV - majoritaire", "ZV - minoritaire",
                                                    "ZAC - majoritaire","ZAC - minoritaire",
                                                    "ZIP - majoritaire","ZIP - minoritaire"
      ),
      color = c('#A6CEE3','#95BDD2',
                '#1F78B4','#0E67A3',
                '#33A02C','#228F1B',
                '#FB9A99','#EA8988',
                '#E31A1C','#D2090B'))
    } else if (input$choix_ps%in%c("sf","inf")){
      zonage_pop[,zonage_majoritaire:=factor(zonage_majoritaire,levels=c("NA - minoritaire",
                                                                         "NA - majoritaire",
                                                                         "Très sous-doté - majoritaire",
                                                                         "Sous-doté - majoritaire",
                                                                         "Intermédiaire - majoritaire",
                                                                         "Très doté - majoritaire",
                                                                         "Sur-doté - majoritaire"))]
      my_colors = data.table(zonage_majoritaire = c("NA - minoritaire","NA - majoritaire",
                                                    "Très sous-doté - majoritaire",
                                                    "Sous-doté - majoritaire",
                                                    "Intermédiaire - majoritaire",
                                                    "Très doté - majoritaire",
                                                    "Sur-doté - majoritaire"),
                             color = c('#A6CEE3','#95BDD2',
                                       '#1F78B4',
                                       '#B2DF8a',
                                       '#33A02C',
                                       '#FB9A99',
                                       '#E31A1C'))
    }
    print("actual levels")
    print(levels(zonage_pop$zonage_majoritaire))
    zonage_pop[,zonage_majoritaire:=droplevels(zonage_majoritaire)]
    zonage_pop = merge(zonage_pop,my_colors,by="zonage_majoritaire")
    tot_pop = sum(zonage_pop$pop)
    plot_ly(zonage_pop, labels = ~zonage_majoritaire, values = ~pop,hoverinfo = 'text',textinfo='label',
            text = ~paste0(zonage_majoritaire,' : ',round(100*pop/tot_pop,1),'% - soit ',pop,ifelse(input$choix_ps=="sf"," femmes"," habitants")), 
            marker = list(colors = zonage_pop$color),
            type = 'pie')%>%hide_legend()%>%
      layout(title = 'Distribution par zone de la population de la région',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
# }
