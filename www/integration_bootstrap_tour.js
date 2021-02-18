$(document).on("click", ".send_to_top", function() {
        $('html, body').animate({ scrollTop: 0 }, 'fast');
});


Shiny.addCustomMessageHandler("scrollToBottom",
  function(message) {
    window.scrollTo(0,document.body.scrollHeight);
  }
);


Shiny.addCustomMessageHandler("startTutoParams",

  function(message) {
    // Instantiate the tour
    var tour = new Tour({
      backdrop:true,
      steps: [
      {
        element: "#ui_choix_reg",
        title: "Choix de la région 1/2",
        content: "Il est possible d'utiliser le menu déroulant pour sélectionner votre région...",
        placement:"left"
      },
      {
        element: "#choixRegMap",
        title: "Choix de la région 2/2",
        content: "Sur la carte il suffit de cliquer sur le polygone correspondant à votre région pour la sélectionner.",
        placement:"left"
      },
      {
        element: "#row_choix_ps",
        title: "Choix de la profession de santé",
        content: "Choisissez la profession de santé dont vous souhaitez indiquer le zonage.",
        placement:"left"
      },
      {
        element: "#load_existing_proj .box",
        title: "Charger un projet existant",
        content: "Si vous avez déjà utilisé l'application, vous pourrez choisir de continuer un projet sauvegardé.",
        placement:"left"
      },
      {
        element: "#create_new_proj .box",
        title: "Démarrer un nouveau projet",
        content: "Ici vous pourrez créer un nouveau projet, il suffit de lui donner un nom et c'est parti.",
        placement:"left"
      },
      {
        element: "#file_import_box .box",
        title: "Importer votre fichier zonage",
        content: "Si vous avez déjà saisi votre zonage dans un fichier Excel ou CSV, vous pouvez l'importer directement dans l'application. Attention il doit être présenté selon l'un des deux modèles indiqués ici.",
        placement:"left"
      },
      {
        element: "#box_go_zonage .box",
        title: "Accéder à l'outil de saisie interactive du zonage",
        content: "Une fois précisés la région, la profession de santé et le projet à ouvrir, un bouton apparaîtra ici pour accéder au menu suivant. Attention une clef d'identification vous sera demandée. Elle a été transmise par email. Si vous l'avez perdu contactez-nous !",
        placement:"left"
      },
      {
        element: "li.treeview:nth-child(6) > a:nth-child(1)",
        title: "Nous contacter",
        content: "N'hésitez pas à nous contacter pour nous dire ce que vous pensez de l'application et nous faire part de vos besoins et difficultés.",
        placement:"right"
      }
    ],
        template: function () {
        return ("<div class='popover tour'>"+
                  "<div class='arrow'></div>"+
                    "<h3 class='popover-title'></h3>"+
                    "<div class='popover-content'></div>"+
                    "<div class='popover-navigation'>"+
                    "<button class='btn btn-default' data-role='prev'>Précédent</button>"+
                    "<button class='btn btn-default' data-role='next'>Suivant</button>"+
                    "<button class='btn btn-default send_to_top' data-role='end'>Fermer</button>"+
                    "</div>"+
                "</div>"

        );
    }

    });

    // Initialize the tour
    tour.init();

    // Start the tour
    tour.start();

    tour.restart();
  }
);




Shiny.addCustomMessageHandler("startTutoZonage",

  function(message) {
    // Instantiate the tour
    var tour = new Tour({
      backdrop:true,
      steps: [
      {
        element: "#box_tableau #zonage_dt",
        title: "Tableau de saisie du zonage",
        content: "Ce tableau est l'élément central de l'application. Il vous permet de saisir le zonage pour chaque TVS/BVCV en partant des valeurs assignées par le cadre national qui s'appuie sur l'APL.",
        placement:"left"
      },
      {
        element: "#box_carte_jauges #communes_map",
        title: "Carte du zonage",
        content: "Cette carte est mise à jour automatiquement dès que vous modifiez un zonage dans le tableau situé juste à gauche. En cliquant sur un TVS/BVCV sur la carte, le tableau sera filtré pour accéder directement à ce TVS/BVCV. Pour supprimer le filtre, cliquer sur 'vider' en haut à droite du tableau",
        placement:"left"
      },
      {
        element: "#box_tableau .dataTables_filter",
        title: "Filtrage du tableau de zonage",
        content: "Cette barre de recherche vous permet de rechercher un TVS/BVCV avec son code ou son nom ou encore de filtrer sur les noms de communes. Le bouton 'vider' permet de supprimer le filtre.",
        placement:"left"
      },
      {
        element: "#box_carte_jauges #gauges",
        title: "Jauges de population",
        content: "Les jauges sont mises à jour automatiquement pour vous aider à respecter les contraintes liées au zonage fixées par la DGOS.",
        placement:"left"
      },
      {
        element: "#box_carte_jauges #dist_zonages",
        title: "Distribution de la population par zone",
        content: "Ce graphique permet d'évaluer la part de population de chaque zone.",
        placement:"left"
      },
      {
        element: "#box_tableau .bootstrap-switch-id-toggle_qpv",
        title: "Zonage des QPV",
        content: "Il est possible d'ajouter sur la carte la liste des Quartiers Prioritaires de la Politique de la Ville. Une bouton apparaîtra en-dessous de celui-ci permettant de rechercher un QPV pour modifier son zonage. Il est également possible de sélectionner le QPV en cliquant dessus sur la carte !",
        placement:"left"
      },
      {
        element: "#box_tableau #force_save",
        title: "Sauvegarde du tableau",
        content: "Des sauvegardes automatiques sont réalisées toutes les 20 secondes mais vous pouvez forcer la sauvegarde en cliquant ici.",
        placement:"left"
      },
      {
        element: "#box_carte_jauges #update_contours",
        title: "Mettre à jour les fonds de carte",
        content: "La date indiquée à gauche vous permet de savoir si les fonds de carte ont été mis à jour récemment. Si la mise à jour remonte à plus de 3 mois il peut être utile de mettre à jour les fonds géographiques en cliquant ici. Les données utilisées sont celles de l'API géo d'Etalab : https://geo.api.gouv.fr/",
        placement:"left"
      },
      {
        element: "#box_tableau #open_form_justification",
        title: "Justification du choix du zonage",
        content: "Une fois le zonage saisi, vous pouvez ajouter des éléments d'explication en cliquant ici.",
        placement:"left"
      },
      {
        element: "#box_tableau #recap_dt",
        title: "Tableu récapitulatif des modifications",
        content: "Ce tableau recense l'ensemble des modifications qui ont été effectuées pour ce projet par rapport aux valeurs par défaut correspondant au cadre national.",
        placement:"left"
      },
      {
        element: "#box_carte_jauges #download_plot",
        title: "Téléchargement de la carte",
        content: "Il est possible de récupérer la carte du zonage au format png.",
        placement:"left"
      },
      {
        element: "#box_carte_jauges #download_table",
        title: "Téléchargement du tableau",
        content: "Il est possible de récupérer le tableau du zonage au format Excel.",
        placement:"left"
      },
      {
        element: "#box_carte_jauges #generate_arrete",
        title: "Génération de l'arrêté",
        content: "En cliquant ici, un formulaire apparaîtra pour vous aider à générer un modèle d'arrêté contenant le tableau et la carte du zonage en annexes.",
        placement:"left"
      },
      {
        element: "#box_tableau #save_envigueur",
        title: "Confirmation d'un zonage en vigueur",
        content: "Une fois un arrêté pris, vous pouvez exporter le zonage associé afin que les autres ARS en tiennent compte et que les données soient partagées avec nos partenaires : CNAM, ReZone, AtlaSante...",
        placement:"left"
      },
      {
        element: "li.treeview:nth-child(5) > a:nth-child(1)",
        title: "Télécharger les données",
        content: "Tous les données utilisées dans l'application sont téléchargeables ici.",
        placement:"right"
      },
      {
        element: "li.treeview:nth-child(6) > a:nth-child(1)",
        title: "Nous contacter",
        content: "N'hésitez pas à nous contacter pour nous dire ce que vous pensez de l'application et nous faire part de vos besoins et difficultés.",
        placement:"right"
      }
    ],
        template: function () {
        return ("<div class='popover tour'>"+
                  "<div class='arrow'></div>"+
                    "<h3 class='popover-title'></h3>"+
                    "<div class='popover-content'></div>"+
                    "<div class='popover-navigation'>"+
                    "<button class='btn btn-default' data-role='prev'>Précédent</button>"+
                    "<button class='btn btn-default' data-role='next'>Suivant</button>"+
                    "<button class='btn btn-default send_to_top' data-role='end'>Fermer</button>"+
                    "</div>"+
                "</div>"

        );
    }

    });

    // Initialize the tour
    tour.init();

    // Start the tour
    tour.start();

    tour.restart();
  }
);

