table.rows().every(function(i, tab, row) {
                  var $this = $(this.node());
                  $this.attr('id', this.data()[0]);
                  $this.addClass('shiny-input-radiogroup');
                });
                Shiny.unbindAll(table.table().node());
                /*setTimeout(function(){*/
                  Shiny.bindAll(table.table().node());
                /*},0);*/
$("input[type='search']").attr('placeholder','Par code ou par nom').after('<input type="reset" value="Vider">');


$("span:contains('Column visibility')").text('Colonnes à afficher');
