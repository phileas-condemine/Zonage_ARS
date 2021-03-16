

$(document).on('click',".dataTables_filter input[type='reset']", function() {
        $(this).
        prev('input[type="search"]').
        val('').keyup();
    });
    
    
$(document).on('click', '.zonage_radio_button', function () {
        Shiny.setInputValue('last_btn',this.name + '_' + this.value);
                         });

$(document).on('mouseover', 'tr.shiny-input-radiogroup', function () {
        Shiny.setInputValue('last_row_hovered',this.id);
                         });
                         
/*
$(document).on('click', '#communes_map .legend.info i', function () {
        Shiny.setInputValue('legend_click',$(this).attr("style"));
                         });
*/                         

                                          
/*$(window).on('scroll',function() {
  var topOffset = $(this).scrollTop();
  const seuil = 10;
  if(topOffset >= seuil) {
    $('#rerun_contours').fadeIn();} else {
    $('#rerun_contours').fadeOut();}
});


$(window).resize(function() {
  var width = $(this).width();
  const seuil = 1024;
  if(width >= seuil) {
    $('#gauge_and_map').fadeIn();} else {
    $('#gauge_and_map').fadeOut();}
});*/

$(document).on('click','.leaflet-top .legend i',function(){
          Shiny.setInputValue('clicked_legend',$(this).text());
});


function checkifrunning() {
      var is_running = $("html").attr("class").includes("shiny-busy");
      if (is_running){
        $(".loading_spinner").show()
        $("#save_envigueur").hide()
      } else {
        $(".loading_spinner").hide()
        $("#save_envigueur").show()

      }
    }; 
setInterval(checkifrunning, 500)
