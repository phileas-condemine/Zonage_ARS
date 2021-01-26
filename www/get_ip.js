$(document).ready(function(){
  $.get("https://api64.ipify.org/", function(response) {
    Shiny.onInputChange("getIP", response);
  }, "json");
});