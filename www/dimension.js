var dimension = 0;
$(document).on("shiny:connected", function(e) {
    dimension = window.innerWidth;
    // dimension[1] = window.innerHeight;
    Shiny.onInputChange("dimension", dimension);
});
$(window).resize(function(e) {
    dimension = window.innerWidth;
    // dimension[1] = window.innerHeight;
    Shiny.onInputChange("dimension", dimension);
});