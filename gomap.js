// When locator icon in datatable is clicked, go to that spot on the map
$(document).on("click", ".go-map", function(e) {
  e.preventDefault();
  $el = $(this);
  var year = $el.data("yearSlider");
  $($("#nav a")[0]).tab("show");
  Shiny.onInputChange("goto", {
    year: lat
  });
});