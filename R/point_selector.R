#' Build a widget for setting scores
#'
#' @export
point_selector <- function(id = "foo") {
  glue::glue('<span id="{id}" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline" role="radiogroup" aria-labelledby="radio_year_select-label" style="font-size: 7;">
  <span class="shiny-options-group">
    <label class="radio-inline">
      <input type="radio" name="{id}" value="0" checked="checked"/>
      <span>0  </span>
    </label>
    <label class="radio-inline">
      <input type="radio" name="{id}" value="1"/>
      <span>1  </span>
    </label>
    <label class="radio-inline">
      <input type="radio" name="{id}" value="2"/>
      <span>2  </span>
    </label>
    <label class="radio-inline">
      <input type="radio" name="{id}" value="3"/>
      <span>3</span>
    </label>
  </span>
</span>') |> shiny::HTML()
}
