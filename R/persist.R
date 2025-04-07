#' Functions to use Local Storage in javascript
#'
persist_text <- function(id) {
  glue::glue(r"[<script type="text/javascript">
      //save entered info
      document.addEventListener('DOMContentLoaded', function() {
      console.log('Adding event listener for [id].');
      const form = document.getElementById('[id]');
      form.addEventListener("input", function () {
        var todo = document.getElementById('[id]').value;
        localStorage.setItem("[id]-contents", todo);
        console.log("Saving value of [id].")
      }, false);

      //on page load, read in the old value
      const savedValue = localStorage.getItem("[id]-contents");
      if (savedValue) {
        document.getElementById('[id]').value = savedValue;
      }
})
</script>]",
  .open = "[", .close = "]", .literal = TRUE)
}

#'
persist_radio <- function(id) {
  glue::glue(r"(<script type="text/javascript">
document.addEventListener('DOMContentLoaded', function() {{
      console.log('Adding event listener for {id}.');
      const form = document.getElementById('{id}-form');
      const radioButtons = form.querySelectorAll('input[type="radio"]');

      // Load saved value on page load
      const savedValue = localStorage.getItem('{id}-choice');
      if (savedValue) {{
        console.log("Restoring radio button values for {id}.");
        radioButtons.forEach(radio => {{
          if (radio.value === savedValue) {{
            radio.checked = true;
          }}
        }});
      }}

      // Save value on change
      form.addEventListener('change', function(event) {{
        if (event.target.type === 'radio') {{
          console.log("Saving changed value for {id}.");
          localStorage.setItem('{id}-choice', event.target.value);
        }}
      }});
    }});
</script>)",
  .literal = TRUE)
}

#' @export
clear_persistent_storage <- function() {
  script <- r"[<script type="text/javascript">
      // clear the local storage
    document.addEventListener('DOMContentLoaded', function() {
      const form = document.getElementById('btnClearStorage');
      form.addEventListener("click", function () {
        localStorage.clear();
        location.reload();
        console.log("Clearing memory");
      }, false);
    }
    )
    </script>]" |> htmltools::HTML()
  tagList(
    script,
    tags$button(
      "Clear memory",
      id = "btnClearStorage"
    )
  ) |> as.character()
}
