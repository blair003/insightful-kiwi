import re

file_path = "modules/mapping_module.R"
with open(file_path, "r") as f:
    content = f.read()

replacement = '''      popup = ~{
        popup_content <- paste(locationName, "<br>Count:", count)
        # Avoid jsonlite here because it evaluates on vectors and doesn't map 1:1, plus quotes issues.
        # Manually format JS string avoiding double quotes so it fits in the HTML attribute.
        onclick_js <- sprintf("Shiny.setInputValue('density_map_review_sequences_click', {location_name: '%s', locality: '%s'}, {priority: 'event'}); return false;", locationName, locality)
        review_link <- sprintf("<br><a href='#' onclick=\\"%s\\" title='Review Sequences'>Review Sequences</a>", onclick_js)
        paste0(popup_content, review_link)
      }'''

content = re.sub(
    r'''      popup = ~{\n        popup_content <- paste\(locationName, "<br>Count:", count\)\n        action_data <- list\(\n          location_name = locationName,\n          locality = locality\n        \)\n        json_data <- jsonlite::toJSON\(action_data, auto_unbox = TRUE\)\n        onclick_js <- sprintf\("Shiny\.setInputValue\('density_map_review_sequences_click', %s, \{priority: 'event'\}\); return false;", json_data\)\n        review_link <- sprintf\("<br><a href='#' onclick=\\"%s\\" title='Review Sequences'>Review Sequences</a>", onclick_js\)\n        paste0\(popup_content, review_link\)\n      }''',
    replacement,
    content
)

with open(file_path, "w") as f:
    f.write(content)
