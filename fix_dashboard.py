import re

file_path = "includes/dashboard_functions.R"
with open(file_path, "r") as f:
    content = f.read()

replacement = '''                  # Manually construct JS object to avoid double quotes in the HTML attribute
                  onclick_js <- sprintf("Shiny.setInputValue('review_sequences_click', {period_name: '%s', rai_group: '%s', locality: '%s'}, {priority: 'event'}); return false;",
                    period_metric$period, metric$rai_group, metric$locality_filter)
                  HTML(sprintf('<a href="#" onclick="%s" title="Review Sequences">%s</a>',
                    onclick_js, val))'''

content = re.sub(
    r'''          action_data <- list\(\n            period_name = period_metric\$period,\n            rai_group = metric\$rai_group,\n            locality = metric\$locality_filter\n          \)\n          json_data <- jsonlite::toJSON\(action_data, auto_unbox = TRUE\)\n          onclick_js <- sprintf\("Shiny\.setInputValue\('review_sequences_click', %s, \{priority: 'event'\}\); return false;", json_data\)\n          HTML\(sprintf\('<a href="#" onclick="%s" title="Review Sequences">%s</a>', onclick_js, val\)\)''',
    replacement,
    content
)

with open(file_path, "w") as f:
    f.write(content)
