# config.R — runtime configuration: an INPUT that builds ik_data, kept as a
# separate global (paths, toggles, ...). Lean for now: the instance directory
# layout plus the strict-import toggle. Timezone, API keys, etc. are added when a
# feature needs them. Never hardcode these paths elsewhere — read config$env$dirs.

#' Build the runtime config.
#'
#' @param instance Path to the active instance directory (holds config/, extdata/,
#'   cache/, logs/), relative to the app working directory.
#' @return A config list. `$env$dirs` are the resolved directory paths;
#'   `$strict_datasets` promotes unregistered-package warnings to errors.
build_config <- function(instance = "instance") {
  list(
    env = list(
      dirs = list(
        instance = instance,
        config   = file.path(instance, "config"),
        extdata  = file.path(instance, "extdata"),
        packages = file.path(instance, "extdata", "packages"),
        raw      = file.path(instance, "extdata", "raw"),
        cache    = file.path(instance, "cache"),
        logs     = file.path(instance, "logs")
      )
    ),
    strict_datasets = FALSE
  )
}
