# bootstrap.R
#
# Startup helpers that run before config and data are loaded. Keep this lean —
# only what is needed to get the app off the ground. Feature helpers belong in
# their own concern-named files, not here.

#' Assert that R packages are installed (available to load).
#'
#' Fails fast at startup if any are missing, instead of installing at runtime —
#' the environment (dev container / renv) is where dependencies are added, and new
#' packages are added deliberately (see AGENTS.md). All missing packages are
#' reported at once.
#'
#' @param packages Character vector of package names.
#' @param context  Where the packages are expected to live (used in the error).
#' @return Invisibly, the asserted package names.
assert_packages_available <- function(packages, context = "the R environment") {
  packages <- unique(packages[!is.na(packages) & nzchar(packages)])
  if (length(packages) == 0) {
    return(invisible(character(0)))
  }

  installed <- vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  if (!all(installed)) {
    missing <- packages[!installed]
    stop(
      sprintf(
        "Missing R package%s in %s: %s. Install %s, then restart.",
        if (length(missing) > 1L) "s" else "",
        context,
        paste(missing, collapse = ", "),
        if (length(missing) > 1L) "them" else "it"
      ),
      call. = FALSE
    )
  }

  invisible(packages)
}

#' Assert availability, then attach packages.
#'
#' Convenience for the common "check, then `library()`" startup pattern: reports
#' every missing package up front rather than failing on the first `library()`.
#'
#' @param packages Character vector of package names to attach.
#' @inheritParams assert_packages_available
#' @return Invisibly, the attached package names.
attach_packages <- function(packages, context = "the R environment") {
  assert_packages_available(packages, context)
  for (pkg in packages) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
  invisible(packages)
}
