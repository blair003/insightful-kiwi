# camtrapdp_template.R — a clean Camtrap DP package skeleton for converters to
# fill, instead of inheriting example_dataset()'s (foreign) metadata. Keeps the
# standard resource schemas + profile; clears example-specific metadata. spatial,
# temporal and taxonomic are auto-derived by the camtrapdp setters
# (deployments<- -> update_spatial/update_temporal; observations<- -> update_taxonomic),
# so the converter need only supply identity + contributors + the taxonomy values.

#' A blank Camtrap DP template: valid structure, no example metadata.
#'
#' @param title        Package title.
#' @param id           Package id/name slug.
#' @param description  Package description.
#' @param contributors List of contributor entries, e.g. list(list(title=, role=)).
#' @return A camtrapdp object with the example data tables still attached (the
#'   converter replaces them via the setters) and cleaned metadata.
ik_camtrapdp_template <- function(title, id, description, contributors = list()) {
  x <- camtrapdp::example_dataset()

  x$name         <- id
  x$id           <- id
  x$title        <- title
  x$description  <- description
  x$created      <- format(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  x$version      <- "1.0"
  x$contributors <- contributors
  x$licenses     <- list(list(name = "CC0-1.0", scope = "data"))

  # Drop example-specific metadata we don't have; spatial/temporal/taxonomic are
  # set from our data by the converter.
  for (f in c("keywords", "sources", "bibliographicCitation", "project", "image",
              "homepage", "relatedIdentifiers", "references",
              "spatial", "temporal", "taxonomic")) {
    x[[f]] <- NULL
  }
  x
}
