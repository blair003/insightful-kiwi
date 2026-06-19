# project.R — project-wide ecological knowledge (USER-EDITED).
#
# Unlike datasets.R (per-dataset import config), this is project-level: which
# species matter and how, applied across all datasets. Loaded at build time and
# materialised into ik_data$app. Changing it re-imports (it's in the cache key).

# Species roles — group species into ecological roles for co-occurrence analysis
# (e.g. predator stalking protected). Within a role the ORDER is priority/interest.
# Each subgroup is a set of matchers; a taxon belongs to it if ANY matcher hits:
#   scientificName  exact name(s)
#   family          taxonomic family (from app$taxonomy)
#   genus           first word of the scientificName (robust to missing family,
#                   e.g. the ferret Mustela putorius furo has family = NA)
species_roles <- list(
  predator = list(
    mustelid = list(genus = "Mustela", family = "Mustelidae"),
    rat      = list(genus = "Rattus"),
    cat      = list(genus = "Felis"),
    dog      = list(family = "Canidae")
  ),
  protected = list(
    kiwi = list(genus = "Apteryx")
  )
)

# Same-species duplicate window — two observations of the same species at the same
# location within this many MINUTES are flagged a possible duplicate. `default`
# applies to all species; `by_species` overrides per scientificName. (Only applies
# to minute-resolution data; date-only sources are excluded.)
duplicate_window <- list(
  default    = 30,
  by_species = list()   # e.g. "Rattus" = 60
)
