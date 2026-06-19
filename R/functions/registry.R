# registry.R — one home for the app's extension registries (converters, geography
# derivers, ...). These map a string key to a FUNCTION, so they are CODE, not data:
# they belong here, never inside ik_data (which is serialised to the .RDS cache and
# is built USING these). An environment is used so each kind's file can register
# into the shared object by reference as the app is sourced.

ik_registry <- new.env(parent = emptyenv())

#' Register a value (usually a function) under a kind + name.
#'
#' @param kind  Registry kind, e.g. "converter", "geography_deriver".
#' @param name  Key within the kind.
#' @param value The function/value to register.
#' @return Invisibly, `value`.
ik_register <- function(kind, name, value) {
  reg <- get0(kind, envir = ik_registry, ifnotfound = list())
  reg[[name]] <- value
  assign(kind, reg, envir = ik_registry)
  invisible(value)
}

#' Look up a registered value, or NULL if absent.
#'
#' @param kind Registry kind.
#' @param name Key within the kind.
#' @return The registered value, or NULL.
ik_registered <- function(kind, name) {
  get0(kind, envir = ik_registry, ifnotfound = list())[[name]]
}
