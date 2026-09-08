#' List Packages Suggested by the `foosball3` Package
#' 
#' These are all packages required for the graphical user interface
#' ([foosball3()]).
#' Make sure that they are installed
#' @param ... Ignored
#' @returns Returns a vector of package names that are suggested by
#' `foosball3` and required by its graphical user interface.
#' @examples
#' foosball3_list_suggests()
#' @export
foosball3_list_suggests <- function(...) {
  utils::packageDescription("foosball3")$Suggests |>
    strsplit(",\n", perl = TRUE) |> unlist()
}

.foosball_suggest <- function(ignore, ...) {
  suggests <- foosball3_list_suggests(...)
  suggests <- suggests[!(suggests %in% ignore)]
  state <- lapply(suggests, requireNamespace) |> unlist()
  suggests[!state]
}

#' Check if All Suggested Packages Are Available
#' 
#' This function returns `TRUE` if all packages required for the
#' graphical user interface (GUI) ([foosball3()]) are available.
#' @param ... Arguments passed to [foosball3_list_suggests()]
#' @param ignore A vector of packages to ignore. By default
#' `"mirai"` is ignored, as this package is not needed for basic
#' functionality. It is recommended to install for better performance.
#' @returns A `logical` value, indicating of all required suggests are
#' available.
#' @examples
#' foosball3_list_suggests()
#' @export
foosball3_suggests_ok <- function(ignore = "mirai", ...) {
  length(.foosball_suggest(ignore, ...) > 0)
}

#' Install Packages Suggested by `foosball3`
#' 
#' Call this function if [foosball3_suggests_ok()] returns `FALSE`.
#' This will install all suggested packages required to run the graphical
#' user interface ([foosball3()]).
#' @inherit foosball3_suggests_ok
#' @param ... Arguments passed to [utils::install.packages()]
#' @param update A `logical` value. If `TRUE`, it will try to update
#' suggested packages that are already installed. If `FALSE`, it
#' will only install suggested packages that are not yet available.
#' @returns invisible `NULL`
#' @export
foosball3_install_suggests <- function(ignore = "mirai", update = TRUE, ...) {
  suggests <- if (update) {
    sg <- foosball3_list_suggests()
    sg[!(sg %in% ignore)]
  } else {
    .foosball_suggest(ignore)
  }
  utils::install.packages(pkgs = suggests, ...)
}