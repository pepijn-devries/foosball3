r_files <- list.files("inst/", pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)

extract_namespaced_calls <- function(file) {
  p_data <- utils::getParseData(parse(file, keep.source = TRUE))
  if (is.null(p_data)) return(data.frame(Package = character(), Function = character()))
  
  ns_indices <- which(p_data$token == "SYMBOL_PACKAGE")
  ns_indices <- ns_indices[p_data$text[ns_indices + 1] == "::"]
  
  if (length(ns_indices) == 0) return(data.frame(Package = character(), Function = character()))
  
  pkgs <- p_data$text[ns_indices]
  funs <- p_data$text[ns_indices + 2]
  
  data.frame(Package = pkgs, Function = funs, stringsAsFactors = FALSE)
}
all_deps <-
  do.call(rbind, lapply(r_files, extract_namespaced_calls)) |>
  unique()

base_pkgs <- c("base", "compiler", "datasets", "graphics", "grDevices", 
               "grid", "methods", "parallel", "splines", "stats", 
               "stats4", "tcltk", "tools", "utils")
exclude <- c("foosball3")

deps <- all_deps[!all_deps$Package %in% c(base_pkgs, exclude), ]
deps <- deps[order(deps$Package, deps$Function),]

roxygen_lines <- by(deps, deps$Package, function(sub) {
  pkg <- sub$Package[1]
  funs_str <- paste(sort(unique(sub$Function)), collapse = " ")
  paste0("#' @importFrom ", pkg, " ", funs_str)
})
roxygen_lines <- as.character(roxygen_lines)

file_content <- c(
  "## Generated automatically with data-raw/dependencies.R, do not edit by hand",
  "#' @title Internal Shiny Dependencies",
  "#' @description This file automatically registers dependencies used only in inst/.",
  roxygen_lines,
  "#' @importFrom rlang !! := .data",
  "#' @noRd",
  "NULL"
)

writeLines(file_content, "R/dependencies.R")
