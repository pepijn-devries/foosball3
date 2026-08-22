#' Start the Foosball App
#' 
#' TODO
#' @param ... TODO
#' @returns Returns NULL silently.
#' @export
foosball3 <- function(...) {
  appdir <- system.file("foosball3", package = "foosball3")
  shiny::runApp(appDir = appdir, ...)
}

#' TODO
#'
#' TODO
#' @param file TODO
#' @param ... TODO
#' @returns TODO
#' @examples
#' fl <- tempfile(fileext = ".sqlite")
#' create_foosball3_db(fl)
#' 
#' ## Clean up:
#' unlink(fl, TRUE, TRUE)
#' @export
create_foosball3_db <- function(file, ...) {
  create_script <- readLines(
    system.file("creation_script.sql", package = "foosball3")
  ) |>
    paste(collapse = "\n") |>
    strsplit(";(?! )", perl = TRUE)
  create_script <- create_script[[1]]
  con <- RSQLite::dbConnect(
    RSQLite::SQLite(), file
  )
  result <- lapply(create_script, RSQLite::dbExecute, conn = con)
  if (any(unlist(result) != 0))
    stop("Failed to create database")

  copy_data <- \(df, nm) dplyr::copy_to( con, df, nm,
                                         overwrite = TRUE,
                                         temporary = FALSE )
  
  copy_data(
    dplyr::tibble(
      BALL_ID = -1L,
      BALL_DESCRIPTION = "Unknown"
    ), "balls")

  copy_data(
    dplyr::tibble(
      TOURN_TYPE_ID = 1L,
      TOURNAMENT_TYPE = "Individual players"
    ), "tournament_types")
  
  copy_data(
    dplyr::tibble(
      TOURNAMENT_STATE_CODE = c("ACT", "ARC"),
      TOURNAMENT_STATE = c("ACTIVE", "ARCHIVED")
    ), "tournament_states")

  copy_data(
    dplyr::tibble(
      TOURN_PHASE_CODE = c("Q", "S", "F", "N", "P"),
      TOURNAMENT_PHASE =
        c("Qualification", "Semi final", "Final", "Consolation final",
          "Practice"),
      IS_NESTED = as.integer(FALSE),
    ), "tournament_phases")

  copy_data(
    dplyr::tibble(
      BRAND_CODE = c("UNK", "BUF", "DTM", "GTS", "VDM"),
      BRAND_NAME =
        c("Unknown", "Buffalo", "Deutscher Meisters", "GTS Original Gametables",
          "Van der Meulen"),
      BRAND_COUNTRY = c(NA_character_, "NL", "DE", NA_character_, "NL")
    ), "brands")

  copy_data(
    dplyr::tibble(
      GENDER_CODE = c("M", "F", "O", "NS"),
      GENDER = c("Male", "Female", "Other", "Not specified")
    ), "genders")

  copy_data(
    dplyr::tibble(
      POINT_SYSTEM_ID = 1L:2L,
      PS_DESCRIPTION =
        c("Win = 3 points; Draw = 1 point; Lose = 0 points; Remainder = 0.9 * success_rate",
          "Number of goals scored minus penalty points"),
      MAX_POINTS_PER_MATCH = c(0L, 10L)
    ), "point_systems")

  expand.grid(
    ROLE_CODE = c("D", "S", "U"),
    SIDE_ID = c(1L, 2L, 0L, -1L)
  ) |>
    dplyr::mutate(
      POSITION =
        paste(dplyr::case_match(.data$ROLE_CODE,
                      "D" ~ "Defense",
                      "S" ~ "Strike",
                      "U" ~ "Unknown role"),
               c("opposite unknown side",
                 "unknown side",
                 "side 1", "Side 2")[(.data$SIDE_ID + 2L)]),
      POSITION_CODE =
        paste0(.data$ROLE_CODE,
               c("O", "U", "1", "2")[(.data$SIDE_ID + 2L)]),
      PSEUDO_SIDE_ID =
        ifelse(.data$SIDE_ID < 1L, 1L - .data$SIDE_ID, .data$SIDE_ID)
    ) |>
    copy_data("positions")

  copy_data(
    dplyr::tibble(
      ROLE_CODE = c("D", "S", "U"),
      ROLE = c("Defense", "Strike", "Unknown")
    ), "roles"
  )
  
  RSQLite::dbDisconnect(con)
}