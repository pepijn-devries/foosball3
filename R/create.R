#' Create a New Foosball Database
#'
#' Creates an SQLite file at the specified location. It will contain
#' all required relationships and constraints and some non-trivial data.
#' @param file File path where the database needs to be stored
#' @param ... Ignored
#' @returns Returns `NULL` invisibly.
#' @examples
#' fl <- tempfile(fileext = ".sqlite")
#' foosball3_create_db(fl)
#' 
#' ## Clean up:
#' unlink(fl, TRUE, TRUE)
#' @export
foosball3_create_db <- function(file, ...) {
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
                                         append = TRUE,
                                         temporary = FALSE )
  
  copy_data(
    dplyr::tibble(
      BALL_ID = -1L,
      BALL_DESCRIPTION = "Unknown"
    ), "balls")
  
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
      IS_NESTED = as.integer(FALSE)
    ), "tournament_phases")

  copy_data(
    dplyr::tibble(
      TOURN_TYPE_CODE = c("I"),
      TOURNAMENT_TYPE = c("Individual"),
      TOURN_TYPE_DESCRIPTION = "Players compete individually"),
    "tournament_types")
  
  copy_data(
    dplyr::tibble(
      QUALIFICATION_CODE = c("H", "R", "O", "F", "E"),
      QUALIFICATION =
        c("Historic", "Plays rarely/never", "Plays ocasionally",
          "Plays frequently", "Plays frequently and excels"),
      QUALIFICATION_RATIO = c(1, 0.71, 0.88, 1, 1.7),
      QUALIFICATION_SUCCESS = c(0.5, 0.3, 0.43, 0.5, 0.71),
      QUALIFICATION_DESCR =
        c("Based on historical performance",
          NA_character_, NA_character_, NA_character_, NA_character_)
    ), "qualifications")
  
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

  copy_data(
    dplyr::tibble(
      SIDE_ID = -1L:2L,
      SIDE = c("Unknown opposed side", "Unknown side", "One", "Two")
    ), "table_sides")
  
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
                "Side 1", "Side 2")[(.data$SIDE_ID + 2L)]),
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