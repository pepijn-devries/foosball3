
.nav_switch <- function(id, icon_name, label, value = TRUE, right = TRUE, width = "150px") {
  bslib::nav_item(
    shiny::div(
      class = "px-3",
      onclick = "event.stopPropagation();",
      shinyWidgets::materialSwitch(
        id, shiny::span(bsicons::bs_icon(icon_name), label),
        value = value, right = right, width = width)
    )
  )
}

.strip_ansi <- function(x) gsub("(\\x9B|\\x1B\\[)[0-?]*[ -/]*[@-~]",
                               "", x, perl = TRUE)

.get_description_field <- function(primary_key, field_names) {
  if (length(primary_key) > 1) return(paste(primary_key, collapse = "|"))
  object <- stringr::str_replace_all(primary_key, "_ID$|_CODE", "")
  df <- field_names[
    grepl(
      sprintf("^%s$|(?=.*(NAME|DESCR))^%s", object, object),
      field_names, perl = TRUE
    ) & field_names != primary_key
  ][1]
  if (is.na(df)) df <- primary_key
  df
}

.get_type_global <- function(table_name, field_name, dictionary) {
  dictionary |>
    dplyr::filter(.data$table %in% table_name & .data$column %in% field_name) |>
    dplyr::pull("data_type")
}

.get_primary_key_global <- function(table_name, primary) {
  primary |>
    dplyr::filter(.data$table == table_name) |>
    dplyr::pull("pk_col")
}

.foosball3_create_db <- function(file, ...) {
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
  protect_data <- \(con, nm, what, when) {
    condition <-
      switch(
        typeof(when),
        integer = sprintf("%s < %i", what, when),
        character = sprintf("%s IN (%s)", what,
                            paste(sprintf("'%s'", when), collapse = ", "))
      )
    fmt <-
      paste("CREATE TRIGGER protect_%s_from_%s",
            "BEFORE %s ON %s",
            "WHEN OLD.%s",
            "BEGIN",
            "    SELECT RAISE(FAIL, 'This record is protected and cannot be altered.');",
            "END;", sep = "\n")
    RSQLite::dbExecute(con, sprintf(fmt, nm, "update", "UPDATE", nm, condition))
    RSQLite::dbExecute(con, sprintf(fmt, nm, "delete", "DELETE", nm, condition))
  }
  
  copy_data(
    dplyr::tibble(
      BALL_ID = -1L,
      BALL_DESCRIPTION = "Unknown"
    ), "balls")
  
  protect_data(con, "balls", "BALL_ID", 0L)
  
  copy_data(
    dplyr::tibble(
      TOURNAMENT_STATE_CODE = c("ACT", "ARC"),
      TOURNAMENT_STATE = c("ACTIVE", "ARCHIVED")
    ), "tournament_states")
  protect_data(con, "tournament_states", "TOURNAMENT_STATE_CODE", c("ACTIVE", "ARCHIVED"))
  
  copy_data(
    dplyr::tibble(
      TOURNAMENT_PHASE_CODE = c("Q", "S", "F", "N", "P"),
      TOURNAMENT_PHASE =
        c("Qualification", "Semi final", "Final", "Consolation final",
          "Practice"),
      IS_NESTED = as.integer(FALSE)
    ), "tournament_phases")
  protect_data(con, "tournament_phases", "TOURNAMENT_PHASE_CODE", c("Q", "S", "F", "N", "P"))
  
  copy_data(
    dplyr::tibble(
      TOURNAMENT_TYPE_CODE = c("I"),
      TOURNAMENT_TYPE = c("Individual"),
      TOURNAMENT_TYPE_DESCRIPTION = "Players compete individually"),
    "tournament_types")
  protect_data(con, "tournament_types", "TOURNAMENT_TYPE_CODE", c("I"))
  
  copy_data(
    dplyr::tibble(
      TOURNAMENT_TYPE_CODE = "I",
      PHASE_ORDER = c(1L, 2L, 3L, 4L, 4L),
      TOURNAMENT_PHASE_CODE = c("P", "Q", "S", "N", "F"),
      IS_OPTIONAL = as.integer(c(TRUE, FALSE, FALSE, TRUE, FALSE))),
    "tournament_phase_flow")
  protect_data(con, "tournament_phase_flow", "TOURNAMENT_TYPE_CODE", c("I"))
  
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
  protect_data(con, "qualifications", "QUALIFICATION_CODE", c("H", "R", "O", "F", "E"))
  
  copy_data(
    dplyr::tibble(
      BRAND_CODE = c("UNK", "BUF", "DTM", "GTS", "VDM", "BNZ", "FPD"),
      BRAND_NAME =
        c("Unknown", "Buffalo", "Deutscher Meister", "GTS Original Gametables",
          "Van der Meulen", "Bonzini", "Fas Pendezza"),
      BRAND_COUNTRY = c(NA_character_, "NL", "DE", "NL", "NL", "IT", "FR")
    ), "brands")
  
  copy_data(
    dplyr::tibble(
      GENDER_CODE = c("M", "F", "O", "NS"),
      GENDER = c("Male", "Female", "Other", "Not specified")
    ), "genders")
  protect_data(con, "genders", "GENDER_CODE", c("M", "F", "O", "NS"))
  
  copy_data(
    dplyr::tibble(
      POINT_SYSTEM_ID = 1L:2L,
      POINT_SYSTEM_DESCRIPTION =
        c("Win = 3 points; Draw = 1 point; Lose = 0 points; Remainder = 0.9 * success_rate",
          "Number of goals scored minus penalty points"),
      MAX_POINTS_PER_MATCH = c(10L, 10L)
    ), "point_systems")
  protect_data(con, "point_systems", "POINT_SYSTEM_ID", 3L)
  
  copy_data(
    dplyr::tibble(
      SIDE_ID = -1L:2L,
      SIDE_DESCRIPTION = c("Unknown opposed side", "Unknown side", "One", "Two")
    ), "table_sides")
  protect_data(con, "table_sides", "SIDE_ID", 3L)
  
  positions <-
    expand.grid(
      ROLE_CODE = c("D", "S", "U"),
      SIDE_ID = c(1L, 2L, 0L, -1L)
    ) |>
    dplyr::mutate(
      POSITION =
        paste(dplyr::recode_values(
          .data$ROLE_CODE,
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
    )
  copy_data(positions, "positions")
  protect_data(con, "positions", "POSITION_CODE", positions$POSITION_CODE)
  
  copy_data(
    dplyr::tibble(
      ROLE_CODE = c("D", "S", "U"),
      ROLE = c("Defense", "Strike", "Unknown")
    ), "roles"
  )
  protect_data(con, "roles", "ROLE_CODE", c("D", "S", "U"))
  
  RSQLite::dbDisconnect(con)
}

#' @noRd
#' @include schema.R
.db_rel <- function(...) {
  if (requireNamespace("dm")) {
    tf <- tempfile()
    .foosball3_create_db(tf)
    
    con <- RSQLite::dbConnect(RSQLite::SQLite(), tf)
    on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
    db_schema <- foosball3_get_schema(con)
    db_static_fk <- dm::dm_get_all_fks(db_schema)
    db_static_fk$parent_key_cols <- unlist(db_static_fk$parent_key_cols)
    db_static_fk$child_fk_cols <- unlist(db_static_fk$child_fk_cols)
    db_static_pk <- dm::dm_get_all_pks(db_schema) |>
      tidyr::unnest_longer("pk_col")
    db_static_dictionary <- names(db_schema) |>
      lapply(\(table_name) {
        
        raw_schema <- RSQLite::dbGetQuery( con, sprintf("PRAGMA table_info('%s');", table_name) )
        
        dplyr::tibble(
          table     = table_name,
          column    = raw_schema$name,
          data_type = raw_schema$type
        )
      }) |>
      dplyr::bind_rows()
    list(
      dictionary = db_static_dictionary,
      primary = db_static_pk,
      foreign = db_static_fk
    )
  }
}

.db_join_keys <- function(
    dictionary, primary_keys, foreign_keys) {
  dictionary |>
    dplyr::left_join(
      primary_keys, c(column = "pk_col", "table")
    ) |>
    dplyr::left_join(
      foreign_keys, c(table = "child_table", column = "child_fk_cols")
    )
}

.foosball3_get_schema <- function(con, include_views = FALSE, ...) {
  tbls <- RSQLite::dbListTables(con)
  
  if (requireNamespace("dm")) {
    my_dm <- dm::dm_from_con(con, learn_keys = TRUE)
    if (include_views) {
      all_tbls <- RSQLite::dbListTables(con)
      unreferenced <- setdiff(all_tbls, names(my_dm))
      unreferenced <- unreferenced[!grepl("^sqlite_", unreferenced)]
      
      unreferenced <-
        lapply(unreferenced, dplyr::tbl, src = con) |>
        stats::setNames(unreferenced)
      for (i in seq_along(unreferenced)) {
        my_dm <-
          my_dm |>
          dm::dm(!!names(unreferenced)[[i]] := unreferenced[[i]])
      }
    }
    return(my_dm)
  } else {
    rlang::abort(c(
      x = "This function needs the 'dm' package",
      i = "Please install 'dm' and try again"
    ))
  }
}
