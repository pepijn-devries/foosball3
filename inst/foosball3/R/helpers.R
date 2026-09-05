strip_ansi <- function(x) gsub("(\\x9B|\\x1B\\[)[0-?]*[ -/]*[@-~]",
                               "", x, perl = TRUE)

get_description_field <- function(primary_key, field_names) {
  ## TODO check if this description field is correctly found for all editable tables
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

get_type_global <- function(table_name, field_name) {
  db_static_dictionary |>
    dplyr::filter(.data$table == table_name & .data$column == field_name) |>
    dplyr::pull("data_type")
}

get_primary_key_global <- function(table_name) {
  db_static_pk |>
    dplyr::filter(.data$table == table_name) |>
    dplyr::pull("pk_col")
}

foosball_meta <- foosball3::foosball3_meta_data()

tf <- tempfile()
foosball3::foosball3_create_db(tf)
con <- RSQLite::dbConnect(RSQLite::SQLite(), tf)
db_schema <- dm::dm_from_con(con, learn_keys = TRUE)
## Create a static map of all primary and foreign keys
db_static_pk <- dm::dm_get_all_pks(db_schema) |>
  tidyr::unnest_longer("pk_col")
db_static_fk <- dm::dm_get_all_fks(db_schema)
db_static_fk$parent_key_cols <- unlist(db_static_fk$parent_key_cols)
db_static_fk$child_fk_cols <- unlist(db_static_fk$child_fk_cols)
db_static_dictionary <- names(db_schema) |>
  lapply(\(table_name) {
    
    raw_schema <- DBI::dbGetQuery( con, sprintf("PRAGMA table_info('%s');", table_name) )
    
    tibble::tibble(
      table     = table_name,
      column    = raw_schema$name,
      data_type = raw_schema$type
    )
  }) |>
  dplyr::bind_rows()
db_join_keys <- function() {
  db_static_dictionary |>
    dplyr::left_join(
      db_static_pk, c(column = "pk_col", "table")
    ) |>
    dplyr::left_join(
      db_static_fk, c(table = "child_table", column = "child_fk_cols")
    )
}

get_schema_static <- function(con, include_views) {
  tbls <- RSQLite::dbListTables(con)
  
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
  dm::dm_draw(my_dm) |>
    DiagrammeRsvg::export_svg()
}

db_schemas_static <-
  lapply(c(TRUE, FALSE), get_schema_static, con = con)

RSQLite::dbDisconnect(con)
unlink(tf, TRUE, TRUE)
rm(con, tf, db_schema)