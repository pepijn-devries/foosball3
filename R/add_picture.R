#' Add a Picture to a Foosball Database
#' 
#' The foosball3 database can use a JPG picture to extract participant avatars,
#' or just to document a tournament. With this function you can add a
#' JPG image to the database.
#' @param con A connection to foosball3 SQLite database (see example).
#' @param tournament Tournament identifier (`integer`), indicating the
#' tournament where the picture was taken.
#' @param file A file path (`character`) or a connection (readable,
#' searchable and binary) to the image file that needs to be added to
#' the database
#' @param description A `character` describing the picture.
#' @param ... Ignored
#' @returns Data is added to the database with [dplyr::rows_upsert].
#' This function returns the result of the [dplyr::rows_upsert] call.
#' @examples
#' # TODO
#' if (interactive()) {
#'   target   <- tempfile(fileext = ".db")
#'   zip_file <- system.file(package = "foosball3", "example.zip")
#'   img_file <- unz(zip_file, "pictures/JPG_DATA1.jpg", "rb")
#'   foosball3_import_db(zip_file, target)
#'   con <- RSQLite::dbConnect(RSQLite::SQLite(), target)
#'   foosball3_add_picture(con, 1L, img_file, "group photo")
#' 
#'   # Cleanup
#'   RSQLite::dbDisconnect(con)
#'   close(img_file)
#'   unlink(target, TRUE, TRUE)
#' }
#' @export
foosball3_add_picture <- function(
    con, tournament, file, description = NA_character_, ...) {
  file_con <- if (inherits(file, "connection")) {
    file
  } else {
    on.exit(close(file_con), add = TRUE)
    file(file, "rb")
  }
  raw_data <- raw()
  chunk_size <- 1048576L
  repeat {
    chunk <- readBin(file_con, "raw", chunk_size)
    raw_data <- c(raw_data, chunk)
    if (length(chunk) < chunk_size) break
  }
  jpg_dim <- .extract_jpg_dim(raw_data)
  lazy_tib <- dplyr::tbl(con, "pictures")
  new_id <-
    lazy_tib |>
    dplyr::summarise(new_id = max(.data$PICTURE_ID, na.rm = TRUE) + 1L) |>
    dplyr::pull("new_id")
  
  new_data <- dplyr::tibble(
    PICTURE_ID          = new_id,
    TOURNAMENT_ID       = tournament,
    PICTURE_DESCRIPTION = description,
    PICTURE_WIDTH       = jpg_dim[["width"]],
    PICTURE_HEIGHT      = jpg_dim[["height"]],
    JPG_DATA            = list(raw_data)
  )

  dplyr::rows_upsert(
    lazy_tib,
    new_data,
    by       = "PICTURE_ID",
    in_place = TRUE,
    copy     = TRUE
  )
}

.extract_jpg_dim <- function(raw_bytes) {
  len_bytes <- length(raw_bytes)
  
  if (len_bytes < 4L || !all(raw_bytes[1L:2L] == c("ff", "d8"))) {
    stop("Not a valid JPEG image payload.")
  }
  
  pos <- 3L
  
  while (pos < len_bytes) {
    if (raw_bytes[pos] == "ff") {
      pos <- pos + 1
      if (pos > len_bytes) break
      
      marker <- raw_bytes[pos]
      
      if (marker == "ff" || marker == "00") {
        pos <- pos + 1L
        next
      }
      
      if (marker %in% as.raw(0xc0:0xcf) && !(marker %in% c("c4", "cc"))) {
        start_idx <- pos + 4L
        
        if ((start_idx + 3L) > len_bytes) stop("Corrupted JPEG: Header block truncated.")
        
        height <- readBin(raw_bytes[start_idx:(start_idx + 1L)], "integer",
                          size = 2, n = 1, signed = FALSE, endian = "big")
        width  <- readBin(raw_bytes[(start_idx + 2L):(start_idx + 3L)], "integer",
                          size = 2, n = 1, signed = FALSE, endian = "big")
        
        return(c(width = width, height = height))
      } else {
        pos <- pos + 1L
        if ((pos + 1L) > len_bytes) break
        
        chunk_len <- as.integer(raw_bytes[pos]) * 256L + as.integer(raw_bytes[pos+1])
        pos <- pos + chunk_len
      }
    } else {
      pos <- pos + 1L
    }
  }
  stop("Could not find an SOF dimension marker inside the raw data payload.")
}
