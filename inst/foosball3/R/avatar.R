avatarServer <- function(id, db) {
  TILE_SIZE <- 75
  ICON_SIZE <- 25
  anonym <- imager::load.image("www/img/anonymous.png")
  anonym <- lapply(c(ICON_SIZE, TILE_SIZE), \(sz) {
    im <- imager::resize(anonym, sz, sz, interpolation_type = 5)
    tf <- tempfile(fileext = ".jpg")
    imager::save.image(im, tf)
    dat <- readBin(tf, "raw", file.size(tf))
    unlink(tf, TRUE, TRUE)
    base64enc::dataURI(dat, mime = "image/png")
  }) |>
    stats::setNames(c("icon", "tile"))
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      shiny::observeEvent(get_picture_tags(), {
        if (avatar_generator$status() == "running") {
          bslib::hide_toast(ns("avatar_progress"))
          browser() #TODO stop process if old on is still running
        }
        avatar_generator$invoke(get_picture_tags())
        
        foosball_progress(ns("avatar_progress"), "Initiating avatars...")

      })
      
      get_picture_tags <- shiny::reactive({
        shiny::req(db())
        con <- db()$connect()
        on.exit({RSQLite::dbDisconnect(con)}, add = TRUE)
        pcts <- dplyr::tbl(con, "pictures") |>
          dplyr::collect()
        tags <- dplyr::tbl(con, "picture_tags") |>
          dplyr::collect()
        dir <- file.path(tempdir(), "avatars")
        if (!dir.exists(dir)) dir.create(dir)
        list(
          pictures = pcts,
          tags     = tags,
          dir      = dir
        )
      })

      get_tagged_persons <- shiny::reactive({
        get_picture_tags()$tags$PERSON_ID |> unique()
      })
      
      avatar_generator <- shiny::ExtendedTask$new(\(pt) {
        unlink(file.path(pt$dir, "*"))
        avatar_expression <- quote({
          for (picid in pcts$PICTURE_ID) {
            pic <- as.list(pcts[pcts$PICTURE_ID == picid,])
            picf <- tempfile(fileext = ".jpg")
            writeBin(pic$JPG_DATA[[1]], picf)
            img <- picf |>
              imager::load.image()
            unlink(picf, TRUE, TRUE)
            
            for (tid in tags$TAG_ID[tags$PICTURE_ID == picid]) {
              tag <- as.list(tags[tags$TAG_ID == tid,])
              avat <- imager::imsub(img,
                                    x > (tag$TAG_X - tag$TAG_SIZE),
                                    y > (tag$TAG_Y - tag$TAG_SIZE),
                                    x <= 2*tag$TAG_SIZE,
                                    y <= 2*tag$TAG_SIZE)
              avat <- imager::renorm(avat)
              avat <- imager::resize(avat, tile_size, tile_size, interpolation_type = 1)
              imager::save.image(
                avat,
                file.path(dir, sprintf("tile%i.png", tag$TAG_ID)))
              imager::save.image(
                imager::resize(avat, icon_size, icon_size, interpolation_type = 1),
                file.path(dir, sprintf("icon%i.png", tag$TAG_ID)))
            }
          }
        })
        if (requireNamespace("mirai", quietly = TRUE)) {
          m <- mirai::mirai(
            avatar_expression,
            pcts = pt$pictures,
            tags = pt$tags,
            dir = pt$dir,
            tile_size = TILE_SIZE,
            icon_size = ICON_SIZE)
          return(m)
        } else {
          
          eval_env <- list2env(list(
            pcts = pt$pictures, 
            tags = pt$tags, 
            dir = pt$dir, 
            tile_size = TILE_SIZE, 
            icon_size = ICON_SIZE
          ), parent = parent.frame())
          
          eval(avatar_expression, envir = eval_env)
          return(TRUE)
        }

      })

      shiny::observe({
        if (avatar_generator$status() == "running") {
          pt <- get_picture_tags()
          tot_files <- pt$tags |> nrow()
          n_files <- as.integer(length(list.files(pt$dir))/2)
          if (tot_files > 0) {
            perc <- as.integer(100*n_files/tot_files)
            shinyjs::js$progressbar(
              id = ns("avatar_progress"),
              percent = perc,
              text = sprintf("Generated %i of %i avatar files", n_files, tot_files)
            )
            shiny::invalidateLater(2000)

          }
        } else {
          bslib::hide_toast(ns("avatar_progress"))
        }
      })
      
      parse_image <- function(uri, id, person_id, my_class, clickable) {
        sprintf(
          "<img src=\"%s\" class=\"%s\" %s/>",
          uri,
          paste(c(my_class, if (clickable) "foosball-clickable" else NULL),
                collapse = " "),
          ifelse(
            clickable,
            sprintf(
              "onClick='Shiny.onInputChange(`%s`, {id: %i,timestamp: new Date()});'",
              id,
              person_id),
            "")
        )
      }
      
      return(
        shiny::reactive({
          click <- input$face_click #TODO doesn't work
          status <- avatar_generator$status()
          if (status == "success") {
            pt <- get_picture_tags()
            list(
              click = click,
              tagged_persons = get_tagged_persons(),
              get_avatar = function(person_id, what = "icon", side = NULL, clickable = FALSE) {
                my_class = paste0("foosball-avatar",
                                  ifelse(is.null(side), "", side))
                what <- match.arg(what, c("icon", "tile"))
                tags <- pt$tags
                tag_id <-
                  tags |>
                  dplyr::filter(.data$PERSON_ID == person_id) |>
                  dplyr::sample_n(min(c(dplyr::n(), 1L))) |>
                  dplyr::pull("TAG_ID")
                if (length(tag_id) == 1) {
                  fp <- file.path(tempdir(), "avatars",
                                  sprintf("%s%i.png", what, tag_id))
                  if (!file.exists(fp)) {
                    img_dat <- anonym[[what]]
                  } else {
                    img_dat <- readBin( fp, "raw", file.size(fp) ) |>
                      base64enc::dataURI(mime = "image/png")
                  }
                } else {
                  img_dat <- anonym[[what]]
                }
                parse_image(img_dat, ns("face_click"), person_id, my_class, clickable)
              }
            )
          } else {
            list(
              click = click,
              tagged_persons = get_tagged_persons(),
              get_avatar = function(person_id, what = "icon", side = NULL, clickable = FALSE) {
                my_class = paste0("foosball-avatar",
                                  ifelse(is.null(side), "", side))
                img_dat <- anonym[[what]]
                parse_image(img_dat, ns("face_click"), person_id, my_class, clickable)
              }
            )
          }
        })
      )
    }
  )
}