avatarUI <- function(id) {
  ns <- shiny::NS(id)
  js_updater <- shiny::tags$script(HTML(sprintf("
  Shiny.addCustomMessageHandler('%s', function(data) {
    var progressBar = document.querySelector('#%s .avatar-progress-bar');
    var progressText = document.querySelector('#%s .avatar-progress-text');

    if (progressBar) {
      progressBar.style.width = data.pct + '%%';
      progressBar.innerText = '\u00a0' + data.pct + '%%';
    }
    if (progressText) {
      progressText.innerText = 'Generated ' + data.count + ' of ' + data.total + ' files...';
    }
  });", ns("avatar_progress"), ns("avatar_progress"), ns("avatar_progress"))))
  tags$head(js_updater)
}

avatarServer <- function(id, db) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(get_picture_tags(), {
        if (avatar_generator$status() == "running") {
          bslib::hide_toast(ns("avatar_progress"))
          browser() #TODO stop process if old on is still running
        }
        avatar_generator$invoke(get_picture_tags())

        bslib::show_toast(
          bslib::toast(
            shiny::div(
              class = "d-flex flex-column gap-2 w-100",
              shiny::div("Initiating avatars...", class = "avatar-progress-text", style = "margin-bottom: 8px;"),
              shiny::div(
                class = "progress",
                shiny::div(
                  class = "avatar-progress-bar progress-bar-striped progress-bar-animated bg-primary",
                  style = "width: 0%;",
                  " 0%"
                )
              )
            ),
            duration_s = NA,
            id = ns("avatar_progress"),
            type = NULL,
            position = "bottom-right")
        )
        
      })
      
      get_picture_tags <- shiny::reactive({
        shiny::req(db())
        con <- db()$connect()
        RSQLite::dbExecute(con, "PRAGMA busy_timeout = 10000;")
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
      
      avatar_generator <- shiny::ExtendedTask$new(\(pt) {
        unlink(file.path(pt$dir, "*"))
        m <- mirai::mirai({
          for (picid in pcts$PICTURE_ID) {
            pic <- as.list(pcts[pcts$PICTURE_ID == picid,])
            img <- pic$JPG_DATA[[1]] |>
              jpeg::readJPEG() |>
              aperm(c(2, 1, 3)) |>
              imager::as.cimg() |>
              suppressWarnings()
            for (tid in tags$TAG_ID[tags$PICTURE_ID == picid]) {
              tag <- as.list(tags[tags$TAG_ID == tid,])
              avat <- imager::imsub(img,
                                    x > (tag$TAG_X - tag$TAG_SIZE),
                                    y > (tag$TAG_Y - tag$TAG_SIZE),
                                    x <= 2*tag$TAG_SIZE,
                                    y <= 2*tag$TAG_SIZE)
              avat <- imager::renorm(avat)
              avat <- imager::resize(avat, 75, 75, interpolation_type = 1)
              imager::save.image(
                avat,
                file.path(dir, sprintf("tile%i.png", tag$TAG_ID)))
              imager::save.image(
                imager::resize(avat, 25, 25, interpolation_type = 1),
                file.path(dir, sprintf("icon%i.png", tag$TAG_ID)))
            }
          }
        }, pcts = pt$pictures, tags = pt$tags, dir = pt$dir)
        m
      })
      
      shiny::observe({
        if (avatar_generator$status() == "running") {
          pt <- get_picture_tags()
          tot_files <- pt$tags |> nrow()
          n_files <- length(list.files(pt$dir))/2
          if (tot_files > 0) {
            perc <- as.character(as.integer(100*n_files/tot_files))
            session$sendCustomMessage(ns("avatar_progress"), list(
              pct = perc,
              count = n_files,
              total = tot_files
            ))
            shiny::reactiveTimer(2000)()

          }
        } else {
          bslib::hide_toast(ns("avatar_progress"))
        }
      })
      
      return(
        reactive({
          status <- avatar_generator$status()
          if (status == "success") {
            pt <- get_picture_tags()
            list(
              get_avatar = function(person_id, what = "icon", side = NULL) {
                what <- match.arg(what, c("icon", "tile"))
                tags <- pt$tags
                tag_id <-
                  tags |>
                  dplyr::filter(.data$PERSON_ID == person_id) |>
                  dplyr::sample_n(min(c(dplyr::n(), 1L))) |>
                  dplyr::pull("TAG_ID")
                if (length(tag_id) == 1) {
                  my_class = paste0("foosball-avatar",
                                    ifelse(is.null(side), "", side))
                  fp <- file.path(tempdir(), "avatars",
                                  sprintf("%s%i.png", what, tag_id))
                  if (!file.exists(fp)) return(NULL)
                  img_dat <- readBin( fp, "raw", file.size(fp) )
                  sprintf(
                    "<img src=\"%s\" class=\"%s\"/>",
                    base64enc::dataURI(img_dat, mime = "image/png"),
                    my_class)
                } else {
                  NULL
                  #TODO
                }
              }
            )
          } else {
            list(
              get_avatar = function(person_id, what = "icon", side = NULL) {
                NULL #TODO anonymous
              }
            )
          }
        })
      )
    }
  )
}