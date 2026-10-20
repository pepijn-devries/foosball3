library(foosball3)
old_wd <- getwd()
dest   <- file.path(normalizePath("inst"), "example.zip")
tryCatch({unlink(dest)})
setwd(file.path("data-raw", "example-data"))
utils::zip(dest, list.files(recursive = TRUE),
           extras = "-q", flags = "-q")
setwd(old_wd)
