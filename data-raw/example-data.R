library(foosball3)
library(imager)
img <- load.image(file.path("data-raw", "chatgpt_group_photo.png"))
save.image(img, file.path("data-raw", "example-data", "pictures", "JPG_DATA1.jpg"),
           quality = .5)
old_wd <- getwd()
dest   <- file.path(normalizePath("inst"), "example.zip")
tryCatch({unlink(dest)})
setwd(file.path("data-raw", "example-data"))
utils::zip(dest, list.files(recursive = TRUE),
           extras = "-q", flags = "-q")
setwd(old_wd)
