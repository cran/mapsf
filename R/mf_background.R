#' @title Plot a background image
#' @description Plot a background image on an existing plot
#' @param filename filename of the background image, PNG or JPG/JPEG format.
#' @param ... ignored
#'
#' @return No return value, a background image is displayed.
#' @export
#'
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq, col = NA, border = NA)
#' mf_background(system.file("img/background.jpg", package = "mapsf"))
#' mf_map(mtq, lwd = 3, col = NA, border = "white", add = TRUE)
#' mf_credits(
#'   txt = "Background photo by Noita Digital on Unsplash",
#'   col = "white"
#' )
mf_background <- function(filename, ...) {
  test_cur_plot()
  ex <- strsplit(basename(filename), split = "\\.")[[1]]
  ex <- tolower(ex[length(ex)])
  if (ex == "png") {
    if (!requireNamespace("png", quietly = TRUE)) {
      stop(
        "'png' is package needed for this function to work. Please install it.",
        call. = FALSE
      )
    }
    img <- png::readPNG(filename)
  }
  if (ex %in% c("jpg", "jpeg")) {
    if (!requireNamespace("jpeg", quietly = TRUE)) {
      stop(
        paste0(
          "'jpeg' is package needed for this function to work. ",
          "Please install it."
        ),
        call. = FALSE
      )
    }
    img <- jpeg::readJPEG(filename)
  }


  recordGraphics(
    {
      pusr <- par("usr")
      graphics::rasterImage(
        image   = img,
        xleft   = pusr[1],
        ybottom = pusr[3],
        xright  = pusr[2],
        ytop    = pusr[4]
      )
    },
    list = list(img = img),
    env = getNamespace("mapsf")
  )
}
