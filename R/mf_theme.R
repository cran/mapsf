#' @title Set a theme
#' @description This function set a map theme.
#' The parameters set by this function are the figure margins, background and
#' foreground colors and some \link{mf_title} options.
#' Use \code{mf_theme(NULL)} or \code{mf_theme('default')} to reset to default
#' theme settings.
#' @param x name of a map theme. One of "default", "brutal", "ink",
#' "dark", "agolalight", "candy", "darkula", "iceberg", "green", "nevermind",
#' "jsk", "barcelona".
#' @param bg background color
#' @param fg foreground color
#' @param mar margins
#' @param pos title position, one of 'left', 'center', 'right'
#' @param tab if TRUE the title is displayed as a 'tab'
#' @param cex cex of the title
#' @param font font of the title
#' @param line number of lines used for the title
#' @param inner if TRUE the title is displayed inside the plot area.
#' @details
#' It is also possible to set a custom theme using a list of arguments
#' (see Examples).
#' \code{mf_theme()} returns the current theme settings.
#' @return The (invisible) list of theme parameters is returned.
#' @export
#' @examples
#' mtq <- mf_get_mtq()
#'
#' # Choosing a theme by name:
#' mf_theme("default")
#' mf_map(mtq)
#' mf_title()
#'
#' # Specifying some values directly:
#' mf_theme(bg = "darkslategrey", fg = "lightgrey")
#' mf_map(mtq)
#' mf_title()
#'
#' # Using a mix of the above:
#' mf_theme("brutal", fg = "lightgreen", pos = "center", font = 2, tab = FALSE)
#' mf_map(mtq)
#' mf_title()
#'
#' # Specifying a list with theme values:
#' theme <- mf_theme("default")
#' theme$mar <- c(1, 1, 3, 1)
#' theme$line <- 2
#' theme$cex <- 1.5
#' mf_theme(theme)
#' mf_map(mtq)
#' mf_title()
#'
#' # or
#' theme <- list(
#'   bg = "green",
#'   fg = "red",
#'   mar = c(2, 2, 2, 2),
#'   tab = TRUE,
#'   pos = "center",
#'   inner = TRUE,
#'   line = 2,
#'   cex = 1.5,
#'   font = 3
#' )
#' mf_theme(theme)
#' mf_map(mtq)
#' mf_title()
#'
#' # Obtaining a list of parameters for the current theme:
#' mf_theme()
#'
#' # Removing the current theme:
#' mf_theme(NULL)
#' # or
#' mf_theme("default")
mf_theme <- function(x,
                     bg,
                     fg,
                     mar,
                     tab,
                     pos,
                     inner,
                     line,
                     cex,
                     font) {
  # current theme
  theme <- list(
    bg = getOption("mapsf.bg"),
    fg = getOption("mapsf.fg"),
    mar = getOption("mapsf.mar"),
    tab = getOption("mapsf.tab"),
    pos = getOption("mapsf.pos"),
    inner = getOption("mapsf.inner"),
    line = getOption("mapsf.line"),
    cex = getOption("mapsf.cex"),
    font = getOption("mapsf.font")
  )

  # if no arg input => return param list
  defined <- ls()
  passed <- names(as.list(match.call())[-1])
  if (!any(passed %in% defined)) {
    return(theme)
  }

  # input a theme name
  if (!missing(x)) {
    # if is.null(x) => set default theme
    if (is.null(x)) {
      x <- "default"
    }
    # if x is a list of args
    if (is.list(x)) {
      theme <- x
    } else {
      theme_names <- names(.gmapsf$themes)
      if (!x %in% theme_names) {
        stop(
          paste0(
            "x should be one of ",
            paste0(theme_names, collapse = ", ")
          ),
          call. = FALSE
        )
      } else {
        theme <- .gmapsf$themes[[x]]
      }
    }
  }

  # modify theme param
  if (!missing(bg)) theme$bg <- bg
  if (!missing(fg)) theme$fg <- fg
  if (!missing(mar)) theme$mar <- mar
  if (!missing(tab)) theme$tab <- tab
  if (!missing(pos)) theme$pos <- pos
  if (!missing(inner)) theme$inner <- inner
  if (!missing(line)) theme$line <- line
  if (!missing(cex)) theme$cex <- cex
  if (!missing(font)) theme$font <- font

  # set theme options
  options(
    mapsf.bg = theme$bg,
    mapsf.fg = theme$fg,
    mapsf.mar = theme$mar,
    mapsf.tab = theme$tab,
    mapsf.pos = theme$pos,
    mapsf.inner = theme$inner,
    mapsf.line = theme$line,
    mapsf.cex = theme$cex,
    mapsf.font = theme$font
  )

  return(invisible(as.list(theme)))
}
