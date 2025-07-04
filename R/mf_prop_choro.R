#' @title Plot proportional symbols using choropleth coloration
#' @description Plot proportional symbols with colors based on a quantitative
#' data classification.
#' @eval my_params(c(
#' 'x',
#' 'var',
#' 'border',
#' 'lwd',
#' 'add' ,
#' 'inches',
#' 'val_max',
#' 'symbol',
#' 'col_na',
#' 'pal',
#' 'alpha',
#' 'rev',
#' 'breaks',
#' 'nbreaks',
#' 'leg_pos2',
#' 'leg_title',
#' 'leg_title_cex',
#' 'leg_val_cex',
#' 'leg_val_rnd',
#' 'leg_no_data',
#' 'leg_frame',
#' 'leg_frame_border',
#' 'leg_fg',
#' 'leg_bg',
#' 'leg_size',
#' 'leg_box_border',
#' 'leg_box_cex',
#' 'leg_adj',
#' 'leg_horiz'))
#' @details
#' Breaks defined by a numeric vector or a classification method are
#' left-closed: breaks defined by \code{c(2, 5, 10, 15, 20)}
#' will be mapped as [2 - 5[, [5 - 10[, [10 - 15[, [15 - 20].
#' The "jenks" method is an exception and has to be right-closed.
#' Jenks breaks computed as \code{c(2, 5, 10, 15, 20)}
#' will be mapped as [2 - 5], ]5 - 10], ]10 - 15], ]15 - 20].
#' @keywords internal
#' @export
#' @return x is (invisibly) returned.
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_map(mtq, c("POP", "MED"), "prop_choro")
#'
#' mf_map(mtq)
#' mtq[6, "MED"] <- NA
#' mf_map(
#'   x = mtq, var = c("POP", "MED"), type = "prop_choro",
#'   inches = .35, border = "tomato4",
#'   val_max = 90000, symbol = "circle", col_na = "grey", pal = "Cividis",
#'   breaks = "equal", nbreaks = 4, lwd = 4,
#'   leg_pos = c("bottomright", "bottomleft"),
#'   leg_title = c("Population", "Median Income"),
#'   leg_title_cex = c(0.8, 1),
#'   leg_val_cex = c(.7, .9),
#'   leg_val_rnd = c(0, 0),
#'   leg_no_data = "No data",
#'   leg_frame = c(TRUE, TRUE),
#'   add = TRUE
#' )
mf_prop_choro <- function(x,
                          var,
                          inches = 0.3,
                          val_max,
                          symbol = "circle",
                          pal,
                          alpha = NULL,
                          rev = FALSE,
                          breaks = "quantile",
                          nbreaks,
                          border,
                          lwd = .7,
                          col_na = "white",
                          leg_pos = mf_get_leg_pos(x, 1),
                          leg_title = var,
                          leg_title_cex = c(.8, .8),
                          leg_val_cex = c(.6, .6),
                          leg_val_rnd = c(0, 2),
                          leg_no_data = "No data",
                          leg_frame = c(FALSE, FALSE),
                          leg_frame_border,
                          leg_horiz = c(FALSE, FALSE),
                          leg_adj = c(0, 0),
                          leg_fg,
                          leg_bg,
                          leg_size = 1,
                          leg_box_border,
                          leg_box_cex = c(1, 1),
                          add = TRUE) {
  # default
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))

  pal <- go(pal, "pal_seq", "Mint")
  leg_box_border <- go(leg_box_border, "highlight")
  leg_fg <- go(leg_fg, "highlight")
  leg_bg <- go(leg_bg, "foreground", getOption("mapsf.background"))
  leg_frame_border <- go(
    leg_frame_border, "foreground",
    getOption("mapsf.highlight")
  )
  border <- go(border, "background")

  var2 <- var[2]
  var1 <- var[1]
  # check merge and order
  dots <- create_dots(x = x, var = var1)

  # jenks
  jen <- FALSE
  if (any(breaks %in% "jenks")) {
    jen <- TRUE
  }
  # get the breaks
  breaks <- mf_get_breaks(
    x = dots[[var2]], nbreaks = nbreaks,
    breaks = breaks
  )
  nbreaks <- length(breaks) - 1
  # get the cols
  pal <- get_the_pal(pal = pal, nbreaks = nbreaks, alpha = alpha, rev = !rev)
  # get the color vector
  mycols <- get_col_vec(x = dots[[var2]], breaks = breaks, pal = pal, jen = jen)

  no_data <- FALSE
  if (max(is.na(mycols)) == 1) {
    no_data <- TRUE
  }
  mycols[is.na(mycols)] <- col_na

  # Default max value
  if (missing(val_max)) {
    val_max <- max(dots[[var1]])
  }

  # get sizes
  sizes <- get_size(
    var = dots[[var1]], inches = inches,
    val_max = val_max, symbol = symbol
  )

  # size and values for legend, hollow circle (fixmax case)
  size_max <- max(sizes)
  val <- seq(sqrt(min(dots[[var1]])), sqrt(max(dots[[var1]])), length.out = 4)
  val <- val * val
  if (inches <= size_max) {
    inches <- size_max
    borders <- border
  } else {
    mycols <- c(NA, mycols)
    borders <- c(NA, rep(border, nrow(dots)))
    dots <- rbind(dots[1, ], dots)
    dots[1, var] <- val_max
    sizes <- c(inches, sizes)
  }

  # empty plot if needed
  if (add == FALSE) {
    mf_init(x)
  }

  # Plot the symbols
  plot_symbols(
    symbol = symbol, dots = dots, sizes = xinch(sizes),
    mycols = mycols, border = borders, lwd = lwd,
    inches = inches
  )

  leg_pos <- split_leg(leg_pos)

  border <- getOption("mapsf.highlight")
  if (is.null(getOption("mapsf.legacy"))) {
    ccol <- getOption("mapsf.foreground")
  } else {
    ccol <- "grey80"
  }
  if (all(leg_frame, !leg_horiz)) {
    ccol <- getOption("mapsf.background")
  }

  if (length(leg_pos) == 1) {
    ## TEST Double args
    la1 <- list(
      type = "prop",
      val = val,
      title = leg_title[1],
      symbol = symbol,
      inches = size_max,
      col = ccol,
      val_rnd = leg_val_rnd[1],
      border = border,
      lwd = lwd,
      horiz = leg_horiz[1],
      self_adjust = TRUE
    )
    lg <- do.call(leg_comp, la1)
    la2 <- list(
      leg = lg,
      type = "choro",
      val = breaks,
      title = leg_title[2],
      val_rnd = leg_val_rnd[2],
      col_na = col_na,
      no_data = no_data,
      no_data_txt = leg_no_data,
      horiz = leg_horiz[2],
      pal = pal,
      box_border = leg_box_border,
      box_cex = leg_box_cex
    )
    lg <- do.call(leg_comp, la2)
    leg_draw(lg,
      pos = leg_pos[[1]], bg = leg_bg, fg = leg_fg, size = leg_size,
      frame = leg_frame[1], title_cex = leg_title_cex[1],
      val_cex = leg_val_cex[1], mar = getOption("mapsf.mar"),
      adj = leg_adj, frame_border = leg_frame_border
    )
  } else {
    leg(
      type = "prop",
      pos = leg_pos[[1]], val = val, title = leg_title[1],
      symbol = symbol, inches = size_max, col = ccol,
      title_cex = leg_title_cex[1], val_cex = leg_val_cex[1],
      val_rnd = leg_val_rnd[1],
      horiz = leg_horiz[1],
      frame = leg_frame[1], border = border, lwd = lwd,
      bg = leg_bg, fg = leg_fg, self_adjust = TRUE,
      mar = getOption("mapsf.mar"), size = leg_size
    )
    leg(
      type = "choro",
      pos = leg_pos[[2]], val = breaks, title = leg_title[2],
      title_cex = leg_title_cex[2], val_cex = leg_val_cex[2],
      val_rnd = leg_val_rnd[2], horiz = leg_horiz[2],
      col_na = col_na, no_data = no_data, no_data_txt = leg_no_data,
      frame = leg_frame[2], pal = pal, bg = leg_bg, fg = leg_fg,
      size = leg_size, box_border = leg_box_border, box_cex = leg_box_cex
    )
  }

  return(invisible(x))
}
