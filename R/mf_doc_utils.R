my_params <- function(x) {
  params <- list(
    xfull = "x object of class \\code{sf} or \\code{sfc}",
    x = "x object of class \\code{sf}",
    var = "var name(s) of the variable(s) to plot",
    vars = "var names of the variables to plot",
    bg = "bg background color",
    fg = "fg foreground color",
    border = "border border color",
    lwd = "lwd border width",
    inches = paste0(
      "inches size of the biggest symbol (radius for circles,",
      " half width for squares) in inches."
    ),
    lwd_max = "lwd_max line width of the largest line",
    symbol = "symbol type of symbols, 'circle' or 'square'",
    col = "col color",
    leg_pos = paste0(
      "leg_pos position of the legend, one of 'topleft', 'top',",
      "'topright', 'right', 'bottomright', 'bottom', ",
      "'bottomleft', 'left' or a vector of two coordinates ",
      "in map units (c(x, y)). If leg_pos = NA then the ",
      "legend is not plotted. If leg_pos = 'interactive' click on",
      "the map to choose the legend position."
    ),
    leg_pos2 = paste0(
      "leg_pos position of the legend, two of 'topleft', 'top','topright', ",
      "'right','bottomright', 'bottom', 'bottomleft', ",
      "'left' or a vector of two ",
      "coordinates in map units (c(x, y)). leg_pos argument can be ",
      "c('position', 'position'), c('position', x2, y2), ",
      "c(x1,y1, 'position') or c(x1, y1, x2, y2). ",
      "Use NA to avoid plotting the legend, use 'interactive' to choose the",
      "legend position interactively."
    ),
    leg_title = "leg_title legend title",
    leg_title_cex = "leg_title_cex size of the legend title",
    leg_val_cex = "leg_val_cex size of the values in the legend",
    leg_val_rnd =
      "leg_val_rnd number of decimal places of the values in the legend",
    val_order =
      "val_order values order, a character vector that matches var modalities",
    leg_frame =
      "leg_frame whether to add a frame to the legend (TRUE) or not (FALSE)",
    leg_no_data = "leg_no_data label for missing values",
    add =
      "add whether to add the layer to an existing plot (TRUE) or not (FALSE)",
    pal = paste0(
      "pal a set of colors or a palette name",
      " (from \\link{hcl.colors})"
    ),
    rev = paste0(
      "rev if \\code{pal} is a \\link{hcl.colors} palette name, ",
      "whether the ordering of the colors should be reversed (TRUE)",
      " or not (FALSE)"
    ),
    alpha = "alpha opacity, in the range [0,1]",
    col_na = "col_na color for missing values",
    cex_na = "cex_na point size for NA values",
    pch_na = "pch_na point type for NA values",
    val_max = "val_max maximum value used for proportional symbols",
    breaks = paste0(
      "breaks either a numeric vector with the actual breaks, ",
      "or a classification method name ",
      "(see \\link{mf_get_breaks} and Details)"
    ),
    nbreaks = "nbreaks number of classes",
    pos = paste0(
      "pos position. It can be one of 'topleft', 'top',",
      "'topright', 'right', 'bottomright', 'bottom',",
      "'bottomleft', 'left', 'interactive' or a vector of two coordinates ",
      "in map units (c(x, y))"
    ),
    title = "title legend title",
    title_cex = "title_cex size of the legend title",
    val_cex = "val_cex size of the values in the legend",
    val_rnd = "val_rnd number of decimal places of the values in the legend",
    frame = "frame whether to add a frame to the legend (TRUE) or not (FALSE)",
    no_data_txt = "no_data_txt label for missing values",
    no_data = "no_data if TRUE a 'missing values' box is plotted",
    cex = "cex size of the legend; 2 means two times bigger",
    cexs = "cex cex (point size) for symbols",
    pch = "pch pch (point type) for symbols",
    leg_adj = "leg_adj adjust the postion of the legend in x and y directions",
    leg_horiz = paste0(
      "leg_horiz display the legend horizontally ",
      "(for proportional symbols and choropleth types)"
    ),
    leg_frame_border = "leg_frame_border border color of the legend frame",
    leg_bg = "leg_bg color of the legend backgournd",
    leg_fg = "leg_fg color of the legend foreground",
    leg_size = "leg_size size of the legend; 2 means two times bigger",
    leg_border = "leg_border symbol border color(s)",
    leg_box_border = "leg_box_border border color of legend boxes",
    leg_box_cex = paste0(
      "leg_box_cex width and height size expansion ",
      "of boxes"
    )
  )


  for (i in seq_along(params)) {
    params[[i]] <- paste0(
      "@param ", " ",
      params[[i]]
    )
  }

  unname(unlist(params[x]))
}
