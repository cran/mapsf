#' Title
#'
#' @param pal pal
#' @param nbreaks nbreaks
#' @param alpha alpha
#' @noRd
#' @importFrom grDevices hcl.pals hcl.colors
get_the_pal <- function(pal, nbreaks, alpha = 1) {
  if (length(pal) == 1) {
    if (pal %in% hcl.pals()) {
      cols <- hcl.colors(n = nbreaks, palette = pal, alpha = alpha, rev = TRUE)
    } else {
      cols <- rep(pal, nbreaks)
    }
  } else {
    cols <- pal
  }
  return(cols)
}


get_col_vec <- function(x, breaks, pal) {
  itv <- findInterval(x, breaks, all.inside = FALSE, rightmost.closed = TRUE)
  itv[itv == 0] <- length(breaks)
  colvec <- pal[itv]
  return(colvec)
}



#' @name create_dots
#' @title create_dots
#' @description Create clean, sorted sf object with centroid coordinates from
#' an sf object
#' @param x x
#' @param var var
#' @return A sorted and cleaned sf object with centroid coordinates.
#' @noRd
create_dots <- function(x = x, var = var) {
  # get centroid coords
  x <- cbind(
    st_coordinates(st_centroid(
      x = st_geometry(x),
      of_largest_polygon = TRUE
    )),
    x
  )
  # remove NAs and 0 values
  x <- x[!is.na(x = x[[var]]), ]
  x <- x[x[[var]] != 0, ]
  # turn to positive values
  x[[var]] <- abs(x[[var]])
  # Order the dots
  x <- x[order(x[[var]], decreasing = TRUE), ]
  return(x)
}





#' @name get_size
#' @title get_size
#' @description get a vector of radii
#' @param inches inches
#' @param var var
#' @param fixmax fixmax
#' @param symbol symbols
#' @return a vector of radii
#' @noRd
get_size <- function(var, inches, val_max, symbol) {
  switch(symbol,
    circle = {
      smax <- inches * inches * pi
      size <- sqrt((var * smax / val_max) / pi)
    },
    square = {
      smax <- inches * inches
      size <- sqrt(var * smax / val_max)
    }
  )
  return(size)
}





# Plot symbols
plot_symbols <- function(symbol, dots, sizes, mycols, border, lwd, inches) {
  switch(symbol,
    circle = {
      symbols(
        x = dots[, 1:2, drop = TRUE],
        circles = sizes,
        bg = mycols,
        fg = border,
        lwd = lwd,
        add = TRUE,
        inches = inches,
        asp = 1
      )
    },
    square = {
      symbols(
        x = dots[, 1:2, drop = TRUE],
        squares = sizes,
        bg = mycols,
        fg = border,
        lwd = lwd,
        add = TRUE,
        inches = inches * 2,
        asp = 1
      )
    }
  )
}


#' @name check_order
#' @title checkOrder
#' @description check if col order match legend.values.order
#' @param val_order val_order
#' @param mod vector of modalities
#' @return  a vector of legend.values.order.
#' @noRd
check_order <- function(val_order, mod) {
  if (!missing(val_order)) {
    m <- match(mod, val_order)
    m <- m[!is.na(m)]

    if (length(m) != length(mod) | length(mod) != length(val_order)) {
      stop(
        paste(
          "'val_order' modalities must fit the modalities of 'var' (",
          paste(mod, collapse = ","), ").",
          sep = ""
        ),
        call. = FALSE
      )
    }
  } else {
    val_order <- sort(mod)
  }
  return(val_order)
}


get_modalities <- function(x, val_order) {
  mod <- unique(x)
  mod <- mod[!is.na(mod)]
  # check val_order vs mod values
  val_order <- check_order(val_order, mod)
}

get_col_typo <- function(x, pal, val_order) {
  # get the colors
  refcol <- data.frame(
    mod = val_order,
    col = pal,
    stringsAsFactors = FALSE
  )
  mycols <- refcol[match(x, refcol[, 1]), 2]
}

get_sym_typo <- function(x, pch, val_order) {
  # get the colors
  refsym <- data.frame(
    mod = val_order,
    pch = pch,
    stringsAsFactors = FALSE
  )
  mysym <- refsym[match(x, refsym[, 1]), 2]
}



# split multiple legend position
split_leg <- function(x) {
  llp <- length(x)
  if (llp == 2) {
    lp1 <- x[1]
    lp2 <- x[2]
  }
  if (llp == 3) {
    tt <- tryCatch(as.numeric(x[1]), warning = function(w) w)
    if (methods::is(tt, "warning")) {
      lp1 <- x[1]
      lp2 <- as.numeric(x[2:3])
    } else {
      lp1 <- as.numeric(x[1:2])
      lp2 <- x[3]
    }
  }
  if (llp == 4) {
    lp1 <- x[1:2]
    lp2 <- x[3:4]
  }
  list(lp1, lp2)
}



get_geom_type <- function(x) {
  a <- list(
    other = "GEOMETRY", POINT = "POINT", LINE = "LINESTRING",
    POLYGON = "POLYGON",
    POINT = "MULTIPOINT",
    LINE = "MULTILINESTRING", POLYGON = "MULTIPOLYGON",
    other = "GEOMETRYCOLLECTION", other = "CIRCULARSTRING",
    other = "COMPOUNDCURVE", other = "CURVEPOLYGON",
    other = "MULTICURVE", other = "MULTISURFACE",
    other = "CURVE", other = "SURFACE", other = "POLYHEDRALSURFACE",
    other = "TIN", other = "TRIANGLE"
  )
  type <- st_geometry_type(x)
  levels(type) <- a
  type <- as.character(unique(type))
  if (length(type) > 1) {
    stop("GEOMETRYCOLLECTION objects should have consistent type",
      call. = FALSE
    )
  }
  return(type)
}
