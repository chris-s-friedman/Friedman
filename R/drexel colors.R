drexel_colors <- c(
  primary_blue        = "#07294D",
  primary_yellow      = "#FFC600",
  accent_dark_blue    = "#006298",
  accent_dark_green   = "#949300",
  accent_light_blue   = "#6CACE4",
  accent_light_green  = "#B7BF10",
  accent_light_red    = "#D14124",
  accent_light_orange = "#FF8F1C",
  accent_gray_1       = "#A7A8AA",
  accent_gray_2       = "#BFB8AF",
  accent_gray_3       = "#BAC5B9",
  accent_gray_4       = "#D0D3D4",
  text_dark_gray      = "#333333",
  rule_light_gray     = "#E1E1E0",
  rule_red            = "#9E0B0F")

#' Function to extract drexel colors as hex codes
#'
#' @param ... Character names of drexel_colors
#'
drexel_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (drexel_colors)

  drexel_colors[cols]
}

drexel_palettes <- list(
  primary         = drexel_cols("primary_blue", "primary_yellow"),

  text            = drexel_cols("primary_blue", "accent_dark_blue",
                             "text_dark_gray"),

  accent_dark     = drexel_cols("accent_dark_blue", "accent_dark_green",
                             "rule_red", "accent_light_orange"),

  accent_light    = drexel_cols("accent_light_blue", "accent_light_green",
                             "accent_light_red", "accent_light_orange"),

  accent_gray     = drexel_cols("accent_gray_1", "accent_gray_2",
                             "accent_gray_3", "accent_gray_4"),

  rule            = drexel_cols("rule_light_gray", "rule_red"),

  grays           = drexel_cols("accent_gray_1", "accent_gray_2",
                             "accent_gray_3", "accent_gray_4",
                             "rule_light_gray"),

  ice             = drexel_cols("primary_blue", "accent_dark_blue",
                            "accent_light_blue"),

  fire            = drexel_cols("primary_yellow", "accent_light_orange",
                             "accent_light_red", "rule_red"),

  greens          = drexel_cols("accent_light_green", "accent_dark_green"),

  categorical     = drexel_cols("primary_blue", "primary_yellow", "rule_red",
                             "accent_dark_green"),

  categorical2    = drexel_cols("primary_blue", "primary_yellow", "rule_red",
                              "accent_dark_green", "accent_light_blue",
                              "accent_light_orange", "accent_light_red",
                              "accent_light_green"),

  ice_n_fire      = drexel_cols("primary_blue", "accent_dark_blue",
                               "accent_light_blue", "primary_yellow",
                               "accent_light_orange", "accent_light_red",
                               "rule_red"),

  ice_n_fire_div  = drexel_cols("primary_blue", "rule_light_gray",
                                "primary_yellow"),

  ice_n_fire_div2  = drexel_cols("primary_blue", "primary_yellow",
                                "rule_red")
)

#' Return function to interpolate a drexel color palette
#'
#' @param palette Character name of palette in drexel_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @export
drexel_pal <- function(palette = "primary", reverse = FALSE, ...) {
  if(!(palette %in% names(drexel_palettes))){
    stop(paste(palette,"is not a valid palette name for drexel_pal\n"))
  }

  pal <- drexel_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#' Color scale constructor for drexel colors
#'
#' @param palette Character name of palette in drexel_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @export
scale_color_drexel <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- drexel_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("drexel_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for drexel colors
#'
#' @param palette Character name of palette in drexel_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @export
scale_fill_drexel <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- drexel_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("drexel_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


#' Display a color palette
#'
#' Creates a visual of the color palette of the drexel colors
#'
#' @param palette Name of the color palette
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @return
#' @export
#'
display_drexel_pal<-function(palette, ...){
  if(!(palette %in% names(drexel_palettes))){
    stop(paste(palette,"is not a valid palette name for drexel_pal\n"))
  }

  pal <- drexel_pal(palette = palette, ...)
  n_colors <- length(unlist(drexel_palettes[palette]))
  image(1:n_colors,1,as.matrix(1:n_colors), col=pal(n_colors),
        xlab=paste(palette),ylab="",xaxt="n",yaxt="n",bty="n")

}



#' Display all Drexel Color Palette Colors
#'
#' @export
#'
display_drexel_all <- function() {
    colorlist <- names(drexel_palettes)
    n <- purrr::map_dbl(drexel_palettes, length)
    maxnum <- max(n)

    nr <- length(colorlist)
    nc <- max(n)

    ylim <- c(0,nr)
    oldpar <- par(mgp=c(2,0.25,0))

    on.exit(par(oldpar))
    plot(1, 1, xlim=c(0,nc), ylim=ylim, type="n", axes=FALSE, bty="n",
         xlab="", ylab="")

    for(i in 1:nr) {
      nj <- n[i]
      if (colorlist[i]=="") next
      shadi <- drexel_pal(colorlist[i])(nj)
      rect(xleft=0:(nj-1), ybottom=i-1, xright=1:nj, ytop=i-0.2, col=shadi,
           border="light grey")
    }

    text(rep(-0.1,nr),(1:nr)-0.6, labels=colorlist, xpd=TRUE, adj=1)
  }
