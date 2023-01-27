#' Georgia Tech colors
#'
#' Color scheme from Georgia tech
#'
#' @source <https://brand.gatech.edu/our-look/colors>
gatech_colors <- c(
  primary_gold                 = "#B3A369",
  primary_blue                 = "#003057",
  primary_white                = "#FFFFFF",
  accessible_gold_med          = "#A4925A",
  accessible_gold_drk          = "#857437",
  secondary_gray_matter        = "#54585A",
  secondary_pi_mile            = "#D6DBD4",
  secondary_diploma            = "#F9F6E5",
  secondary_buzz_gold          = "#EAAA00",
  tertiary_print_impact_purple = "#5F249F",
  tertiary_print_bold_blue     = "#3A5DAE",
  tertiary_print_olympic_teal  = "#008C95",
  tertiary_print_electric_blue = "#64CCC9",
  tertiary_print_canopy_lime   = "#A4D233",
  tertiary_print_rat_cap       = "#FFCD00",
  tertiary_print_new_horizon   = "#E04F39",
  tertiary_web_bright_purple   = "#7800FF",
  tertiary_web_bright_blue     = "#2961FF",
  tertiary_web_bright_electric = "#00FFFF",
  tertiary_web_bright_canaopy  = "#00EC9C",
  tertiary_web_bright_buzz     = "#FFCC00",
  tertiary_web_bright_horizon  = "#FF640F"
)

#' Function to extract gatech colors as hex codes
#'
#' @param ... Character names of gatech_colors
#'
gatech_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (gatech_colors)

  gatech_colors[cols]
}

gatech_palettes <- list(
  primary         = gatech_cols("primary_gold", "primary_blue"),

  text            = gatech_cols("primary_blue", "accessible_gold_med",
                             "accessible_gold_drk"),

  secondary     = gatech_cols("secondary_gray_matter", "secondary_pi_mile",
                             "secondary_diploma", "secondary_buzz_gold"),

  tertiary_print    = gatech_cols("tertiary_print_impact_purple",
                                  "tertiary_print_bold_blue",
                                  "tertiary_print_olympic_teal",
                                  "tertiary_print_electric_blue",
                                  "tertiary_print_canopy_lime",
                                  "tertiary_print_rat_cap",
                                  "tertiary_print_new_horizon"),

  tertiary_web     =  gatech_cols("tertiary_web_bright_purple",
                                  "tertiary_web_bright_blue",
                                  "tertiary_web_bright_electric",
                                  "tertiary_web_bright_canaopy",
                                  "tertiary_web_bright_buzz",
                                  "tertiary_web_bright_horizon")
)

#' Return function to interpolate a gatech color palette
#'
#' @param palette Character name of palette in gatech_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @export
gatech_pal <- function(palette = "primary", reverse = FALSE, ...) {
  if(!(palette %in% names(gatech_palettes))){
    stop(paste(palette,"is not a valid palette name for gatech_pal\n"))
  }

  pal <- gatech_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#' Color scale constructor for gatech colors
#'
#' @param palette Character name of palette in gatech_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @export
scale_color_gatech <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- gatech_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("gatech_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for gatech colors
#'
#' @param palette Character name of palette in gatech_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @export
scale_fill_gatech <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- gatech_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("gatech_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


#' Display a color palette
#'
#' Creates a visual of the color palette of the gatech colors
#'
#' @param palette Name of the color palette
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @return
#' @export
#'
display_gatech_pal<-function(palette, ...){
  if(!(palette %in% names(gatech_palettes))){
    stop(paste(palette,"is not a valid palette name for gatech_pal\n"))
  }

  pal <- gatech_pal(palette = palette, ...)
  n_colors <- length(unlist(gatech_palettes[palette]))
  image(1:n_colors,1,as.matrix(1:n_colors), col=pal(n_colors),
        xlab=paste(palette),ylab="",xaxt="n",yaxt="n",bty="n")

}



#' Display all gatech Color Palette Colors
#'
#' @export
#'
display_gatech_all <- function() {
    colorlist <- names(gatech_palettes)
    n <- purrr::map_dbl(gatech_palettes, length)
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
      shadi <- gatech_pal(colorlist[i])(nj)
      rect(xleft=0:(nj-1), ybottom=i-1, xright=1:nj, ytop=i-0.2, col=shadi,
           border="light grey")
    }

    text(rep(-0.1,nr),(1:nr)-0.6, labels=colorlist, xpd=TRUE, adj=1)
  }
