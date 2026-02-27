# rtemis color system
# ::rtemis.utils::
# 2025- EDG rtemis.org

# Violet: Class names (structure)
# Blue: Outer resampling (evaluation)
# Orange: Hyperparameter tuning (optimization)
# Green: Model training + important highlights (execution)
# Cyan: Info messages (communication)

# References
# ANSI escape codes
# https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
# Xterm color names: https://jonasjacek.github.io/colors/
# CSS color keywords: https://www.uxgem.com/docs/css-color-keywords
# Unicode emojis: https://www.unicode.org/emoji/charts/full-emoji-list.html
# UTF-8 icons: https://www.utf8icons.com/

rtemis_light_teal <- "#00fdfd"
rtemis_light_blue <- "#30cefe"
rtemis_teal <- "#00b2b2"
kaimana_red <- "#ff004c"
kaimana_blue <- "#0067e0"
kaimana_light_blue <- "#479cff"
coastside_orange <- "#ff9f20"
rtemis_orange <- "#ff4f36"
kaimana_green <- "#00ffb3"
kaimana_med_green <- "#00996b"
rtemis_purple <- "#6125f7"
rtemis_magenta <- "#912ac8"
rtemis_light_magenta <- "#b25bd6"
magenta <- "#ff00ff"
lmd_burgundy <- "#a92459"
rtms_gray <- "#808080"

rt_gray <- rtms_gray
rt_red <- kaimana_red
rt_blue <- kaimana_light_blue
rt_green <- kaimana_med_green
rt_orange <- coastside_orange
rt_teal <- rtemis_teal
rt_purple <- rtemis_purple
rt_magenta <- rtemis_light_magenta

# %% rtemis colors ----
highlight_col <- rt_orange
col_object <- rt_gray # object name in repr_S7name
col_info <- highlight2_col <- lmd_burgundy
col_outer <- rt_green
col_tuner <- rt_blue
col_calibrator <- rt_magenta


#' rtemis Color System
#'
#' A named list of colors used consistently across all packages
#' in the rtemis ecosystem.
#'
#' Colors are provided as hex strings.
#'
#' @format A named list with the following elements:
#' \describe{
#'   \item{red}{"kaimana red"}
#'   \item{blue}{"kaimana light blue"}
#'   \item{green}{"kaimana medium green"}
#'   \item{orange}{"coastside orange"}
#'   \item{teal}{"rtemis teal"}
#'   \item{purple}{"rtemis purple"}
#'   \item{magenta}{"rtemis magenta"}
#'   \item{highlight_col}{"highlight color"}
#'   \item{object}{"rtemis teal"}
#'   \item{info}{"lmd burgundy"}
#'   \item{outer}{"kaimana red"}
#'   \item{tuner}{"coastside orange"}
#' }
#'
#' @examples
#' rtemis_colors[["orange"]]
#'
#' @author EDG
#'
#' @export
#'
#' @examples
#' rtemis_colors[["teal"]]
rtemis_colors <- list(
  red = rt_red,
  light_blue = kaimana_light_blue,
  blue = rt_blue,
  green = rt_green,
  medium_green = kaimana_med_green,
  orange = rt_orange,
  light_teal = rtemis_light_teal,
  teal = rt_teal,
  purple = rt_purple,
  light_magenta = rtemis_light_magenta,
  magenta = rt_magenta
) # /rtemis.utils::rtemis_colors
