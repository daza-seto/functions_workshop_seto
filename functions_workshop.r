## Function workshop
## Nov 14 2023
## Created by Daisuke Seto

airtemps <- c(212,20.3,78,32)
celsius1 <- (airtemps[1]-32)*5/9

## wirte function
#' Converting 
#'
#' @param fahr 
#'
#' @return numeric to numeric vector in degrees celsius
#' @export
#'
#' @examples
#' F_to_C(32)
#' F_to_C(c(32,20.4,72))
#' 
F_to_C <- function(fahr){
celsius <- (fahr -32)*5/9
return(celsius)
}


C_to_F <- function(cels){
  fahr <- celsius *9/5 +32
  return(fahr)
}

result <- F_to_C(airtemps)
airtemp = result


## Create a function to covert 
convert_temps <- function(fahr){
  celsius <- (fahr -32)*5/9
  
  kelvin <- celsius + 273
  
  return(list(fahr = fahr, celsius = celsius, kelvin = kelvin))
  
}

temps_df <- data.frame(convert_temps(seq(-100,100,10)))

## function 
custom_theme <- function(base_size = 9) {
  ggplot2::theme(
    text             = ggplot2::element_text(family = 'Helvetica', 
                                             color = 'gray30', 
                                             size = base_size),
    plot.title       = ggplot2::element_text(size = ggplot2::rel(1.25), 
                                             hjust = 0.5, 
                                             face = 'bold'),
    panel.background = ggplot2::element_blank(),
    panel.border     = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = 'grey90', 
                                             linewidth = 0.25),
    legend.position  = 'right',
    legend.key       = ggplot2::element_rect(colour = NA, 
                                             fill = NA),
    axis.ticks       = ggplot2::element_blank(),
    axis.line        = ggplot2::element_blank()
  )
}

library(ggplot2)

ggplot(temps_df, mapping = aes(x = fahr, y = celsius, color = kelvin)) +
  geom_point() +
  custom_theme(19)
