### some functions are missing, please find them in the .pdf/.rmd in this repository, or
### elsewhere in my repositories

library(dplyr)
library(stringr)

d2r <- function(x){x / 180 * pi}

r2d <- function(x){x * 180 / pi}

max_day_length <- function(latitude, radius, verbose = T){
  axial_tilt <- 23.5
  theta1 <- 90 - latitude
  radius_at_latitude <- radius * sin(d2r(theta1))
  adj <- sqrt(radius^2 - radius_at_latitude^2)
  opp2 <- adj * tan(d2r(axial_tilt))
  if(opp2 > radius_at_latitude){
    theta2 <- 90
  } else {
    theta2 <- r2d(asin(opp2 / radius_at_latitude))
  }
  
  S_arc <- 180 + (2 * theta2)
  D_arc <- 360 - S_arc
  
  S_dur <- S_arc / 360 * 24
  D_dur <- 24 - S_dur
  
  if(verbose){
    print(paste0("Max day length: ", round(S_dur, 1)))
    print(paste0("Min night length: ", round(D_dur, 1)))
  } else {
    S_dur
  }
}

dh2hm <- function(x){
  y <- round(x, 2) %>% str_split("\\.") %>% unlist() %>% as.numeric()
  if(length(y) > 1){
    y[2] <- round(y[2] / 10 ^ nchar(y[2])  * 60, 0)
    paste0(y[1], "H ", y[2], "M")
  } else {
    paste0(y[1], "H")
  }
}

my_lat <- 50
max_day <- max_day_length(my_lat, 5, verbose = T)
min_day <- 24 - max_day

x <- seq(0, 2 * pi, length.out = 367)[1:366]
y <- sin(x)
y = (y + 1) / 2 # so that range [0, 1]
y <- inRange(c(min_day, max_day), y)

winter_solstice <- which.min(y)
y <- y[c((winter_solstice + 1):366, 1:winter_solstice)]
par(pty = "m")
mar(w = 5.1, s = 4.1)
plot(x, y, type = "n", ylim = inRange(c(min_day, max_day), c(0, 1.2)), las = 1, axes = F, xlab = "", 
     ylab = paste0("Day length (h)  @ ", my_lat, "\u00b0"), cex.lab = 1.5)
abline(h = 12, col = "darkgrey", lty = 2)
abline(v = x[c(1, 92, 366 * 0.5, 366 * 0.75)], col = "darkgrey")
mtext(3, 1, at = x[1], text = "Winter\nsolstice")
mtext(3, 1, at = x[92], text = "Spring\nequinox")
mtext(3, 1, at = x[round(366 * 0.5, 0)], text = "Summer\nsolstice")
mtext(3, 1, at = x[round(366 * 0.75)], text = "Autumn\nequinox")
text(x[round(inRange(c(1, 366), c(0, 0.25, 0.5, 0.75)), 0)], 18.5, labels = sapply(y[round(inRange(c(1, 366), c(0, 0.25, 0.5, 0.75)), 0)], dh2hm), col = "darkred")
points(x, y, col = "#00000099")

year_dates <- data.frame(ymd = seq(as.Date("2023-12-21"), as.Date("2024-12-20"), by = 1))
year_dates$doy <- year_dates$ymd %>% str_split("-") %>% sapply("[[", 3)
month_starts <- which(year_dates$doy == "01")
months <- c("Jan\n", "Feb\n", "Mar\n", "Apr\n", "May\n", "Jun\n", "Jul\n", "Aug\n", "Sep\n", "Oct\n", "Nov\n", "Dec\n")
points(x[month_starts], y[month_starts], pch = 16)
arrows(x[month_starts], y[month_starts], x[month_starts], y[month_starts] + 0.5, length = 0)
text(x[month_starts], y[month_starts], months, pos = 3)
mtext(1, 1.5, at = inRange(x, 0.5), text = "Day of solar year", cex = 1.5)
today <- which(year_dates$ymd == Sys.Date())
points(x[today], y[today], pch = 16, col = "darkred")
text(x[today], y[today], paste0("Today: ", dh2hm(y[today])), pos = ifelse(today > 366/2, 2, 4), col = "darkred")
internal_axis_ticks(2, 2)
box()


### consult an app or website and see what that says about day length to compare
sunrise_today <- lubridate::hm("06:05")
sunset_today <- lubridate::hm("20:05")
sunset_today - sunrise_today

