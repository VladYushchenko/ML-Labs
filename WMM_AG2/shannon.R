SNR <- function(x, qx){
  out <- mean(x*x)/mean((x-qx)*(x-qx))
  out <- 10*log10(out)
  return(out)
}

shannon <- function (x, fj, SF, padding=FALSE)
{
  sinc <- function (t) ifelse (t==0, 1, sin(t)/t)
  fx <- rep (0, length(x))
  for (j in seq(along=fj))
    fx <- fx + fj[j] * sinc (pi * (x-j/SF) * SF)
  if (padding)
    for (j in seq(along=fj))
    {
      fx <- fx + fj[j] * sinc (pi * (x-(j-length(fj))/SF) * SF)
      fx <- fx + fj[j] * sinc (pi * (x-(j+length(fj))/SF) * SF)
    }
  return (fx)
}

resampling <- function (fun, SF=800, t.max=0.03, N=48, M=200, titel="Funktionsabtastung"){
  # plot echte Funktion
  curve (fun, from=0, to=t.max)
  
  # plot abgetastete Werte
  #xAbt <- seq(0, t.max, length.out = N)
  xAbt <- seq(t.max/N, t.max, by = t.max/N)
  funAbt <- fun(xAbt)
  points(xAbt, funAbt, col = "blue")

  # Shannon-Werte ohne "padding"
  x <- seq(0, t.max, length.out = M)
  shanNoPad <- shannon(x, fun(x), SF)
  snrNoPad <- SNR(fun(x),shanNoPad)
  lines(x, shanNoPad, type = "l", lty = 3, col = "red")
  
  # Shannon-Werte mit "padding"
  shanPad <- shannon(x, fun(x), SF, TRUE)
  snrPad <- SNR(fun(x), shanPad)

  lines(x, shanPad, type = "l", lty = 2, col = "green")
  title(titel, cex.main = 1.1)
  legend("bottom", c(paste('SNR', toString(round(snrNoPad, 2)), "Pad: F", sep = " "), 
                     paste('SNR', toString(round(snrPad, 2)), "Pad: T", sep = " ")), 
         lty = c(3, 2), col = c("red", "green")
         )
}

# Testfunktion sin, 120 freq
sinfreq <- function(t, freq = 120){
  return (sin(t*2*pi*freq))
}

# Testfunktion cos, 120 freq
cosfreq <- function(t, freq = 128){
  return (cos(t*2*pi*freq))
}

rechteck <- function(t, t_max = 0.03, freq = 120){
  z <- ifelse (floor(t*2*freq) %% 2 == 1, 1, 0)
  return (z)
}

sagefunc <- function(t, t_max = 0.03, freq = 120){
  return (((t %% (1/freq))*freq))
}

dreieck <- function(t, t_max = 0.03, freq = 120){
  temp <- ifelse ((floor(t*freq + t%/%freq) %% 2) == 0, t, 1-t)
  z <- ((temp %% (1/(freq)))*freq)
  return (z)
}

resampling(sinfreq, titel = "func = sin, freq = 120")
resampling(rechteck, titel = "func = rechteck, freq = 120")
resampling(sagefunc, titel = "func = sagefunc, freq = 120")
resampling(dreieck, titel = "func = dreieck, freq = 120")
