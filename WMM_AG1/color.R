load("color.rda")

# Aufgabe 3.a)
vec <- seq(0,1, length.out=48)
greyvalue <- array(0, c(48,48,3))
greyvalue[,,] <- vec
plot.array(greyvalue, main="(a) Greyvalue plot")

# Aufgabe 3.b)
bluevalue <- array(0, c(48,48,3))
bluevalue[,,3] <- vec
plot.array(bluevalue, main="(b) Bluevalue plot")

# Aufgabe 3.c)
redgreenvalue <- array(0, c(48,48,3))
redgreenvalue[,,2] <- vec
redgreenvalue <- aperm(redgreenvalue, c(2,1,3))
redgreenvalue[,,1] <- vec
plot.array(redgreenvalue, main="(c) Red+green plot")

# Aufgabe 3.d)
colorpic <- redgreenvalue;
colorpic[,,3] <- colorpic[,,1] + colorpic[,,2]
colorpic[,,3] <- array(1, c(48,48)) - colorpic[,,3]
colorpic <- replace(colorpic, colorpic[,,3] < 0,0)
plot.array(colorpic, main="(d) R + G + B = 1 plot")

# Aufgabe 3.e)
maxrgbmat <- apply(colorpic,1:2,max)
maxrgbmat <- replace(maxrgbmat, maxrgbmat[,] == 0, 1)

multminpic <- colorpic
multminpic[,,,] <- colorpic[,,]/maxrgbmat
#multminpic[,,2] <- colorpic[,,2]/maxrgbmat
#multminpic[,,3] <- colorpic[,,3]/maxrgbmat
plot.array(multminpic, main="(e) Mult Normierung")

# Aufgabe 3.f)
addminpic <- colorpic
addminpic[,,1] <- colorpic[,,1] + (array(1, c(48,48)) - maxrgbmat)
addminpic[,,2] <- colorpic[,,2] + (array(1, c(48,48)) - maxrgbmat)
addminpic[,,3] <- colorpic[,,3] + (array(1, c(48,48)) - maxrgbmat)
plot.array(addminpic, main="(f) Additiv Normierung")