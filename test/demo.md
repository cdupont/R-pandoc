Here is a plot of a function:

```` {.R}
require(stats)
D = 150
T = 10
t = seq(0, 80, 0.01)
x = -D*exp(-(t/T))+D
v = (D/T)*exp(-(t/T))
plot(t, x, type="l", main="Evolution of position through time", xlab="time (s)", ylab="position (m)", xlim=c(0,80), ylim=c(0, D+10),  xaxs = "i", yaxs = "i")
abline(h=D, col="red")
text(2, D-3, "D", 1, col="red")
````

