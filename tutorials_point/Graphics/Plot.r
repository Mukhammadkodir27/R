# to draw one point in the diagram at position (1) and position(3)
plot(1, 3)  # plot(x, y)

# to draw more points use vector
plot(c(1, 8), c(8, 10))
#Multiple Points
plot(c(1, 2, 3, 4, 5), c(3, 7, 8, 9, 12))

# for better organization use variables
x <- c(1, 2, 3, 4, 5)
y <- c(6, 7, 8, 9, 10)
plot(x, y)

# sequence of points
plot(1:10)

#draw a line
plot(1:10, type="l")

# Plot Labels: main, xlab - x label, ylab - y label
plot(1:10, type="l", main="My Graph", xlab="The x-axis", ylab="The y-axis")

# change color
plot(1:10, col="red")

plot(1:10, cex=2)

plot(1:10, cex=2, col="blue")

plot(1:10, pch=12, cex=2)



# Line Graph
plot(1:10, type="l", col="red", lwd=9)

# Line Styles
plot(1:10, type="l", lwd=5, col="blue", lty=6)

line1 <- c(1, 2, 3, 4, 5, 10, 6)
plot(line1, type="l", col="blue")

line2 <- c(2, 5, 7, 8, 9, 10, 10)
plot(line2, type="l", col="red")

lines3 <- c(3, 1, 4, 5, 8, 3, 10)

plot(line1, type="l", lwd=4, col="blue")
lines(line2, type="l", lwd=4, col="red")
lines(lines3, type="l", lwd=4, col="black")
