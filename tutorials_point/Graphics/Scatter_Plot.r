x <- c(5,7,8,7,2,2,9,4,11,12,9,6)
y <- c(99,86,87,88,111,103,87,94,78,77,85,86)

plot(x, y, main="Observation of Cars", xlab="Car age", ylab="Car speed")
# here(x, y) so one item from x and one item from y, and index[][] is found by this way

# To recap, the observation in the example above is the result of 12 cars passing by.
# The x-axis shows how old the car is.
# The y-axis shows the speed of the car when it passes.
# Are there any relationships between the observations?
# It seems that the newer the car, the faster it drives, but that could be a coincidence,
# after all we only registered 12 cars.



# draw two plots on the same figure
# day one, the age and speed of 12 cars:
x1 <- c(5,7,8,7,2,2,9,4,11,12,9,6)
y1 <- c(99,86,87,88,111,103,87,94,78,77,85,86)

# day two, the age and speed of 15 cars:
x2 <- c(2,2,8,1,15,8,12,9,7,3,11,4,7,14,12)
y2 <- c(100,105,84,105,90,99,90,95,94,100,79,112,91,80,85)

plot(x1, y1, main="Observation of Cars", xlab="Car age", ylab="Car speed", col="red", cex=2, pch=19)
points(x2, y2, col="blue", cex=2, pch=19)

