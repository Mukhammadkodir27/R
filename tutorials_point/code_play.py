name = readline("Please, enter your name: ")

introduce <- function(name){
  print(paste("Hello my dear", name))
}

introduce(name)


this_matrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 3)

print(this_matrix)


this_matrix <- matrix(c("apple", "banana", "cherry", "oranges"), nrow=2, ncol=2)
print(this_matrix)


print(this_matrix[1, 2])

print(this_matrix[2,])


this_matrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 3)

print(this_matrix)


this_matrix <- matrix(c("apple", "banana", "cherry", "oranges"), nrow=2, ncol=2)
print(this_matrix)


print(this_matrix[1, 2])

print(this_matrix[2,])


this_matrix <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow=3, ncol=3)
print(this_matrix)

print(this_matrix[c(1,2), ])
print(this_matrix[, c(1,2)])



new_matrix <- cbind(this_matrix, c(0, 0, 0))
print(new_matrix)

new_matrix <- rbind(new_matrix, c(0, 0, 0, 0))
print(new_matrix)



thismatrix <- matrix(c("apple", "banana", "cherry", "orange", "mango", "pineapple"), nrow = 3, ncol =2)
#Remove the first row and the first column
thismatrix <- thismatrix[-c(1), -c(1)]
print(thismatrix)


random_matrix <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow=3, ncol=3)
print(random_matrix)
#removing some selected values from rows and cols
random_matrix <- random_matrix[-c(1,2), -c(3)]
print(random_matrix)


