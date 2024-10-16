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
