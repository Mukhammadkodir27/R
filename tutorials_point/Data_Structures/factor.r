# R Factors
# R Factors
# Factors are used to categorize data. Examples of factors are: 
# Demography: Male/Female
# Music: Rock, Pop, Classic, Jazz
# Training: Strength, Stamina

# to create a factor, use the factor() function and add a vector as argument: 
music_genre <- factor(c("Jazz", "Rock", "Classic", "Classic", "Pop", "Jazz", "Rock", "Jazz"))
print(music_genre)

# from the example, you can see that factor has four levels(categories): Classic, Jazz, Pop and Rock

# to only print the levels, use the levels() function: 
print(levels(music_genre))

# You can also set the levels, by adding the levels argument inside the factor() function: 
