name <- c("Elbek", "Kadir", "Boy", "Adil", "John", "Sardor", "Tommy", "Karim")
id <- c(10, 11, 12, 13, 14, 15, 16, 17)
age <- c(23, 22, 21, 25, 20, 17, 22, 19)
married <- c(F, F, F, T, T, F, T, F)
height <- c(170, 170, 170, 170, 170, 170, 170, 170)

mySet1<-data.frame(name,id,age,married,height)

print(mySet1)

# 1a
mySet1[5,]
# 1b
colnames(mySet1)[2]<-"column02"
# 1c
print(mySet1[1:7,])
print(head(mySet1,7))

#2
iris[seq(from=40,to=120,by=3),]

#3a
women[[1]]<-as.character(women[[1]])

#3c
women$shoe_size<-floor(runif(nrow(women),min=35,max=43)) 
