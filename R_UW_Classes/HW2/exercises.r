# 3a
table<-matrix(0,nrow=5,ncol=2);table[,1]<-3
# 3b
table<-matrix(0,nrow=5,ncol=2);table[3,2]<-20
# 4 
sampleString<-"Mukhammadkodir";sampleMatrix<-matrix(c(1,0),nrow=2,ncol=2);sampleNumeric<-27;sampleVector<-c(T,F,T,F,F,F,F,T,T,F,T);myList<-list(sampleString,sampleMatrix,sampleNumeric,sampleVector);last_element<-length(myList);myList[[last_element]][4]

# 4d
sampleString<-"Mukhammadkodir";sampleMatrix<-matrix(c(1,0),nrow=2,ncol=2);sampleNumeric<-27;sampleVector<-c(T,F,T,F,F,F,F,T,T,F,T);myList<-list(sampleString,sampleMatrix,sampleNumeric,sampleVector);last_element<-length(myList);myList[[last_element]][5]<-NA;print(myList[[4]])
