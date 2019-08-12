#marialundon
#assignent3


#install.packages("expm")
library(stats)
library(expm) #Usage: Compute the exponential of a matrix.
main <- function(){
genMatrix <- function(g){
  #Takes a vector of entries for a matrix.
  #Then turns vectopr into a square four dimensional matrix.
  entries <- c((-2-g), 1, 1, g, 1, (-2-g), g, 1, 1, g, (-2-g), 1, g, 1, 1, (-2-g))
  M <- matrix(data = entries, nrow = 4, ncol = 4, byrow = TRUE)
  return(M)
}
#g is a value passed to a matrix to scale it
g <- 2/3
scaled_mat <- genMatrix(g)
#We must evaluate e^(t*scaled_mat) and maximise this.
trans_prob_P <- expm(t * scaled_mat)
trans_prob_P
#Two genetic sequences
seq1 <- c('A', 'G', 'T', 'C', 'C', 'A', 'T', 'G', 'A', 'T')
seq2 <- c('A', 'C', 'G', 'T', 'C', 'G', 'T', 'G', 'C', 'T')
sum_seq = c()
#Iterate simultaeneously through both sequences
#Then perform addition of their corresponding
#matrix entries in trans_prob_P

convert <- function(a){
  #Takes a nucleotide and convert to its index in the list
  #[1,2,3,4] corresponding to [A,T,C,G]
    if(a == 'A'){
    return(1)
  }
  if(a == 'T'){
    return(2)
  }
  if(a == 'C'){
    return(3)
  }
  else{
    return(4)
  }
}
for(i in 1:10){
  #Convert string elements of sequences to integers
  #Then lookup that index of the exponentiated matrix
  #and add the value for that index to a vector of sums of the 
  #sequence.
  index1 = convert(seq1[i])
  index2 = convert(seq2[i])
  entry = trans_prob_P[index1, index2]
  sum_seq[i] <- entry
}
SumSeq <- function(sum_seq){
  #Iterates through the individual sums from the two sequences
  #Then keeps an aggregating sums, and return the sum of the whole sequence.
  sum = 0
  for(value in sum_seq){
    sum = sum * value
  }
  return(sum)
}
#The returned sum from SumSeq corresponds to the genetic distance
#between the two sequences.
genetic_distance = SumSeq(sum_seq)
return("The genetic distance is: ",genetic_distance)
}

maximise <- optim(par(1,0.5), main,control = list(fnscale=1))
maximise

