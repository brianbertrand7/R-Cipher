# Final R Project
# Brian Bertrand and Daniel Meaney
# We created a Hill Cipher Encoder, which combines cryptography as well as Multiplication
# POINT- we discussed cryptography in CS50
# POINT- crytography and matrix multiplication are two related but distinct topics

library("gtools")  #necessary for the ascii operations
library("RConics")  #necessary for the ascii operations
# POINT- encorporating a new R library/package

# Use readline function to prompt user for plaintext
# POINT - this is an R function that has not appeared in any Math 23A script
plaintext <- readline(prompt="Enter plaintext: ")

# Function to convert string into vector with numeric values represented by
# numbers ranging from 0-25, the spaces are however, not included.
# 1/2 POINT- creating our own function
hash <- function(string) {
  plains <- asc(string)  # Turn the string into a list of corresponding ascii numbers
  n <- vector(); n #declare an empty vector
  # Iterate through each element of the list and add it to the vector
  for (x in 1:(length(plains))) {
    # If it is a lowercase letter subtract by 97 to bring it down to the range of 0 to 25
    if (plains[x] >= 97 && plains[x] <= 122){
      plains[x] <- plains[x] - 97
      # Append to the vector
      n <- c(n, plains[x])
    }
    # If it is a uppercase letter subtract by 65 to bring it down to the range of 0 to 25
    else if (plains[x] >= 65 && plains[x] <= 90){
      plains[x] <- plains[x] - 65
      # Append to the vector
      n <- c(n, plains[x])
    }
    else if (plains[x] == 32){
      plains[x] <- plains[x] - 6
      # Append to the vector
      n <- c(n, plains[x])
    }
    else if (plains[x] == 39){
      plains[x] <- plains[x] - 12
      # Append to the vector
      n <- c(n, plains[x])
    }
    else if (plains[x] == 44){
      plains[x] <- plains[x] - 16
      # Append to the vector
      n <- c(n, plains[x])
    }
  }
  # Return the new vector
  return(n)
}

# Call the hashed and numerized plaintext plains
plains <- hash(plaintext); plains

# Pad plains so that it can be divided into 3x1 matrices

x <- length(plains); x
b <- x %% 5; b
# If the remainder is one add one zero to the end
if (x %% 5 == 4){
  plains[x+1] <- 26
# If the remainder is 2 add two zeros at the end
} else if (x %% 5 == 3) {
  plains[x+1] <- plains[x+2] <- 26
# Else do nothing
}else if (x %% 5 == 2) {
  plains[x+1] <- plains[x+2] <- plains[x+3]<- 26
  # Else do nothing
}else if (x %% 5 == 1) {
  plains[x+1] <- plains[x+2] <- plains[x+3] <- plains[x+4] <- 26
  # Else do nothing
} else {
}
plains

# Figure out how many 3x1 columns are needed to split the plaintext into
columns <- length(plains)/5; columns
# Declare the A matrix to have that many columns
A <- matrix(, nrow = 5, ncol = columns)
counter <- 1

# For loop through the columns, appending each to the A matrix
for (j in 1:columns) {
  for (i in 1:5) {
    A[i,j] = plains[counter]
    counter <- counter + 1
  }
}
print(A)

# Function to generate a random, n x n matrix
# POINT Random number Generator and For Loop
randmatrix <- function(n) {
  A <- matrix(, nrow = n, ncol = n); key
  
  for(x in 1:n){
    for(y in 1:n){
      A[x,y] <- sample(0:28,1)
    }
  }
  return(A)
}

# Randomly fill the key matrix
key <- randmatrix(5)
print(key)

# Multiply the Key matrix by the Plaintext matrix
cipher <- (key%*%A) %% 29; cipher

# Create a vector the length of the plaintext
ciphertext <- vector(, nchar(plaintext)); ciphertext
counter <- 1

# Turn the Matrix containing the newly encoded words back into a singular vector
for (i in 1:columns) {
  for (j in 1:5) {
    if (cipher[j,i] == 27){
      ciphertext[counter] <- cipher[j,i] + 67
      counter <- counter + 1
    }
    else{
    ciphertext[counter] <- cipher[j,i] + 65
    counter <- counter + 1
    }
  }
}
print(ciphertext)

# turn the ciphertext vector back into letters
ciphertext <- chr(ciphertext); ciphertext
# join the vector into a string
ciphertext <- toString(ciphertext); ciphertext
#take away all the punctuation and show the ciphertext without interruption
ciphertext <- gsub(", ", "", ciphertext); ciphertext
