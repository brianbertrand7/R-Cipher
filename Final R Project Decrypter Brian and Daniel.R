# Final R Project
# Brian Bertrand and Daniel Meaney
# We created a Hill Cipher Encoder, which combines cryptography as well as Multiplication
# POINT- we discussed cryptography in CS50
# POINT- crytography and matrix multiplication are two related but distinct topics

library("gtools")  #necessary for the ascii operations
library("RConics")  #necessary for the ascii operations

#Input ciper text
ciphertext <- readline(prompt="Enter ciphertext: ")

#Create the Key Matrix through inputting
key <- matrix(c(21,27,28,15,26,5,14,7,7,12,21,9,13,0,13,2,1,13,14,17,6,2,6,21,23), nrow = 5, ncol = 5); key
  
plains <- asc(ciphertext); plains
columns <- length(plains)/5; columns

det(key)
detmod <- det(key) %% 29; detmod

da <- 0
repeat {
  da <- da+1
  ab <- da*detmod
  abc <- round((ab %% 29), digits = 0)
  if (abc == 1){
    b <- da
    break
  }
  if (da >= 29){
    break
  }
}
b 
keyp2 <- adjoint(key) %% 29; keyp2
keyinv <- round((b*keyp2) %% 29, digits = 1); keyinv

n <- vector(); n #declare an empty vector
# Iterate through each element of the list and add it to the vector
for (x in 1:(length(plains))) {
  if (plains[x] == 94){
    plains[x] <- plains[x] - 67
    n <- c(n, plains[x])
  }
  else{
    plains[x] <- plains[x] - 65
    # Append to the vector
    n <- c(n, plains[x])
  }
}
n
# Declare the B matrix to have that many columns
B <- matrix(, nrow = 5, ncol = columns)
counter <- 1
# For loop through the columns, appending each to the A matrix
for (j in 1:columns) {
  for (i in 1:5) {
    B[i,j] = plains[counter]
    counter <- counter + 1
  }
}
print(B)
# Multiply the Key matrix by the Plaintext matrix
cipher <- (keyinv%*%B) %% 29; cipher

# Create a vector the length of the plaintext
ciphertext <- vector(, nchar(plaintext)); ciphertext
counter <- 1

# Turn the Matrix containing the newly encoded words back into a singular vector
for (i in 1:columns) {
  for (j in 1:5) {
    if (cipher[j,i] > 25 && cipher[j,i] < 27){
      ciphertext[counter] <- cipher[j,i] + 6
      counter <- counter + 1
    }
    else if (cipher[j,i] > 26 && cipher[j,i] < 28){
      ciphertext[counter] <- cipher[j,i] + 12
      counter <- counter + 1
    }
    else if (cipher[j,i] > 27 && cipher[j,i] < 29){
      ciphertext[counter] <- cipher[j,i] + 16
      counter <- counter + 1
    }
    else {
      ciphertext[counter] <- cipher[j,i] + 65
      counter <- counter + 1
    }
  }
}
print(ciphertext)

# turn the ciphertext vector back into letters
ciphertext <- chr(ciphertext)
# join the vector into a string
ciphertext <- toString(ciphertext)
#take away all the punctuation and show the ciphertext without interruption
ciphertext <- gsub(", ", "", ciphertext); ciphertext

