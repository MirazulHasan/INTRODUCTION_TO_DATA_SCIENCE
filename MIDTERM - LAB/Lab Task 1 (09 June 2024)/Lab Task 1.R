x <- 21 #Variable initialization
y <- 10
z <- 15
#x+y*z
a <- x*y*z

abs(-57)
ceiling(89.1)
floor(84.5)

tolower("HIMEL")
toupper("himel")
strsplit("Mirazul Hasan Himel", split = "0")

str <- "Mirazul Hasan Himel"
nchar(str)

str1 <- "Mirazul"
str2 <- "Hasan"
paste(str1, str2)

str1 <- "Mirazul"
str2 <- "Hasan"
paste(str1, str2, sep="")

str3 <- "Hello, everyone.
This is Mirazul Hasan."

str3

cat(str3)

str4 <- "My name is \"Himel\". I am from Bogura."

cat(str4)

if(x>y){
  print("x is greater than y.")
} else if(x == y) {
  print("x and y are equal.")
} else{
  print("y is greater than x.")
}

switch(1, "Mirazul","Hasan", "Himel", "Red")

switch("Himel", "Mirazul" = 19, "Hasan" = 20, "Himel" = 16)

i <- 1

while (i < 10) {
  print(i)
  i <- i + 1
}

# While Loop With break
j <- 2

while (j < 10) {
  print(j)
  j <- j + 1
  if(j == 6) {
    break
  }
}

# While Loop With Next
k <- 0
while (k < 6) {
  k <- k + 1
  if(k == 3) {
    next
  }
  print(k)
}

# For Loop
for (x in 0:9){
  print(x)
}

# Nested For Loop
for (x in 1:2) {
  for (y in 1:3) {
    print(x*y)
  }
}

# Function

add_numbers <- function(x,y) {
  sum <- x+y
return(sum)
}

print(add_numbers(5,10))
