Z <- matrix(1:20, nrow = 4, ncol = 5, byrow = T); Z
apply(Z, 1, sum)
apply(Z, 2, sum)

A <- 1:12; dim(A) = c(3,4); A
B <- 13:24; dim(B) = c(3,4); B
X1 <- rbind(A, B); X1
X2 <- cbind(A, B); X2

A <- array(c(10,2,1,3,-10,3,1,3,10), dim = c(3,3)); A
b <- array(c(14,-5,14), dim = c(3,1)); b
solve(A)
solve(A, b)

Z <- matrix(1:30, nrow = 5, ncol = 6, byrow = T); Z <- data.frame(Z); Z

getwd()
setwd("D:/杂物箱/计算机作业/R")
write(A, file = "data")

write.table(Z, file = "number.txt")

rd1 <- read.delim("temp.txt", fileEncoding = "Windows-1252")  # 读取ANSI编码（不建议）
rd1 <- read.delim("temp1.txt")  # Attention: Remember to change the code form to UTF-8 in Notepad
rd2 <- read.csv("temp1.csv")
head(rd1)
head(rd2)