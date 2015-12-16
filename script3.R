qx <- read.table("clipboard",sep = "\t",header = FALSE,dec = ",")
qx <- qx[,1]


tabla <- data.frame(prob_qx=qx)
tabla$pep <- 1:101
tabla$pep <- NULL
tabla$prob_px <- 1-qx
lx <- c(1:length(qx))
dx <- c(1:length(qx))


lx[1]<-1000000


for (i in 1:length(qx)){
  dx[i] <- lx[i]*qx[i]
  lx[i+1] <- lx[i]-dx[i]
}
lx <- lx[1:length(qx)]
lx <-round(lx,digits = 0)
dx <-round(dx,digits = 0)
tabla$l_x <- lx
tabla$d_x <- dx
tabla$x <- c(0:(length(qx)-1))


