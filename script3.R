qx <- read.table("clipboard",sep = "\t",header = FALSE,dec = ",")
qx <- qx[,1]
interes <- 0.04

lx <- c(1:length(qx))
dx <- c(1:length(qx))
lx[1]<-1000000



tabla <- data.frame(prob_qx=qx)
tabla$pep <- 1:101
tabla$pep <- NULL
tabla$prob_px <- 1-qx
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
v <- 1/(1+interes)
tabla$D_x <- (v^tabla$x)*tabla$l_x
tabla$N_x <- c(1:length(qx))
tabla$S_x <- c(1:length(qx))
for (i in 1:length(qx)){
  tabla$N_x[i] <- sum(tabla$D_x[c(i:length(qx))])
}
tabla$N_x <- round(tabla$N_x,digits = 2)
for (i in 1:length(qx)){
  tabla$S_x[i] <- sum(tabla$N_x[c(i:length(qx))])
}
tabla$S_x <- round(tabla$S_x,digits = 2)
tabla$C_x <- (v^tabla$x)*tabla$d_x
tabla$M_x <- c(1:length(qx))
tabla$R_x <- c(1:length(qx))
for (i in 1:length(qx)){
  tabla$M_x[i] <- sum(tabla$C_x[c(i:length(qx))])
}
tabla$M_x <- round(tabla$M_x,digits = 2)
for (i in 1:length(qx)){
  tabla$R_x[i] <- sum(tabla$M_x[c(i:length(qx))])
}
tabla$R_x <- round(tabla$R_x,digits = 2)



