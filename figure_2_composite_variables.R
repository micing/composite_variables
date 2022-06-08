library("MASS")

n=3*10^3
r=.70
sigma = matrix(c(1,0,0, 0,1,r, 0,r,1), nrow=3)
mu=c(5,5,5)

#### Generate data ####
set.seed(3)
d=mvrnorm(n, mu, sigma)

a<-d[,2]
b<-d[,1]
y<-d[,3]

#### Plot function ####
plotit <- function(x, y, main="", xlab="", ylab="", legpos="bottomright", bg=NA, cex=1) {
  r=format(round(cor(x,y, use="complete.obs"), 2), nsmall = 2)
  plot(x,y, main=main, ylab=ylab, xlab=xlab)
  legend(legpos, paste0("r=", r), bg=bg, adj=0, box.col=NA, cex=cex, inset=c(0,-.00))
}

#### figure 2 ####
op=par(no.readonly = TRUE)
png(filename = "png/figure_2.png", width = 1300, height = 2000, res=300)
par(mfrow=c(4,2), mar=c(2.1, 2, 1.4, 0.5), cex=.5)

plotit(a, y, main="A")
plotit(b, y, main="B", bg="white")
plotit(a-b, y, main=expression(bold(A-B)))
plotit(b-a, y, main=expression(bold(B-A)), legpos="topright")
plotit(a+b, y, main=expression(bold(A+B)))
plotit(a*b, y, main=expression(bold(A%*%B)))
plotit(a/b, y, main=expression(bold(A/B)))
plotit(b/a, y, main=expression(bold(B/A)), legpos="topright")

par(op)
dev.off()