library("MASS")

n=3*10^3
r=.70
Sigma = matrix(c(1,0,0, 0,1,r, 0,r,1), nrow=3)
mu=c(5,5,5)

#### Genrate data ####
set.seed(3)
d=mvrnorm(n, mu, Sigma)

a<-d[,2]
b<-d[,1]
y<-d[,3]

#### Plot function ####
plotit <- function(x, y, main="", xlab="", ylab="", legpos="bottomright", bg=NA, cex=.8) {
  r=format(round(cor(x,y, use="complete.obs"), 2), nsmall = 2)
  plot(x,y, main=main, ylab=ylab, xlab=xlab)
  legend(legpos, paste0("r=", r), bg=bg, adj=0, box.col=NA, cex=cex, inset=c(0,-.00))
}

#### figure 2 ####
op=par(no.readonly = TRUE)
par(mfrow=c(4,2), mar=c(2.1, 2, 1.4, 0.5), cex=1)

plotit(a, y, main="A")
plotit(b, y, main="B", bg="white")
plotit(a-b, y, main=expression(bold(A-B)))
plotit(b-a, y, main=expression(bold(B-A)), legpos="topright")
plotit(a+b, y, main=expression(bold(A+B)))
plotit(a*b, y, main=expression(bold(A%*%B)))
plotit(a/b, y, main=expression(bold(A/B)))
plotit(b/a, y, main=expression(bold(B/A)), legpos="topright")

par(op)

#### figure 3 ####
## Data can be downloaded here: https://wwwn.cdc.gov/nchs/nhanes/Search/DataPage.aspx?Component=Questionnaire&CycleBeginYear=2007

data=read.csv("ICPSR_25505/DS0012/25505-0012-Data.tsv", sep="\t")

dat=data.frame(age=data$RIDAGEYR, female=data$RIAGENDR==2, kg=data$BMXWT, m=data$BMXHT/100)
dat$bmi=dat$kg/dat$m^2

d=subset(dat, dat$age > 18)
males=subset(d, d$female==FALSE)
females=subset(d, d$female==TRUE)

op=par(no.readonly = TRUE)
par(mfrow=c(3,2), mar=c(3, 3, 2, 1), mgp=c(1.9,0.8,0), cex=1)

plotit(males$m,males$kg, main="Males", xlab="Height (m)", ylab="Weight (kg)", legpos="topright")
plotit(females$m,females$kg, main="Females", xlab="Height (m)", ylab="Weight (kg)", legpos="topright")
plotit(males$bmi,males$m, main="BMI and height",  xlab=bquote(paste("Body Mass Index (", kg/m^2 , ")")), ylab="Height (m)", , legpos="topright")
plotit(females$bmi,females$m, main="BMI and height",  xlab=bquote(paste("Body Mass Index (", kg/m^2 , ")")), ylab="Height (m)", legpos="topright")
plotit(males$bmi,males$kg, main="BMI and weight",  xlab=bquote(paste("Body Mass Index (", kg/m^2 , ")")), ylab="Weight (kg)")
plotit(females$bmi,females$kg, main="BMI and weight",  xlab=bquote(paste("Body Mass Index (", kg/m^2 , ")")), ylab="Weight (kg)")

par(op)

