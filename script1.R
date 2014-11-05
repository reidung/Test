###################
#Before starting
###################

# Open Notepad for writing/reading the script.
# Open an R window above. File/Change dir: open the directive Verktøy/R/scripts/.

rm(list=ls())

#########################
#Creation of a data set
#########################

#matrix of values
data_init<-rnorm(n=1000,mean=0,sd=1) # different probability function
# Checking if it is a vector or matrix: if NULL it is a vector.
dim(data_init)
# transform the vector into a matrix:
dim(data_init)<-c(100,10) # rows=100, columns=10 ->100 time steps and 10 stations

# plotting the matrix:
image(data_init)

#vector of character
vector_date<-vector(length=nrow(data_init))
# %Y means Year with century etc.  Evt ("1998-12-04-10:00","%Y-%m-%d-%H:%M")
date_debut<-strptime("1998-12-04-10:00", "%Y-%m-%d-%H:%M") 
for (i_date in 1:(length(vector_date)))
{
   vector_date[i_date]<-as.character(date_debut+3600*i_date)
}

#our data set (if you want daily, multiply with four)
dataset<-cbind(vector_date,data_init)
dataset[1,]
dataset[,2]

#recording file
date_debut_file<-sub('0000','00',sub(':', '',sub(':', '',sub(' ', '',sub('-', '',sub('-', '',date_debut))))))
date_fin_file<-sub('0000','00',sub(':', '',sub(':', '',sub(' ', '',sub('-', '',sub('-', '',vector_date[length(vector_date)]))))))
filename<-paste("../data/",date_debut_file,"-",date_fin_file,".txt",sep="")

write.table(dataset,filename,row.names=FALSE,col.names=FALSE,sep=" ")

###############
#Reading data
###############
data <- read.table(filename,header=FALSE,sep=" ",blank.lines.skip=FALSE)#,na.string="-99.000")
nstep <- nrow(data)
nstat <- ncol(data)-1

matrix_data <- matrix(0,nc=nstat,nr=nstep)
date <- vector(length=nstep)

for (j in 1:nstat) {
      matrix_data[1:nstep,j]=data[1:nstep,j+1]
}

date <- data[1:nrow(data),1]

###############
#Analyse data
###############
Mean<-mean(matrix_data)

#variance of each station
sd(matrix_data)

#global variance
tmp<-matrix_data
dim(tmp)<-NULL
Variance<-sd(tmp)

#Experimental cumulated probability function
#For station #3
ExpCpd<-matrix(0,nstep,3) #freq val ,time step
ExpCpd[,2]<-matrix_data[,3]   
ExpCpd[,3]<-seq(1,nstep)
ExpCpd<-ExpCpd[sort.list(ExpCpd[,2]),]
for (i_step in 1:(nstep)) {
   ExpCpd[i_step,1]<-(i_step-0.5)/(nstep)
} 

#Theoretical Normal cpd
erf <- function(x) {
  2 * pnorm(x * sqrt(2)) - 1
}
cpdNorm <- function (x,p) {
  m <- p[1]
  v <- p[2] 
  (1/2)*(1+erf((x-m)/sqrt(2*v)))     
}

min_quantile<-min(matrix_data)
max_quantile<-max(matrix_data)
quantile_values<-seq(min_quantile,max_quantile,0.01) 
cpd_values<-cpdNorm(quantile_values,c(Mean,Variance))


png("../results/Cpd_Theoretical_And_Experimental.png")

plot(quantile_values, cpd_values,col=1,type="l",lty=1,axes=F,xlim=c(min(quantile_values),max(quantile_values)),ylim=c(0,1.2), xlab="", ylab="")
# Draw the y-axis
axis(2, ylim=c(0,1))
mtext(expression(Probability),side=2,line=2.5)
# Draw the x-axis
axis(1,xlim=c(min(quantile_values),max(quantile_values)))
mtext(expression(quantile),side=1,col="black",line=2.5)

#bew plot
par(new=T)

plot(ExpCpd[,2],ExpCpd[,1],axes=F,xlim=c(min(quantile_values),max(quantile_values)),ylim=c(0,1.2), xlab="", ylab="")

dev.off()