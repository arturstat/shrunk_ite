# IDEAL project
# http://www.ideal.rwth-aachen.de/
#
# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Creates figure 4.
#
# Remarks:
#  Needs several hours to complete.

iseed <- 3141593; # seed for RNG

nsim <- 1e6; # number of simulations

tau <- 0; # population average of true individual treatment effect
n <- 3; # number of crossovers per subject
sigma <- 1; # residual variance
w <- c(0.5, 0.6, 0.75, 0.85, 0.9, 0.95, 0.99); # weights
# variance of the true individual treatment effect
psi <- w/(1-w)*sigma/n;
# number of n-of-1 trials
s <- c(2:8, 10, 14, 20, 30, 40, 50, 70, 100, 300, 1000);

files <- c(
  "boot.statistic.R", "bootR.R",
  "checkArgs.boot.R", "checkArgs.randomIntercepts.R",
  "checkObject.statistic.R", "dpill.R",
  "estimateITE.lme.randomIntercepts.R", "lmeModel.R",
  "lmeStatistic.R", "lmeStatistic.randomIntercepts.R",
  "locpoly.R", "naive.R", "newFrame.R",
  "randomIntercepts.R", "shrunk.R", "squareError.R",
  "statistic.args.R", "statistic.dots.R"
);

# define working directory
while ( !all( files %in% list.files(path="./include") ) ) {
  file <- file.choose();# choose this file
  WorkingDir  <- dirname(file);# get path to file
  setwd(dir=WorkingDir); # define working directory
  rm(file, WorkingDir); # remove objects
}

# query working directory
#getwd();

# list files in working directory
#list.files();

# get list of files to source
files <- list.files(path="./include");

# source files
for (file in files) {
  source(
    file=paste("./include/", file, sep="")
  );
}
rm(file);

# create parallel cluster
cl <- parallel::makeCluster(
  spec=parallel::detectCores(), type="PSOCK"
);

# export list of files to cluster
parallel::clusterExport(
  cl=cl, varlist=c("files"), envir=environment()
);

# source files in cluster
invisible(
  parallel::clusterEvalQ(
    cl=cl,
    expr={
      for (file in files) {
        source(
          file=paste("./include/", file, sep="")
        );
      }
      rm(files, file);
    }
  )
);

rm(files);

# define simulation list
sim.data <- vector( mode="list", length=length(psi) );

system.time(
  expr={
    for ( i in 1:length(psi) ) {
      sim.data[[i]] <- vector(mode="list", length=3);
      sim.data[[i]][[1]] <- psi[i]; # individual treatment effect variance
      sim.data[[i]][[2]] <- psi[i]/(psi[i]+sigma/n); # naive mean weight
      sim.data[[i]][[3]] <- data.frame(
        s=vector( mode="numeric", length=length(s) ), # number of n-of-1 trials
        mse=vector( mode="numeric", length=length(s) ), # Mean Squared Error
        se.mse=vector( mode="numeric", length=length(s) ) # Monte Carlo standard error of MSE
      );
      names(sim.data[[i]]) <- c("psi", "k", "loss.data");
      
      # set seed on parallel cluster
      parallel::clusterSetRNGStream(cl=cl, iseed=iseed);
      
      for( j in 1:length(s) ) {
        # set seed on parallel cluster
        parallel::clusterSetRNGStream(cl=cl, iseed=iseed);
        
        boot.data <- bootR(
          func=randomIntercepts, # simulation function
          args=list(
            size=as.integer( rep(x=n, times=s[j]) ),
            fixed=tau,
            random=list(psi=psi[i], sigma=sigma),
            model=list()
          ), # list of arguments to simulation function
          stat=shrunk, # statistic function
          nsim=ceiling( nsim/( s[j]*length(cl) ) ), # number of simulations
          cl=cl # parallel cluster
        );
        
        sim.data[[i]]$loss.data$s[j] <- s[j]; # save number of n-of-1 trials
        sim.data[[i]]$loss.data$mse[j] <- mean(boot.data@measure[,1]); # compute MSE
        sim.data[[i]]$loss.data$se.mse[j] <- # compute Monte Carlo standard error of MSE
          sqrt( var(boot.data@measure[,1])/length(boot.data@measure[,1]) );
      }
    }
  }
);

parallel::stopCluster(cl=cl);

# compute maximum Monte Carlo standard error of MSE
se.max <- max(sim.data[[1]]$loss.data$se.mse);
for ( i in 2:length(sim.data) ) {
  se.max <- max(se.max, sim.data[[i]]$loss.data$se.mse)
}
print(se.max);

##########################
###### Plot results ######
##########################

pch <- c(3, 4, 7, 25, 9, 10, 24);
col <- c(
  "red", "green", "orange",
  "purple", "blue", "magenta", "cyan"
);

tiff(
  filename="Figure4.tif",
  width=3840,
  height=2160,
  units="px",
  pointsize=2,
  compression="lzw",
  res=800,
  bg="white",
  type="cairo"
);

old <- par();

par(
  mar=c(
    8.1, # bottom margin default 5.1
    9.1, # left margin default 4.1
    2.1, # top margin default 4.1
    1.1 # right margin default 2.1
  )
);

plot(
  x=log10(sim.data[[1]]$loss.data$s),
  y=sim.data[[1]]$loss.data$mse*n/sigma,
  xlim=range( log10(s) ),
  ylim=c(0.4, 1.25),
  xlab="",
  ylab="",
  type="p",
  pch=pch[1],
  col=col[1],
  cex=2,
  bty="n",
  axes=FALSE
);

# define x-axis
axis(
  side=1,
  at=log10(s),
  labels=s,
  tick=TRUE,
  outer=FALSE,
  lty="solid",
  lwd=1.5,
  lwd.ticks=1.5,
  cex.axis=2.25,
  mgp=c(-3, 2, -1)
);

# define y-axis
axis(
  side=2,
  at=seq(from=0.4, to=1.2, by=0.2),
  labels=TRUE,
  tick=TRUE,
  outer=FALSE,
  lty="solid",
  lwd=1.5,
  lwd.ticks=1.5,
  cex.axis=2.25,
  mgp=c(3, 2, 0)
);

# define xy-axis label
title(
  xlab="number of subjects",
  ylab="relative efficiency",
  line=6,
  cex.lab=3
);

for ( i in 2:length(sim.data) ) {
  points(
    x=log10(sim.data[[i]]$loss.data$s),
    y=sim.data[[i]]$loss.data$mse*n/sigma,
    type="p",
    pch=pch[i],
    col=col[i],
    cex=2
  );
}

# plot equality line
abline(
  h=1,
  lty=2,
  lwd=0.5
);

legend(
  x="topright",
  legend=round(
    x=psi/(psi+sigma/n),
    digits=4
  ),
  col=col,
  pch=pch,
  bty="n",
  cex=2.5,
  pt.cex=2,
  xjust=0,
  yjust=0,
  ncol=1,
  horiz=FALSE,
  title="weight",
  title.col="black",
  title.adj=0.75
);

#mtext(
#  text=paste("n=", n, sep=""),
#  side=3,
#  line=0,
#  outer=FALSE,
#  cex=2
#);

text(
  x=log10(2.75),
  y=0.5,
  labels="favors shrunk",
  cex=2.5,
  col="black"
);

text(
  x=log10(2.75),
  y=1.15,
  labels="favors naive",
  cex=2.5,
  col="black"
);

par(old);

dev.off();

save.image(file="./Figure4.RData");
