# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Create figure 1.
#
# Remarks:
#  None.

tau <- 0; # population average of true individual treatment effect
n <- 3; # number of cycles per subject
psi <- 2; # variance of the true indivual treatment effect
sigma <- 1; # residual variance

# probability for the computations
# of the x-axis limits of the plot
prob <- 0.999;

# weight for the shrunk estimate
k <- n*psi/(n*psi+sigma);

# shrunk
a <- qnorm(
  p=(1+c(-1, 1)*prob)/2,
  mean=tau,
  sd=sqrt(k^2*(psi+sigma/n))
);

#naive
b <- qnorm(
  p=(1+c(-1, 1)*prob)/2,
  mean=tau,
  sd=sqrt(psi+sigma/n)
);

# compute x-axis limits
xlim <- (a+b)/2;
xlim[1] <- floor(xlim[1]);
xlim[2] <- ceiling(xlim[2]);

x <- seq(from=xlim[1], to=xlim[2], by=0.01);
mse.naive <- rep( x=sigma/n, times=length(x) );
mse.shrunk <- (1-k)^2*(x-tau)^2+k^2*sigma/n;

tiff(
  filename="MSE_vs_Estimand_A.tif",
  width=1920,
  height=1080,
  units="px",
  pointsize=12,
  compression="lzw",
#  res=96,
  bg="white",
  type="cairo"
);

#jpeg(
#  filename="MSE_vs_Estimand_A.jpg",
#  width=1920,
#  height=1080,
#  units="px",
#  pointsize=12,
#  quality=100,
#  res=96,
#  bg="white",
#  type="cairo"
#);

#png(
#  filename="MSE_vs_Estimand_A.png",
#  width=1920,
#  height=1080,
#  units="px",
#  pointsize=12,
#  res=96,
#  bg="white",
#  type="cairo"
#);

old <- par();

par(
  mar=c(
    7.1, # bottom margin default 5.1
    9.1, # left margin default 4.1
    4.1, # top margin default 4.1
    2.1 # right margin default 2.1
  )
);

plot(
  x=x,
  y=mse.naive,
  xlim=xlim,
  ylim=c(0, 1),
  xlab="",
  ylab="",
  type="l",
  col="black",
  lty=1,
  lwd=1,
  bty="n",
  axes=FALSE
);

# define x-axis
axis(
  side=1,
  at=seq(from=xlim[1], to=xlim[2], by=2),
  labels=TRUE,
  tick=TRUE,
  outer=FALSE,
  lty="solid",
  lwd=1.5,
  lwd.ticks=1.5,
  cex.axis=2.5,
  mgp=c(3, 1, -1)
);

# define y-axis
axis(
  side=2,
  at=seq(from=0, to=1, by=0.2),
  labels=TRUE,
  tick=TRUE,
  outer=FALSE,
  lty="solid",
  lwd=1.5,
  lwd.ticks=1.5,
  cex.axis=2.5,
  mgp=c(3, 1, 0)
);

# define xy-axis label
title(
  xlab=expression(theta),
  ylab=expression( paste("MSE[", hat(theta), "|", theta, "]") ),
  line=5,
  cex.lab=3
);

lines(
  x=x,
  y=mse.shrunk,
  col="black",
  lty=2,
  lwd=1
);

legend(
  x="top",
  legend=c(
    "naive",
    "shrunk"
  ),
  col=c(
    "black",
    "black"
  ),
  lty=1:2,
  lwd=1,
  bty="n",
  cex=2.5,
  xjust=0,
  yjust=0,
  ncol=1,
  horiz=FALSE
);

par(old);

dev.off();
