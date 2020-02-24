# IDEAL project
# http://www.ideal.rwth-aachen.de/
#
# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Creates figure 1.
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

# naive
b <- qnorm(
  p=(1+c(-1, 1)*prob)/2,
  mean=tau,
  sd=sqrt(psi+sigma/n)
);

# compute x-axis limits
xlim <- (a+b)/2;
xlim[1] <- floor(xlim[1]);
xlim[2] <- ceiling(xlim[2]);

# second y-axis limits
density.at <- pretty( x=c( 0, 1/sqrt(2*pi*psi) ) );
density.ylim <- range(density.at);

x <- seq(from=xlim[1], to=xlim[2], by=0.01);
mse.naive <- rep( x=sigma/n, times=length(x) );
mse.shrunk <- (1-k)^2*(x-tau)^2+k^2*sigma/n;
density.x <- dnorm( x=x, mean=tau, sd=sqrt(psi) );

tiff(
  filename="Figure1.tif",
  width=1920,
  height=1080,
  units="px",
  pointsize=2,
  compression="lzw",
  res=400,
  bg="white",
  type="cairo"
);

#jpeg(
#  filename="Figure1.jpg",
#  width=1920,
#  height=1080,
#  units="px",
#  pointsize=2,
#  quality=100,
#  res=400,
#  bg="white",
#  type="cairo"
#);

#png(
#  filename="Figure1.png",
#  width=1920,
#  height=1080,
#  units="px",
#  pointsize=2,
#  res=400,
#  bg="white",
#  type="cairo"
#);

old <- par();

par(
  mar=c(
    7.1, # bottom margin default 5.1
    9.1, # left margin default 4.1
    4.1, # top margin default 4.1
    9.1 # right margin default 2.1
  )
);

# plot MSE naive
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
  lwd=0.75,
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
  lwd=1.25,
  lwd.ticks=1.25,
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
  lwd=1.25,
  lwd.ticks=1.25,
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

# plot MSE shrunk
lines(
  x=x,
  y=mse.shrunk,
  col="black",
  lty=2,
  lwd=0.75
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
  lwd=0.75,
  bty="n",
  cex=2.5,
  xjust=0,
  yjust=0,
  ncol=1,
  horiz=FALSE
);

# allow second plot
par(new=TRUE);

# plot density
plot(
  x=x,
  y=density.x,
  xlim=xlim,
  ylim=density.ylim,
  xlab="",
  ylab="",
  type="l",
  col="red",
  lty=1,
  lwd=0.75,
  bty="n",
  axes=FALSE
);

# define second y-axis
axis(
  side=4,
  at=density.at,
  labels=TRUE,
  tick=TRUE,
  outer=FALSE,
  lty="solid",
  lwd=1.25,
  lwd.ticks=1.25,
  col="red",
  col.ticks="red",
  col.axis="red",
  cex.axis=2.5,
  mgp=c(3, 2, 0)
);

# define second y-axis label
mtext(
  text="Probability density",
  side=4,
  line=6,
  outer=FALSE,
  cex=3,
  col="red"
);

par(old);

dev.off();
