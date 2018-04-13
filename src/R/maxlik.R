a.set <- read.csv("a_set_summary.csv", header = 1)[,1]
a.set <- a.set[ a.set > 0 ]
b.set <- read.csv("b_set_summary.csv", header = 1)[,1]
b.set <- b.set[ b.set > 0 ]
a.set.p <- read.csv("a_set_prior.csv", header = 1)[,1]
a.set.p <- a.set.p[ a.set.p > 0 ]
b.set.p <- read.csv("b_set_prior.csv", header = 1)[,1]
b.set.p <- b.set.p[ b.set.p > 0 ]

neglik.a <- function(par.a) {
  shape <- par.a[1]
  scale <- par.a[2]
  liks.a <<- dgamma(a.set, shape = shape, scale = scale, log=T)
  return (-sum(liks.a))
}

neglik.b <- function(par.b) {
  shape <- par.b[1]
  scale <- par.b[2]
  liks.b <<- dgamma(b.set, shape = shape, scale = scale, log=T)
  return (-sum(liks.b))
}

neglik.ap <- function(par.ap) {
  shape <- par.ap[1]
  scale <- par.ap[2]
  liks.ap <<- dgamma(a.set.p, shape = shape, scale = scale, log=T)
  return (-sum(liks.ap))
}

neglik.bp <- function(par.bp) {
  shape <- par.bp[1]
  scale <- par.bp[2]
  liks.bp <<- dgamma(b.set.p, shape = shape, scale = scale, log=T)
  return (-sum(liks.bp))
}

opt.a <- optim(c(3,7), neglik.a, lower = c(1,0.1), upper=c(10,10))
opt.b <- optim(c(3,7), neglik.b, lower = c(1,0.1), upper=c(10,10))
opt.ap <- optim(c(3,7), neglik.ap, lower = c(1,0.1), upper=c(10,10))
opt.bp <- optim(c(3,7), neglik.bp, lower = c(1,0.1), upper=c(10,10))

a.bin <- seq(min(a.set), max(a.set), by=0.01)
b.bin <- seq(min(b.set), max(b.set), by=0.01)
ap.bin <- seq(min(a.set.p), max(a.set.p), by=0.01)
bp.bin <- seq(min(b.set.p), max(b.set.p), by=0.01)

a.gamma <- dgamma(a.bin, shape = opt.a$par[1], scale = opt.a$par[2])
b.gamma <- dgamma(b.bin, shape = opt.b$par[1], scale = opt.b$par[2])

a.gamma.p <- dgamma(ap.bin, shape=opt.ap$par[1], scale=opt.ap$par[2])
b.gamma.p <- dgamma(bp.bin, shape=opt.bp$par[1], scale=opt.bp$par[2])

#a.gamma.p <- dgamma(a.bin, shape=1.5, scale=0.8)
#b.gamma.p <- dgamma(b.bin, shape=1.5, scale=0.8)

filename <- "ab_set_summary_with_gamma.png"
png(filename, width=960*2, height=960, res=150)
par(mfcol=c(1,2))

hist(a.set,breaks=seq(0,ceiling(max(a.set)),by=0.1),xlim=c(0,ceiling(max(a.set))), xlab="wait time a", freq=F)
qt <- quantile(a.set, prob=c(0.025,0.975))
text(par("usr")[2], par("usr")[4], adj=c(1,1), cex=1.2, labels=sprintf("95%% CrI: %.3f-%.3f", qt[1],qt[2]))
points(a.bin, a.gamma, type="l",col="blue")
points(ap.bin, a.gamma.p, type="l",col="red")

hist(b.set,breaks=seq(0,ceiling(max(b.set)),by=0.1),xlim=c(0,ceiling(max(b.set))), xlab="wait time b", freq=F)
qt <- quantile(b.set, prob=c(0.025,0.975))
text(par("usr")[2], par("usr")[4], adj=c(1,1), cex=1.2, labels=sprintf("95%% CrI: %.3f-%.3f", qt[1],qt[2]))
points(b.bin, b.gamma, type="l",col="blue")
points(bp.bin, b.gamma.p, type="l",col="red")

dev.off()

# tests
#p <- pgamma(xs, shape=4, scale=0.6)
#d <- dgamma(xs, shape=4, scale=0.6)
#plot(xs,p,type="l", col="blue")
#points(xs,d,type="l", col="red")

