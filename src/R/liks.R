function () {

a.set <- read.csv("./20180327_2/a_set_summary.csv", header = 1)[,1]
b.set <- read.csv("./20180327_2/b_set_summary.csv", header = 1)[,1]

filename <- "ab_set_summary.png"
png(filename, width=960*2, height=960, res=150)
par(mfcol=c(1,2))
  
hist(a.set,breaks=seq(0,ceiling(max(a.set)),by=0.1),xlim=c(0,ceiling(max(a.set))), xlab="wait time a", freq = F)
qt <- quantile(a.set, prob=c(0.025,0.975))
text(par("usr")[2], par("usr")[4], adj=c(1,1), cex=1.2, labels=sprintf("95%% CrI: %.3f-%.3f", qt[1],qt[2]))
  
hist(b.set,breaks=seq(0,ceiling(max(b.set)),by=0.1),xlim=c(0,ceiling(max(b.set))), xlab="wait time b", freq = F)
qt <- quantile(b.set, prob=c(0.025,0.975))
text(par("usr")[2], par("usr")[4], adj=c(1,1), cex=1.2, labels=sprintf("95%% CrI: %.3f-%.3f", qt[1],qt[2]))
dev.off()

} #comment out

r3 <- read.csv("./real/realsize_h3_3.csv", header = 1)[,2]
r4 <- read.csv("./real/realsize_h3_4.csv", header = 1)[,2]
r5 <- read.csv("./real/realsize_h3_5.csv", header = 1)[,2]

f3 <- read.csv("./sim/e2_foi2_ns3.csv", header = 1)[,2]
f4 <- read.csv("./sim/e2_foi2_ns4.csv", header = 1)[,2]
f5 <- read.csv("./sim/e2_foi2_ns5.csv", header = 1)[,2]

liks <- function (simvec, realvec) {
  res <- c()
  for (i in min(simvec):max(simvec)) {
    res <- c(res, c(lik(length(simvec[simvec == i]), length(realvec[realvec == i]))))
  }
  return (sum(res))
}

lik <- function (sim, real) {
  return (real * log(sim/256))
}

res3 <- liks(f3, r3)
res4 <- liks(f4, r4)
res5 <- liks(f5, r5)

print(res3)
print(res4)
print(res5)
print(sum(res3,res4,res5))
