# SOURCE 
install.packages("extRemes")

# ====== 1 - Libraries and functions =====
library(extRemes)
source("/maxBlock.R")
source("/EVCd.R")


# ================= 2 - Data ========

# cnt
# F01: Eth off, wifi on, core 0
D11 <- read.csv("/cnt/cnt_with_wifi_1.csv", header=TRUE, sep=";")
D21 <- read.csv("/cnt/cnt_with_wifi_2.csv", header=TRUE, sep=";")
D31 <- read.csv("/cnt/cnt_with_wifi_3.csv", header=TRUE, sep=";")
D41 <- read.csv("/cnt/cnt_with_wifi_4.csv", header=TRUE, sep=";")
D51 <- read.csv("/cnt/cnt_with_wifi_5.csv", header=TRUE, sep=";")



# ============= GEV MLE Block ========
# adjust block size
# X is a vector of x indices that contains the maximums
# considering blocks of size n
# X[x] are the values ​​of the maxima.

block_size = 100

fbD11 <- fevd(D11$CYCLES[maxBlock(D11$CYCLES,block_size)], units = "deg C")
fbD21 <- fevd(D21$CYCLES[maxBlock(D21$CYCLES,block_size)], units = "deg C")
fbD31 <- fevd(D31$CYCLES[maxBlock(D31$CYCLES,block_size)], units = "deg C")
fbD41 <- fevd(D41$CYCLES[maxBlock(D41$CYCLES,block_size)], units = "deg C")
fbD51 <- fevd(D51$CYCLES[maxBlock(D51$CYCLES,block_size)], units = "deg C")


# ======  GEV L-Moments Block ========
block_size = 100

flmbD11 <- fevd(D11$CYCLES[maxBlock(D11$CYCLES,block_size)], method = "Lmoments")
flmbD21 <- fevd(D21$CYCLES[maxBlock(D21$CYCLES,block_size)], method = "Lmoments")
flmbD31 <- fevd(D31$CYCLES[maxBlock(D31$CYCLES,block_size)], method = "Lmoments")
flmbD41 <- fevd(D41$CYCLES[maxBlock(D41$CYCLES,block_size)], method = "Lmoments")
flmbD51 <- fevd(D51$CYCLES[maxBlock(D51$CYCLES,block_size)], method = "Lmoments")


# ====== Plot GEV Block ========
par(mfrow=c(2,3))
plot(fbD11,type = "qq", main = "Sample 01")
plot(fbD21,type = "qq", main = "Sample 02")
plot(fbD31,type = "qq", main = "Sample 03")
plot(fbD41,type = "qq", main = "Sample 04")
plot(fbD51,type = "qq", main = "Sample 05")
mtext("F01", side = 3, line = -2, outer = TRUE)

# ====== Plot GEV L-Moments - Block ========
par(mfrow=c(2,3))
plot(flmbD11,type = "qq", main = "Sample 01")
plot(flmbD21,type = "qq", main = "Sample 02")
plot(flmbD31,type = "qq", main = "Sample 03")
plot(flmbD41,type = "qq", main = "Sample 04")
plot(flmbD51,type = "qq", main = "Sample 05")
mtext("F01", side = 3, line = -2, outer = TRUE)


# ======== 5-GPD =============

# Choose threshold
quantile(D21$CYCLES)
limit=320000
fPD <- fevd(D21$CYCLES, type="GP", threshold = limit)
plot(fPD, type="qq")

limithigh=325000
threshrange.plot(D21$CYCLES, r=c(limit,limithigh), type="GP", nint=15)

# trying to fit
fPD11 <- fevd(D11$CYCLES, type="GP", threshold = limit)
fPD21 <- fevd(D21$CYCLES, type="GP", threshold = limit)
fPD31 <- fevd(D31$CYCLES, type="GP", threshold = limit)
fPD41 <- fevd(D41$CYCLES, type="GP", threshold = limit)
fPD51 <- fevd(D51$CYCLES, type="GP", threshold = limit)


# ==== GPD - L-Moments ======
# trying to fit
limit <-320000
fPD <- fevd(D21$CYCLES, type="GP", threshold = limit, method = "Lmoments")
plot(fPD, type = "qq", main = "Sample 01")

flmPD11 <- fevd(D11$CYCLES, type="GP", threshold = limit, method = "Lmoments")
flmPD21 <- fevd(D21$CYCLES, type="GP", threshold = limit, method = "Lmoments")
flmPD31 <- fevd(D31$CYCLES, type="GP", threshold = limit, method = "Lmoments")
flmPD41 <- fevd(D41$CYCLES, type="GP", threshold = limit, method = "Lmoments")
flmPD51 <- fevd(D51$CYCLES, type="GP", threshold = limit, method = "Lmoments")


# ======= Plot GPD ====
par(mfrow=c(2,3))
plot(fPD11, type = "qq", main = "Sample 01")
plot(fPD21, type = "qq", main = "Sample 02")
plot(fPD31, type = "qq", main = "Sample 03")
plot(fPD41, type = "qq", main = "Sample 04")
plot(fPD51, type = "qq", main = "Sample 05")
mtext("F01 - GPD", side = 3, line = -2, outer = TRUE)


# == Plot GPD L-Moments ====
par(mfrow=c(2,3))
plot(flmPD11, type = "qq", main = "Sample 01")
plot(flmPD21, type = "qq", main = "Sample 02")
plot(flmPD31, type = "qq", main = "Sample 03")
plot(flmPD41, type = "qq", main = "Sample 04")
plot(flmPD51, type = "qq", main = "Sample 05")
mtext("F01 - GPD", side = 3, line = -2, outer = TRUE)


# ======= GP threshrange.plot  ====
threshrange.plot(D11$CYCLES, r=c(27950400,27950000), type="GP", nint=15)



# ===== 6-EVCd =========
k=seq(20, 500, by=5); 

par(mfrow=c(2,3))
par=MTestEVC1d(D11$CYCLES,k,eta=2.0);
par=MTestEVC1d(D21$CYCLES,k,eta=2.0);
par=MTestEVC1d(D31$CYCLES,k,eta=2.0);
par=MTestEVC1d(D41$CYCLES,k,eta=2.0);
par=MTestEVC1d(D51$CYCLES,k,eta=2.0);


# ==== 7-Return level ====
ci(fbD21, alpha = 0.05, type = c("return.level"), return.period = c(100000))
ci(flmbD21, alpha = 0.05, type = c("return.level"), return.period = c(100000))
ci(fbD21, alpha = 0.05, type = c("return.level"), return.period = c(100000))

ci(fbD41, type = "return.level", return.period =  c(100000, 1000000, 10000000, 100000000))
ci(flmbD41, type = "return.level", return.period =  c(100000, 1000000, 10000000, 100000000))

