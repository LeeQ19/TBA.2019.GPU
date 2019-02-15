#########################################################################################################################
### Project  : Technology forecasting of GPU
### Script   : GPU.R
### Contents : GPU paper for PICMET2019
#########################################################################################################################

#########################################################################################################################
### Setting up environment
#########################################################################################################################

# Load library  
pkgs <- c("DJL", "ggplot2")
sapply(pkgs, require, character.only = T)

# Load data
df.raw <- read.csv(url("http://bit.ly/2SxWwZC"), header = T)
df.raw[, "Released.year"] <- floor(df.raw[, "Released.date"])

# Parameter
id.out <- c(18, 46, 47, 50, 51, 52, 57, 70, 78, 124, 125, 141, 143, 144,
            150, 155, 165, 166, 217, 252, 268, 209, 235, 247, 251, 264)
id.x   <- c(4)
id.y   <- c(8:10)
id.t   <- 15
fy     <- 2018
rts    <- "vrs"
ori    <- "o"


#########################################################################################################################
### Pre-analysis
#########################################################################################################################

# Cleansed data
df.eff <- df.raw[-id.out,]
df.nvd <- subset(df.eff, mf == "NVIDIA")
df.amd <- subset(df.eff, mf == "AMD")

# Descriptive statistics
table.1 <- sapply(df.eff[, c(id.x, id.y)], function(x) c(Min  = min(x),
                                                         Med  = median(x),
                                                         Mean = mean(x),
                                                         Max  = max(x),
                                                         Std  = sd(x)))
print(noquote(format(round(t(table.1),2), big.mark = ",")))

# Fitting test for ALL
f.hrz   <- 2
since   <- 2012
res.fit <- data.frame(MAD.all  = NA, MAD.nvd = NA, MAD.amd = NA)
for(i in since:(2018 - f.hrz)){
  df.t.all  <- df.eff[df.eff$Released.year <= (i + f.hrz),]
  df.t.nvd  <- df.nvd[df.nvd$Released.year <= (i + f.hrz),]
  df.t.amd  <- df.amd[df.amd$Released.year <= (i + f.hrz),]
  res.t.all <- target.arrival.dea(df.t.all[, id.x], df.t.all[, id.y], df.t.all[, id.t], i, rts, ori)$arrival_seg
  res.t.nvd <- target.arrival.dea(df.t.nvd[, id.x], df.t.nvd[, id.y], df.t.nvd[, id.t], i, rts, ori)$arrival_seg
  res.t.amd <- target.arrival.dea(df.t.amd[, id.x], df.t.amd[, id.y], df.t.amd[, id.t], i, rts, ori)$arrival_seg
  res.e.all <- mean(abs(df.t.all[, id.t] - c(res.t.all)), na.rm = T)
  res.e.nvd <- mean(abs(df.t.nvd[, id.t] - c(res.t.nvd)), na.rm = T)
  res.e.amd <- mean(abs(df.t.amd[, id.t] - c(res.t.amd)), na.rm = T)
  res.fit[i - since + 1,] <- c(res.e.all, res.e.nvd, res.e.amd)
}
print(cbind(F.origin = c(since:(2018 - f.hrz), "Avg"), round(rbind(res.fit, colMeans(res.fit)), 4)))


#########################################################################################################################
### Analysis - all
#########################################################################################################################

# Run
res.roc.all <- roc.dea(df.eff[, id.x], df.eff[, id.y], df.eff[, id.t], fy, rts, ori)

# Efficient GPU in 2018
df.eff[round(res.roc.all$eff_t, 5) == 1,]

# RoC
cbind(df.eff, res.roc.all$roc_local)[!is.na(res.roc.all$roc_local),]

# Bind DMUs & eff at 2018
df.eff <- cbind(df.eff, Eff.2018 = res.roc.all$eff_t)


#########################################################################################################################
### Analysis - NVidia
#########################################################################################################################

# Selective data
df.nvd <- subset(df.eff, mf == "NVIDIA")

# Run
res.roc.nvd <- roc.dea(df.nvd[, id.x], df.nvd[, id.y], df.nvd[, id.t], fy, rts, ori)

# Efficient GPU in 2018
df.nvd[round(res.roc.nvd$eff_t, 5) == 1,]

# RoC
cbind(df.nvd, res.roc.nvd$roc_local)[!is.na(res.roc.nvd$roc_local),]

# Bind DMUs & eff at 2018
df.nvd <- cbind(df.nvd, Eff.2018    = res.roc.nvd$eff_t)
df.nvd <- cbind(df.nvd, Eff.release = res.roc.nvd$eff_r)


#####################################################################################
### Analysis - AMD
#####################################################################################

# Selective data
df.amd <- subset(df.eff, mf == "AMD")

# Run
res.roc.amd <- roc.dea(df.amd[, id.x], df.amd[, id.y], df.amd[, id.t], fy, rts, ori)

# Efficient GPU in 2018
df.amd[round(res.roc.amd$eff_t, 5) == 1,]

# RoC
cbind(df.amd, res.roc.amd$roc_local)[!is.na(res.roc.amd$roc_local),]

# Bind DMUs & eff at 2018
df.amd <- cbind(df.amd, Eff.2018    = res.roc.amd$eff_t)
df.amd <- cbind(df.amd, Eff.release = res.roc.amd$eff_r)


#####################################################################################
### I/O Regression
#####################################################################################

# Selective data - target & previous model
### Have to change
id.nvd <- c(1, 54, 60, 131, 146, 188, 221, 254, 281, 334)
id.amd <- c(33, 50, 110, 141, 187, 242, 265, 283, 309)

df.reg.nvd <- df.nvd[which(df.nvd$DMU %in% id.nvd), ]
df.reg.amd <- df.amd[which(df.amd$DMU %in% id.amd), ]

month.pred.nvd <- 167
month.pred.amd <- 150


# TDP predict -Nvidia
model.TDP.nvd <- lm(TDP ~ month, df.reg.nvd)
summary(model.TDP.nvd)

pred.TDP.nvd_temp <- predict(model.TDP.nvd, data.frame(month = month.pred.nvd), level = 0.9, interval = "confidence")
rownames(pred.TDP.nvd_temp) <- c("TDP")
pred.TDP.nvd <- data.frame(t(pred.TDP.nvd_temp),
                           TDP.Change = c((pred.TDP.nvd_temp[1] / df.reg.nvd[10, 4] - 1) * 100, (pred.TDP.nvd_temp[2] / df.reg.nvd[10, 4] - 1) * 100, (pred.TDP.nvd_temp[3] / df.reg.nvd[10, 4] - 1) * 100),
                           month = rep(month.pred.nvd, 3), 
                           Forecasting.date = rep(month.pred.nvd%/%12+2007+(month.pred.nvd%%12-2)/100, 3))

pred.TDP.nvd


# TDP plot - Nvidia
ggplot(df.reg.nvd, aes(x = month, y = TDP)) + 
  geom_point() + scale_size(guide = "none") + 
  geom_smooth(method = 'lm', se = TRUE)


# TDP predict - AMD
model.TDP.amd <- lm(TDP ~ month, df.reg.amd)
summary(model.TDP.amd)

pred.TDP.amd_temp <- predict(model.TDP.amd, data.frame(month = month.pred.amd), level = 0.9, interval = "confidence")
rownames(pred.TDP.amd_temp) <- c("TDP")
pred.TDP.amd <- data.frame(t(pred.TDP.amd_temp),
                           TDP.Change = c((pred.TDP.amd_temp[1] / df.reg.amd[9, 4] - 1) * 100, (pred.TDP.amd_temp[2] / df.reg.amd[9, 4] - 1) * 100, (pred.TDP.amd_temp[3] / df.reg.amd[9, 4] - 1) * 100),
                           month = rep(month.pred.amd, 3),
                           Forecasting.date = rep(month.pred.amd%/%12+2007+(month.pred.amd%%12-2)/100, 3))

pred.TDP.amd

# TDP plot - AMD
ggplot(df.reg.amd, aes(x = month, y = TDP)) + 
  geom_point() + scale_size(guide = "none") + 
  geom_smooth(method = 'lm', se = TRUE)


# Weight based on rescaling each output
weight.nvd_temp <- data.frame(FPP = 1 / df.reg.nvd$Floating.point.performance, 
                              TR  = 1 / df.reg.nvd$Texture.rate, 
                              PR  = 1 / df.reg.nvd$Pixel.rate)[nrow(df.reg.nvd), ]
weight.nvd <- weight.nvd_temp / sum(weight.nvd_temp)

weight.amd_temp <- data.frame(FPP = 1 / df.reg.amd$Floating.point.performance, 
                              TR  = 1 / df.reg.amd$Texture.rate, 
                              PR  = 1 / df.reg.amd$Pixel.rate)[nrow(df.reg.amd), ]
weight.amd <- weight.amd_temp / sum(weight.amd_temp)

weight     <- rbind(nvd = weight.nvd, 
                    amd = weight.amd)
# weight.reg <- rbind(nvd = as.data.frame(t(scale(t(weight.nvd), center = FALSE))), 
#                     amd = as.data.frame(t(scale(t(weight.amd), center = FALSE))))


#####################################################################################
### Target setting
#####################################################################################

# Function to compare
target.table <- function(DMU, target){
  table.x <- data.frame(Name = DMU$Name, 
                        Eff.2018 = DMU$Eff.2018, 
                        TDP = target$alpha, 
                        FPP = target$beta[1], 
                        TR = target$beta[2], 
                        PR = target$beta[3], 
                        TDP.change = round(target$alpha / DMU$TDP, 4), 
                        FPP.change = round(target$beta[1] / DMU$Floating.point.performance, 4), 
                        TR.change = round(target$beta[2] / DMU$Texture.rate, 4), 
                        PR.change = round(target$beta[3] / DMU$Pixel.rate, 4))
  table.x
}

# Target setting - Nvidia.RTX 2080
target.nvd.fit <- target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                  t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], wv = weight.nvd, rts = rts)

target.nvd.upr <- target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                  t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], wv = weight.nvd, rts = rts)

# Target setting - AMD.Radeon RX 580
target.amd.fit <- target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                  t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], wv = weight.amd, rts = rts)

target.amd.upr <- target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                  t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], wv = weight.amd, rts = rts)

# Run function
target.table(df.nvd[which(df.nvd$Name == "RTX 2080"), ], target.nvd.fit)

target.table(df.amd[which(df.amd$Name == "Radeon RX 580")[1], ], target.amd.fit)

# Grid search
weight.grid <- data.frame(FPP = rep(seq(1e-3, 1e-2, by = 1e-3), each = 1e+2), 
                          TR  = rep(seq(1e-1, 1, by = 1e-1), each = 1e+1, times = 1e+1), 
                          PR  = rep(seq(1001, 1010, by = 1), times = 1e+2))

res.grid    <- data.frame(t(apply(weight.grid, 1, 
                                  function(x){target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], 
                                                              wv = x, rts = rts)$beta})))

which(round(res.grid$X1, 2) != round(res.grid$X1[1], 2))
which(round(res.grid$X2, 4) != round(res.grid$X2[1], 4))
which(round(res.grid$X3, 4) != round(res.grid$X3[1], 4))

weight.grid.view <- weight.grid[which(round(res.grid$X1, 2) != round(res.grid$X1[1], 2)),]
for (i in 1 : length(t(weight.grid.view[1]))){
  weight.grid.view[i, ] <- round(weight.grid.view[i, ] / weight.grid.view[i, 1], 2)
}

weight.grid.non <- data.frame(weight.grid[-which(round(res.grid$X1, 2) != round(res.grid$X1[1], 2)),])
for (i in 1 : length(t(weight.grid.non[1]))){
  weight.grid.non[i, ] <- round(weight.grid.non[i, ] / weight.grid.non[i, 1], 2)
}

res.grid.view <- res.grid[which(round(res.grid$X1, 2) != round(res.grid$X1[1], 2)),]

plot(weight.grid.view[2:3], pch=25, xlim = c(100, 1000), ylim = c(100, 1000)) 
points(weight.grid.non[2:3], pch=1, xlim = c(100, 1000), ylim = c(100, 1000), col="red")

# RTX 2080
table.nvd <- data.frame()
table.pre <- data.frame(Name = "RTX 2080", 
                        Target.efficiency = df.nvd[which(df.nvd$Name == "RTX 2080"), ]$Eff.2018,
                        Displacement = pred.TDP.nvd$TDP[1], 
                        Weight.vector = "Auto",
                        FPP = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts)$beta[1], 
                        TR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                             t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts)$beta[2], 
                        PR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                             t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts)$beta[3], 
                        Post.hoc.test = df.nvd[which(df.nvd$Name == "RTX 2080"), ]$Eff.2018)
table.pre1 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 1",
                         FPP = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts, wv = c(1, 5, 1))$beta[1], 
                         TR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts, wv = c(1, 5, 1))$beta[2], 
                         PR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts, wv = c(1, 5, 1))$beta[3], 
                         Post.hoc.test = df.nvd[which(df.nvd$Name == "RTX 2080"), ]$Eff.2018)
table.pre2 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 7",
                         FPP = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts, wv = c(1, 5, 7))$beta[1], 
                         TR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts, wv = c(1, 5, 7))$beta[2], 
                         PR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts, wv = c(1, 5, 7))$beta[3], 
                         Post.hoc.test = df.nvd[which(df.nvd$Name == "RTX 2080"), ]$Eff.2018)
table.pre3 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 106",
                         FPP = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts, wv = c(1, 5, 106))$beta[1], 
                         TR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts, wv = c(1, 5, 106))$beta[2], 
                         PR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts, wv = c(1, 5, 106))$beta[3], 
                         Post.hoc.test = df.nvd[which(df.nvd$Name == "RTX 2080"), ]$Eff.2018)
table.nvd <- rbind(table.nvd, table.pre, table.pre1, table.pre2, table.pre3)

table.pre <- data.frame(Name = "", 
                        Target.efficiency = "",
                        Displacement = pred.TDP.nvd$TDP[3], 
                        Weight.vector = "Auto",
                        FPP = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts)$beta[1], 
                        TR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                             t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts)$beta[2], 
                        PR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                             t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts)$beta[3], 
                        Post.hoc.test = df.nvd[which(df.nvd$Name == "RTX 2080"), ]$Eff.2018)
table.pre1 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 1",
                         FPP = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts, wv = c(1, 5, 1))$beta[1], 
                         TR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts, wv = c(1, 5, 1))$beta[2], 
                         PR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts, wv = c(1, 5, 1))$beta[3], 
                         Post.hoc.test = df.nvd[which(df.nvd$Name == "RTX 2080"), ]$Eff.2018)
table.pre2 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 11",
                         FPP = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts, wv = c(1, 5, 11))$beta[1], 
                         TR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts, wv = c(1, 5, 11))$beta[2], 
                         PR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts, wv = c(1, 5, 11))$beta[3], 
                         Post.hoc.test = df.nvd[which(df.nvd$Name == "RTX 2080"), ]$Eff.2018)
table.pre3 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 210",
                         FPP = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts, wv = c(1, 5, 210))$beta[1], 
                         TR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts, wv = c(1, 5, 210))$beta[2], 
                         PR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts, wv = c(1, 5, 210))$beta[3], 
                         Post.hoc.test = df.nvd[which(df.nvd$Name == "RTX 2080"), ]$Eff.2018)
table.nvd <- rbind(table.nvd, table.pre, table.pre1, table.pre2, table.pre3)

table.pre <- data.frame(Name = "", 
                        Target.efficiency = 1.00000,
                        Displacement = pred.TDP.nvd$TDP[1], 
                        Weight.vector = "Auto",
                        FPP = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts, et = 1)$beta[1], 
                        TR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                             t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts, et = 1)$beta[2], 
                        PR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                             t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts, et = 1)$beta[3], 
                        Post.hoc.test = df.nvd[which(df.nvd$Name == "RTX 2080"), ]$Eff.2018)
table.pre1 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 1",
                         FPP = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts, wv = c(1, 5, 1), et = 1)$beta[1], 
                         TR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts, wv = c(1, 5, 1), et = 1)$beta[2], 
                         PR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts, wv = c(1, 5, 1), et = 1)$beta[3], 
                         Post.hoc.test = df.nvd[which(df.nvd$Name == "RTX 2080"), ]$Eff.2018)
table.pre2 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 7",
                         FPP = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts, wv = c(1, 5, 7), et = 1)$beta[1], 
                         TR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts, wv = c(1, 5, 7), et = 1)$beta[2], 
                         PR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts, wv = c(1, 5, 7), et = 1)$beta[3], 
                         Post.hoc.test = df.nvd[which(df.nvd$Name == "RTX 2080"), ]$Eff.2018)
table.pre3 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 106",
                         FPP = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts, wv = c(1, 5, 106), et = 1)$beta[1], 
                         TR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts, wv = c(1, 5, 106), et = 1)$beta[2], 
                         PR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], rts = rts, wv = c(1, 5, 106), et = 1)$beta[3], 
                         Post.hoc.test = df.nvd[which(df.nvd$Name == "RTX 2080"), ]$Eff.2018)
table.nvd <- rbind(table.nvd, table.pre, table.pre1, table.pre2, table.pre3)

table.pre <- data.frame(Name = "", 
                        Target.efficiency = "",
                        Displacement = pred.TDP.nvd$TDP[3], 
                        Weight.vector = "Auto",
                        FPP = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts, et = 1)$beta[1], 
                        TR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                             t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts, et = 1)$beta[2], 
                        PR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                             t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts, et = 1)$beta[3], 
                        Post.hoc.test = df.nvd[which(df.nvd$Name == "RTX 2080"), ]$Eff.2018)
table.pre1 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 1",
                         FPP = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts, wv = c(1, 5, 1), et = 1)$beta[1], 
                         TR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts, wv = c(1, 5, 1), et = 1)$beta[2], 
                         PR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts, wv = c(1, 5, 1), et = 1)$beta[3], 
                         Post.hoc.test = df.nvd[which(df.nvd$Name == "RTX 2080"), ]$Eff.2018)
table.pre2 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 11",
                         FPP = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts, wv = c(1, 5, 11), et = 1)$beta[1], 
                         TR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts, wv = c(1, 5, 11), et = 1)$beta[2], 
                         PR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts, wv = c(1, 5, 11), et = 1)$beta[3], 
                         Post.hoc.test = df.nvd[which(df.nvd$Name == "RTX 2080"), ]$Eff.2018)
table.pre3 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 210",
                         FPP = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts, wv = c(1, 5, 210), et = 1)$beta[1], 
                         TR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts, wv = c(1, 5, 210), et = 1)$beta[2], 
                         PR = target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[3], rts = rts, wv = c(1, 5, 210), et = 1)$beta[3], 
                         Post.hoc.test = df.nvd[which(df.nvd$Name == "RTX 2080"), ]$Eff.2018)
table.nvd <- rbind(table.nvd, table.pre, table.pre1, table.pre2, table.pre3)

# Radeon RX 580
table.amd <- data.frame()
table.pre <- data.frame(Name = "Radeon RX 580", 
                        Target.efficiency = df.amd[which(df.amd$Name == "Radeon RX 580")[1], ]$Eff.2018,
                        Displacement = pred.TDP.amd$TDP[1], 
                        Weight.vector = "Auto",
                        FPP = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts)$beta[1], 
                        TR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                             t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts)$beta[2], 
                        PR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                             t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts)$beta[3], 
                        Post.hoc.test = df.amd[which(df.amd$Name == "Radeon RX 580")[1], ]$Eff.2018)
table.pre1 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 1",
                         FPP = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts, wv = c(1, 5, 1))$beta[1], 
                         TR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts, wv = c(1, 5, 1))$beta[2], 
                         PR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts, wv = c(1, 5, 1))$beta[3], 
                         Post.hoc.test = df.amd[which(df.amd$Name == "Radeon RX 580")[1], ]$Eff.2018)
table.pre2 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 10",
                         FPP = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts, wv = c(1, 5, 10))$beta[1], 
                         TR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts, wv = c(1, 5, 10))$beta[2], 
                         PR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts, wv = c(1, 5, 10))$beta[3], 
                         Post.hoc.test = df.amd[which(df.amd$Name == "Radeon RX 580")[1], ]$Eff.2018)
table.pre3 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 100",
                         FPP = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts, wv = c(1, 5, 100))$beta[1], 
                         TR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts, wv = c(1, 5, 100))$beta[2], 
                         PR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts, wv = c(1, 5, 100))$beta[3], 
                         Post.hoc.test = df.amd[which(df.amd$Name == "Radeon RX 580")[1], ]$Eff.2018)
table.amd <- rbind(table.amd, table.pre, table.pre1, table.pre2, table.pre3)

table.pre <- data.frame(Name = "", 
                        Target.efficiency = "",
                        Displacement = pred.TDP.amd$TDP[3], 
                        Weight.vector = "Auto",
                        FPP = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts)$beta[1], 
                        TR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                             t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts)$beta[2], 
                        PR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                             t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts)$beta[3], 
                        Post.hoc.test = df.amd[which(df.amd$Name == "Radeon RX 580")[1], ]$Eff.2018)
table.pre1 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 1",
                         FPP = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts, wv = c(1, 5, 1))$beta[1], 
                         TR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts, wv = c(1, 5, 1))$beta[2], 
                         PR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts, wv = c(1, 5, 1))$beta[3], 
                         Post.hoc.test = df.amd[which(df.amd$Name == "Radeon RX 580")[1], ]$Eff.2018)
table.pre2 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 10",
                         FPP = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts, wv = c(1, 5, 10))$beta[1], 
                         TR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts, wv = c(1, 5, 10))$beta[2], 
                         PR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts, wv = c(1, 5, 10))$beta[3], 
                         Post.hoc.test = df.amd[which(df.amd$Name == "Radeon RX 580")[1], ]$Eff.2018)
table.pre3 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 100",
                         FPP = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts, wv = c(1, 5, 100))$beta[1], 
                         TR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts, wv = c(1, 5, 100))$beta[2], 
                         PR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts, wv = c(1, 5, 100))$beta[3], 
                         Post.hoc.test = df.amd[which(df.amd$Name == "Radeon RX 580")[1], ]$Eff.2018)
table.amd <- rbind(table.amd, table.pre, table.pre1, table.pre2, table.pre3)

table.pre <- data.frame(Name = "", 
                        Target.efficiency = 1,
                        Displacement = pred.TDP.amd$TDP[1], 
                        Weight.vector = "Auto",
                        FPP = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts, et = 1)$beta[1], 
                        TR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                             t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts, et = 1)$beta[2], 
                        PR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                             t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts, et = 1)$beta[3], 
                        Post.hoc.test = df.amd[which(df.amd$Name == "Radeon RX 580")[1], ]$Eff.2018)
table.pre1 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 1",
                         FPP = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts, wv = c(1, 5, 1), et = 1)$beta[1], 
                         TR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts, wv = c(1, 5, 1), et = 1)$beta[2], 
                         PR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts, wv = c(1, 5, 1), et = 1)$beta[3], 
                         Post.hoc.test = df.amd[which(df.amd$Name == "Radeon RX 580")[1], ]$Eff.2018)
table.pre2 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 10",
                         FPP = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts, wv = c(1, 5, 10), et = 1)$beta[1], 
                         TR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts, wv = c(1, 5, 10), et = 1)$beta[2], 
                         PR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts, wv = c(1, 5, 10), et = 1)$beta[3], 
                         Post.hoc.test = df.amd[which(df.amd$Name == "Radeon RX 580")[1], ]$Eff.2018)
table.pre3 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 100",
                         FPP = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts, wv = c(1, 5, 100), et = 1)$beta[1], 
                         TR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts, wv = c(1, 5, 100), et = 1)$beta[2], 
                         PR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[1], rts = rts, wv = c(1, 5, 100), et = 1)$beta[3], 
                         Post.hoc.test = df.amd[which(df.amd$Name == "Radeon RX 580")[1], ]$Eff.2018)
table.amd <- rbind(table.amd, table.pre, table.pre1, table.pre2, table.pre3)

table.pre <- data.frame(Name = "", 
                        Target.efficiency = "",
                        Displacement = pred.TDP.amd$TDP[3], 
                        Weight.vector = "Auto",
                        FPP = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts, et = 1)$beta[1], 
                        TR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                             t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts, et = 1)$beta[2], 
                        PR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                             t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts, et = 1)$beta[3], 
                        Post.hoc.test = df.amd[which(df.amd$Name == "Radeon RX 580")[1], ]$Eff.2018)
table.pre1 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 1",
                         FPP = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts, wv = c(1, 5, 1), et = 1)$beta[1], 
                         TR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts, wv = c(1, 5, 1), et = 1)$beta[2], 
                         PR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts, wv = c(1, 5, 1), et = 1)$beta[3], 
                         Post.hoc.test = df.amd[which(df.amd$Name == "Radeon RX 580")[1], ]$Eff.2018)
table.pre2 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 10",
                         FPP = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts, wv = c(1, 5, 10), et = 1)$beta[1], 
                         TR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts, wv = c(1, 5, 10), et = 1)$beta[2], 
                         PR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts, wv = c(1, 5, 10), et = 1)$beta[3], 
                         Post.hoc.test = df.amd[which(df.amd$Name == "Radeon RX 580")[1], ]$Eff.2018)
table.pre3 <- data.frame(Name = "", 
                         Target.efficiency = "",
                         Displacement = "", 
                         Weight.vector = "1 : 5 : 100",
                         FPP = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                               t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts, wv = c(1, 5, 100), et = 1)$beta[1], 
                         TR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts, wv = c(1, 5, 100), et = 1)$beta[2], 
                         PR = target.spec.dea(data.frame(df.amd[, id.x]), data.frame(df.amd[, id.y]), data.frame(df.amd[, id.t]), 
                                              t = fy, dt = 2, dmu = which(df.amd$Name == "Radeon RX 580")[1], alpha = pred.TDP.amd$TDP[3], rts = rts, wv = c(1, 5, 100), et = 1)$beta[3], 
                         Post.hoc.test = df.amd[which(df.amd$Name == "Radeon RX 580")[1], ]$Eff.2018)
table.amd <- rbind(table.amd, table.pre, table.pre1, table.pre2, table.pre3)
