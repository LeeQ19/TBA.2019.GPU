#####################################################################################
### Project  : Technology forecasting of GPU
### Script   : GPU.R
### Contents : GPU paper for PICMET2019
#####################################################################################

#####################################################################################
### Setting up environment
#####################################################################################

# Load library  
pkgs <- c("DJL", "ggplot2")
sapply(pkgs, require, character.only = T)

# Load data
path   <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRozW_LPzi4Objm4RbLzM_cItbMZzPbaVFheZcv__palp9QhA0qUwUidRqeP7SwrHpcfDSAuXYraPjP/pub?output=csv"
path2  <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRyu65AXjBaSFrKy2oos9nbJ4NM6PpHKoOcuie3O_58STPQ8FGFresl9nMhXkQDHPL1pQkmTTccZQBi/pub?output=csv"
df.raw <- read.csv(url(path))
df.raw2 <- read.csv(url(path2))
df.raw[, "Released.year"] <- floor(df.raw[, "Released.date"])


# Parameter
id.out <- c(18, 46, 47, 50, 51, 52, 57, 70, 78, 124, 125, 141, 143, 144, 150, 155, 165, 166, 217, 252, 268)
id.x   <- c(4)
id.y   <- c(8:10)
id.t   <- 15
fy     <- 2018
rts    <- "vrs"
ori    <- "o"


#####################################################################################
### Analysis - all
#####################################################################################

# Cleansed data
df.eff  <- df.raw[-id.out,]

# Run
res.roc.all <- roc.dea(df.eff[, id.x], df.eff[, id.y], df.eff[, id.t], fy, rts, ori)

# Efficient GPU in 7
df.eff[round(res.roc.all$eff_t, 5) == 1,]

# RoC
cbind(df.eff, res.roc.all$roc_local)[!is.na(res.roc.all$roc_local),]

# Bind DMUs & eff at 2018
df.eff <- cbind(df.eff, Eff.2018 = res.roc.all$eff_t)


#####################################################################################
### Analysis - NVidia
#####################################################################################

# Selective data
df.nvd <- subset(df.eff, mf == "NVIDIA")

# Run
res.roc.nvd <- roc.dea(df.nvd[, id.x], df.nvd[, id.y], df.nvd[, id.t], fy, rts, ori)

# Efficient GPU in 7
df.nvd[round(res.roc.nvd$eff_t, 5) == 1,]

# RoC
cbind(df.nvd, res.roc.nvd$roc_local)[!is.na(res.roc.nvd$roc_local),]

# Bind DMUs & eff at 2018
df.nvd <- cbind(df.nvd, Eff.2018 = res.roc.nvd$eff_t)

#####################################################################################
### Analysis - AMD
#####################################################################################

# Selective data
df.amd <- subset(df.eff, mf == "AMD")

# Run
res.roc.amd <- roc.dea(df.amd[, id.x], df.amd[, id.y], df.amd[, id.t], fy, rts, ori)

# Efficient GPU in 7
df.amd[round(res.roc.amd$eff_t, 5) == 1,]

# RoC
cbind(df.amd, res.roc.amd$roc_local)[!is.na(res.roc.amd$roc_local),]

# Bind DMUs & eff at 2018
df.amd <- cbind(df.amd, Eff.2018 = res.roc.amd$eff_t)


#####################################################################################
### I/O Regression
#####################################################################################

# Selective data - target & previous model
### Have to change
id.nvd <- c(1, 54, 60, 131, 146, 188, 221, 254, 281, 334)
id.amd <- c(33, 50, 110, 141, 187, 242, 265, 283, 309)

df.reg.nvd <- df.raw[id.nvd, ]
df.reg.amd <- df.raw[id.amd, ]

month.pred.nvd <- 167
month.pred.amd <- 150


# TDP predict -Nvidia
model.TDP.nvd <- lm(TDP ~ month, df.reg.nvd)
summary(model.TDP.nvd)

pred.TDP.nvd_temp <- predict(model.TDP.nvd, data.frame(month = month.pred.nvd), level = 0.9, interval = "confidence")
rownames(pred.TDP.nvd_temp) <- c("TDP")
pred.TDP.nvd <- data.frame(month = rep(month.pred.nvd, 3), 
                           t(pred.TDP.nvd_temp))

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
pred.TDP.amd <- data.frame(month = rep(month.pred.amd, 3), 
                           t(pred.TDP.amd_temp))

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

# Define function to compare
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
target.table(df.nvd[which(df.nvd$Name == "RTX 2080"), ], target.nvd.upr)

target.table(df.amd[which(df.amd$Name == "Radeon RX 580")[1], ], target.amd.fit)

# Grid search
weight.grid <- data.frame(FPP = rep(seq(1e-1, 1, by = 1e-1), each = 1e+2), 
                          TR  = rep(seq(1e-1, 1, by = 1e-1), each = 1e+1, times = 1e+1), 
                          PR  = rep(seq(1e-1, 1, by = 1e-1), times = 1e+2))

res.grid    <- data.frame(t(apply(weight.grid, 1, 
                                  function(x){target.spec.dea(data.frame(df.nvd[, id.x]), data.frame(df.nvd[, id.y]), data.frame(df.nvd[, id.t]), 
                                                              t = fy, dt = 2, dmu = which(df.nvd$Name == "RTX 2080"), alpha = pred.TDP.nvd$TDP[1], 
                                                              wv = x, rts = rts)$beta})))

which(round(res.grid$X1, 2) != round(res.grid$X1[1], 2))
which(round(res.grid$X2, 4) != round(res.grid$X2[1], 4))
which(round(res.grid$X3, 4) != round(res.grid$X3[1], 4))
