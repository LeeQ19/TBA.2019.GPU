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
df.eff <- cbind(df.eff, Eff.release = res.roc.all$eff_r)


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
df.nvd <- cbind(df.nvd, Eff.2018.nvd    = res.roc.nvd$eff_t)
df.nvd <- cbind(df.nvd, Eff.release.nvd = res.roc.nvd$eff_r)


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
df.amd <- cbind(df.amd, Eff.2018.amd    = res.roc.amd$eff_t)
df.amd <- cbind(df.amd, Eff.release.amd = res.roc.amd$eff_r)


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


#####################################################################################
### Target setting
#####################################################################################

making.Table <- function(Name.pr, pred.input, efficiency = "c", data.all = df.eff){
  table.new <- data.frame()
  for (i in 1:length(pred.input)){
    table.pre <- data.frame(Name = if (i == 1) Name.pr else "", 
                            Target.efficiency = if (i == 1) data.all[which(data.all$Name == Name.pr)[1], ]$Eff.2018 else "",
                            TDP = pred.input[i], 
                            Weight.vector = "Auto",
                            FPP = target.spec.dea(data.frame(data.all[, id.x]), data.frame(data.all[, id.y]), data.frame(data.all[, id.t]), 
                                                  t = fy, dt = 2, dmu = which(data.all$Name == Name.pr)[1], alpha = pred.input[i], rts = rts)$beta[1], 
                            TR = target.spec.dea(data.frame(data.all[, id.x]), data.frame(data.all[, id.y]), data.frame(data.all[, id.t]), 
                                                 t = fy, dt = 2, dmu = which(data.all$Name == Name.pr)[1], alpha = pred.input[i], rts = rts)$beta[2], 
                            PR = target.spec.dea(data.frame(data.all[, id.x]), data.frame(data.all[, id.y]), data.frame(data.all[, id.t]), 
                                                 t = fy, dt = 2, dmu = which(data.all$Name == Name.pr)[1], alpha = pred.input[i], rts = rts)$beta[3], 
                            Post.hoc.test = data.all[which(data.all$Name == Name.pr)[1], ]$Eff.2018)
    table.new <- rbind(table.new, table.pre)
  }
  if (efficiency[1] != "c"){
    for (j in 1:length(efficiency)){
      for (i in 1:length(pred.input)){
        table.pre <- data.frame(Name = "", 
                                Target.efficiency = if (i == 1) efficiency[j] else "",
                                TDP = pred.input[i], 
                                Weight.vector = "Auto", 
                                FPP = target.spec.dea(data.frame(data.all[, id.x]), data.frame(data.all[, id.y]), data.frame(data.all[, id.t]), 
                                                      t = fy, dt = 2, dmu = which(data.all$Name == Name.pr)[1], alpha = pred.input[i], rts = rts, et = efficiency[j])$beta[1], 
                                TR = target.spec.dea(data.frame(data.all[, id.x]), data.frame(data.all[, id.y]), data.frame(data.all[, id.t]), 
                                                     t = fy, dt = 2, dmu = which(data.all$Name == Name.pr)[1], alpha = pred.input[i], rts = rts, et = efficiency[j])$beta[2], 
                                PR = target.spec.dea(data.frame(data.all[, id.x]), data.frame(data.all[, id.y]), data.frame(data.all[, id.t]), 
                                                     t = fy, dt = 2, dmu = which(data.all$Name == Name.pr)[1], alpha = pred.input[i], rts = rts, et = efficiency[j])$beta[3], 
                                Post.hoc.test = data.all[which(data.all$Name == Name.pr)[1], ]$Eff.2018)
        table.new <- rbind(table.new, table.pre)
      }
    }
    
  }
  return(table.new)
}

# NVIDIA & AMD Product table
table.nvd <- making.Table("RTX 2080", c(pred.TDP.nvd$TDP[1], pred.TDP.nvd$TDP[3]), c(1, mean(df.reg.nvd$Eff.release)))
table.amd <- making.Table("Radeon RX 580", c(pred.TDP.amd$TDP[1], pred.TDP.amd$TDP[3]), c(1, mean(df.reg.amd$Eff.release)))

# Histogram
hist(replace(df.eff$TDP[df.eff$Released.date > 2017], df.eff$TDP[df.eff$Released.date > 2017] > 300, 290), breaks = 6, xlab = "TDP", main = "Histogram of TDP after 2017")

# New target setting 
df.eff[round(res.roc.all$eff_t, 5) == 1 & df.eff$TDP < 200,]

#####################################################################################
### Trends of Outputs per Input
#####################################################################################

# Make electric efficiency
df.trend <- data.frame(DMU     = df.eff$DMU, 
                       Name    = df.eff$Name, 
                       TDP     = df.eff$TDP, 
                       FPP     = df.eff$Floating.point.performance, 
                       TR      = df.eff$Texture.rate, 
                       PR      = df.eff$Pixel.rate, 
                       FPP.TDP = df.eff$Floating.point.performance / df.eff$TDP, 
                       TR.TDP  = df.eff$Texture.rate / df.eff$TDP, 
                       PR.TDP  = df.eff$Pixel.rate / df.eff$TDP, 
                       Month   = df.eff$month, 
                       MF      = df.eff$mf)

# Select DMUs on frontier
df.frt <- data.frame()

for (i in 2007:2018){
  res.roc.all <- roc.dea(df.eff[, id.x], df.eff[, id.y], df.eff[, id.t], i, rts, ori)
  df.frt      <- rbind(df.frt, df.eff[round(res.roc.all$eff_t, 5) == 1,])
}

df.frt <- unique(na.omit(df.frt))
rownames(df.frt) <- seq(nrow(df.frt))

# Make electric efficiency
df.trend.frt <- data.frame(DMU     = df.frt$DMU, 
                           Name    = df.frt$Name, 
                           TDP     = df.frt$TDP, 
                           FPP     = df.frt$Floating.point.performance, 
                           TR      = df.frt$Texture.rate, 
                           PR      = df.frt$Pixel.rate, 
                           FPP.TDP = df.frt$Floating.point.performance / df.frt$TDP, 
                           TR.TDP  = df.frt$Texture.rate / df.frt$TDP, 
                           PR.TDP  = df.frt$Pixel.rate / df.frt$TDP, 
                           Month   = df.frt$month, 
                           MF      = df.frt$mf)

colors <- c("NVIDIA" = "#75b806", "AMD" = "#870203")

# Plot FPP/TDP - Month
ggplot(df.trend) + 
  geom_point(data = df.trend, aes(x = Month, y = FPP.TDP, color = MF), size = 2, alpha = 0.3) + 
  geom_line(data = df.trend, stat = "smooth", method = lm, aes(x = Month, y = FPP.TDP), size = 1.0, color = "blue", linetype = "dashed") + 
  geom_point(data = df.trend.frt, aes(x = Month, y = FPP.TDP, color = MF), size = 2) + 
  geom_line(data = df.trend.frt, stat = "smooth", method = lm, aes(x = Month, y = FPP.TDP), size = 1.0, color = "blue") + 
  scale_color_manual(values = colors) + 
  scale_x_continuous(labels = c(2007, 2011, 2015, 2019)) +
  labs(x = "Released Year", y = "FLOPS / TDP (GFLOPS / W)") + 
  theme(legend.title         = element_blank(), 
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "vertical", 
        legend.justification = c(1,0), legend.position = c(1,0), 
        legend.key = element_blank())

# Plot TR/TDP - Month
ggplot(df.trend) + 
  geom_point(data = df.trend, aes(x = Month, y = TR.TDP, color = MF), size = 2, alpha = 0.3) + 
  geom_line(data = df.trend, stat = "smooth", method = lm, aes(x = Month, y = TR.TDP), size = 1.0, color = "blue", linetype = "dashed") + 
  geom_point(data = df.trend.frt, aes(x = Month, y = TR.TDP, color = MF), size = 2) + 
  geom_line(data = df.trend.frt, stat = "smooth", method = lm, aes(x = Month, y = TR.TDP), size = 1.0, color = "blue") + 
  scale_color_manual(values = colors) + 
  scale_x_continuous(labels = c(2007, 2011, 2015, 2019)) +
  labs(x = "Released Year", y = "TRPS / TDP (GTPS / W)") + 
  theme(legend.title         = element_blank(), 
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "vertical", 
        legend.justification = c(1,0), legend.position = c(1,0), 
        legend.key = element_blank())

# Plot PR/TDP - Month
ggplot(df.trend) + 
  geom_point(data = df.trend, aes(x = Month, y = PR.TDP, color = MF), size = 2, alpha = 0.3) + 
  geom_line(data = df.trend, stat = "smooth", method = lm, aes(x = Month, y = PR.TDP), size = 1.0, color = "blue", linetype = "dashed") + 
  geom_point(data = df.trend.frt, aes(x = Month, y = PR.TDP, color = MF), size = 2) + 
  geom_line(data = df.trend.frt, stat = "smooth", method = lm, aes(x = Month, y = PR.TDP), size = 1.0, color = "blue") + 
  scale_color_manual(values = colors) + 
  scale_x_continuous(labels = c(2007, 2011, 2015, 2019)) +
  labs(x = "Released Year", y = "PRPS / TDP (GPPS / W)") + 
  theme(legend.title         = element_blank(), 
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "vertical", 
        legend.justification = c(1,0), legend.position = c(1,0), 
        legend.key = element_blank())
