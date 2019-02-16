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
### Analysis
#########################################################################################################################

# Cleansed data
df.eff <- df.raw[-id.out,]
df.nvd <- subset(df.eff, mf == "NVIDIA")
df.amd <- subset(df.eff, mf == "AMD")

# Table.1 Descriptive statistics
table.1 <- sapply(df.eff[, c(id.x, id.y)], function(x) c(Min  = min(x),
                                                         Med  = median(x),
                                                         Mean = mean(x),
                                                         Max  = max(x),
                                                         Std  = sd(x)))
print(noquote(format(round(t(table.1), 2), big.mark = ",")))

# Table.2 Verification
f.hrz   <- 2
since   <- 2012
table.2 <- data.frame(MAD.all  = NA, MAD.nvd = NA, MAD.amd = NA)
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
  table.2[i - since + 1,] <- c(res.e.all, res.e.nvd, res.e.amd)
}
print(cbind(F.origin = c(since:(2018 - f.hrz), "Avg"), round(rbind(table.2, colMeans(res.fit)), 4)))

# Table.3 SOAs in 2018
res.roc <- roc.dea(df.eff[, id.x], df.eff[, id.y], df.eff[, id.t], fy, rts, ori)
id.lroc <- which(res.roc$roc_local > 1)
table.3 <- data.frame(Name     = df.eff$Name[id.lroc],
                      MF       = df.eff$mf[id.lroc],
                      Year     = df.eff$Released.year[id.lroc],
                      df.eff[id.lroc, c(id.x, id.y), drop = F],
                      LocalRoC = res.roc$roc_local[id.lroc])
print(cbind(table.3[,1:3], format(round(table.3[,4:7], 2), big.mark = ","), round(table.3[,8, drop = F], 4)))


#########################################################################################################################
### I/O Regression
#########################################################################################################################

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
hist(replace(df.eff$TDP[df.eff$Released.date > 2017], df.eff$TDP[df.eff$Released.date > 2017] > 300, 290), breaks = 3, xlab = "TDP", main = "Histogram of TDP after 2017")

# New target setting 
df.eff[round(res.roc.all$eff_t, 5) == 1 & df.eff$TDP < 200,]


