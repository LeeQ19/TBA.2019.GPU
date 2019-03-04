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
id.out <- c(18, 46, 47, 50, 51, 52, 57, 70, 78, 124, 125, 141, 143, 144, 150, 155, 
            165, 166, 217, 252, 268, 209, 243, 254, 264, 274, 279, 296, 309, 310)
id.x   <- c(4)
id.y   <- c(8:10)
id.t   <- 15
fy     <- 2018
rts    <- "drs"
ori    <- "o"


#########################################################################################################################
### Analysis
#########################################################################################################################

# Cleansed data
df.eff  <- df.raw[-id.out,]
df.nvd  <- subset(df.eff, mf == "NVIDIA")
df.amd  <- subset(df.eff, mf == "AMD")
res.roc <- roc.dea(df.eff[, id.x], df.eff[, id.y], df.eff[, id.t], fy, rts, ori)

# Table.1 Descriptive statistics
table.1 <- sapply(df.eff[, c(id.x, id.y)], function(x) c(Min  = min(x),
                                                         Med  = median(x),
                                                         Mean = mean(x),
                                                         Max  = max(x),
                                                         Std  = sd(x)))
print(noquote(format(round(t(table.1), 2), big.mark = ",")))

# Shares of AMD & Nvidia
table(df.eff$mf)
aggregate(mf~Released.year, data = df.eff[round(res.roc$eff_r, 4) == 1,], table)

# Figure 1.1 FLOPS/TDP (740*415)
ggplot() + 
  geom_point(data = df.eff, 
             aes(x = month, y = Floating.point.performance/TDP, color = mf), 
             size = 2, alpha = 0.3) + 
  geom_line(data = df.eff, stat = "smooth", method = lm, 
            aes(x = month, y = Floating.point.performance/TDP), 
            size = 1.0, color = "dodgerblue4", linetype = "dashed") + 
  geom_point(data = df.eff[round(res.roc$eff_r, 5) == 1,], 
             aes(x = month, y = Floating.point.performance/TDP, color = mf), 
             size = 2) + 
  geom_line(data = df.eff[round(res.roc$eff_r, 5) == 1,], stat = "smooth", method = lm, 
            aes(x = month, y = Floating.point.performance/TDP), 
            size = 1.0, color = "dodgerblue4") + 
  scale_color_manual(values = c("#870203", "#75b806")) + 
  scale_x_continuous(labels = seq(2006, 2018, 4)) +
  labs(x = "Year", y = "FLOPS/TDP (GFLOPS/W)") + 
  theme(axis.title           = element_text(size = 14),
        legend.title         = element_blank(), 
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "vertical", 
        legend.justification = c(0,1), legend.position = c(0,1), 
        legend.key = element_blank())

# Figure 1.2 TR/TDP (740*415)
ggplot() + 
  geom_point(data = df.eff, 
             aes(x = month, y = Texture.rate/TDP, color = mf), 
             size = 2, alpha = 0.3) + 
  geom_line(data = df.eff, stat = "smooth", method = lm, 
            aes(x = month, y = Texture.rate/TDP), 
            size = 1.0, color = "dodgerblue4", linetype = "dashed") + 
  geom_point(data = df.eff[round(res.roc$eff_r, 5) == 1,], 
             aes(x = month, y = Texture.rate/TDP, color = mf), 
             size = 2) + 
  geom_line(data = df.eff[round(res.roc$eff_r, 5) == 1,], stat = "smooth", method = lm, 
            aes(x = month, y = Texture.rate/TDP), 
            size = 1.0, color = "dodgerblue4") + 
  scale_color_manual(values = c("#870203", "#75b806")) + 
  scale_x_continuous(labels = seq(2006, 2018, 4)) +
  labs(x = "Year", y = "Texture Fill-Rate/TDP (GTPS/W)") + 
  theme(axis.title           = element_text(size = 14),
        legend.title         = element_blank(), 
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "vertical", 
        legend.justification = c(0,1), legend.position = c(0,1), 
        legend.key = element_blank())

# Figure 1.3 PR/TDP (740*415)
ggplot() + 
  geom_point(data = df.eff, 
             aes(x = month, y = Pixel.rate/TDP, color = mf), 
             size = 2, alpha = 0.3) + 
  geom_line(data = df.eff, stat = "smooth", method = lm, 
            aes(x = month, y = Pixel.rate/TDP), 
            size = 1.0, color = "dodgerblue4", linetype = "dashed") + 
  geom_point(data = df.eff[round(res.roc$eff_r, 5) == 1,], 
             aes(x = month, y = Pixel.rate/TDP, color = mf), 
             size = 2) + 
  geom_line(data = df.eff[round(res.roc$eff_r, 5) == 1,], stat = "smooth", method = lm, 
            aes(x = month, y = Pixel.rate/TDP), 
            size = 1.0, color = "dodgerblue4") + 
  scale_color_manual(values = c("#870203", "#75b806")) + 
  scale_x_continuous(labels = seq(2006, 2018, 4)) +
  labs(x = "Year", y = "Pixel Fill-Rate/TDP (GPPS/W)") + 
  theme(axis.title           = element_text(size = 14),
        legend.title         = element_blank(), 
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "vertical", 
        legend.justification = c(0,1), legend.position = c(0,1), 
        legend.key = element_blank())

# Pro WX 9100
df.2016.all  <- df.eff[df.eff$Released.year <= (2016 + 2),]
df.2016.amd  <- df.amd[df.amd$Released.year <= (2016 + 2),]
res.2016.all <- target.arrival.dea(df.2016.all[, id.x], df.2016.all[, id.y], df.2016.all[, id.t], 2016, rts, ori)
res.2016.amd <- target.arrival.dea(df.2016.amd[, id.x], df.2016.amd[, id.y], df.2016.amd[, id.t], 2016, rts, ori)
data.frame(A.Arv = df.2016.all$Released.year[df.2016.all$Name == "Pro WX 9100"],
           F.Arv.All = res.2016.all$arrival_seg[df.2016.all$Name == "Pro WX 9100"],
           F.RoC.All = res.2016.all$roc_ind[df.2016.all$Name == "Pro WX 9100"],
           F.Arv.AMD = res.2016.amd$arrival_seg[df.2016.amd$Name == "Pro WX 9100"],
           F.RoC.AMD = res.2016.amd$roc_ind[df.2016.amd$Name == "Pro WX 9100"])

# Table.2 Model accuracy
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
print(cbind(F.origin = c(since:(2018 - f.hrz), "Avg"), round(rbind(table.2, colMeans(table.2)), 4)))

# Table.3 SOAs in 2018
id.lroc <- which(res.roc$roc_local > 1)
table.3 <- data.frame(Name     = df.eff$Name[id.lroc],
                      MF       = df.eff$mf[id.lroc],
                      Year     = df.eff$Released.year[id.lroc],
                      df.eff[id.lroc, c(id.x, id.y), drop = F],
                      LocalRoC = res.roc$roc_local[id.lroc])
print(cbind(table.3[,1:3], format(round(table.3[,4:7], 2), big.mark = ","), round(table.3[,8, drop = F], 4)))

# Table.4 Target setting
id.soa   <- which(res.roc$roc_local > 1)
id.rtx80 <- which(df.eff$DMU %in% c(1, 54, 60, 131, 146, 188, 221, 254, 281, 334))
id.rx80  <- which(df.eff$DMU %in% c(33, 50, 110, 141, 187, 242, 265, 283, 309))
table.4  <- data.frame(Name       = rep(c("RTX 2080", "Radeon RX 580", "Virtual"), each = 3),
                       T.eff.2020 = c(res.roc$eff_t[284], mean(res.roc$eff_r[id.rtx80]), 1,
                                      res.roc$eff_t[310], mean(res.roc$eff_r[id.rx80]), 1,
                                      mean(res.roc$eff_r[df.eff$TDP == 75,]), mean(res.roc$eff_r[df.eff$TDP <= 75,]), 1),
                       Alpha.TDP  = rep(c(df.eff$TDP[284], df.eff$TDP[310], 75), each = 3),
                       Beta.FLO   = 1, Beta.TR    = 1, Beta.PR    = 1, Validation = NA)
for(i in 1:nrow(table.4)){
  x_f             <- mapply(c, df.eff[id.soa, id.x, drop = F], table.4$Alpha.TDP[i])
  y_f             <- mapply(c, df.eff[id.soa, id.y, drop = F] * res.roc$roc_local[id.soa,]^2, table.4[i, 4:6])
  res.target      <- target.spec.dea(x_f, y_f, dmu = length(id.soa) + 1, et = table.4$T.eff.2020[i], 
                                     alpha  = table.4$Alpha.TDP[i], rts = rts)
  table.4[i, 4:6] <- y_f[length(id.soa) + 1,] <- res.target$beta
  table.4[i, 7]   <- dm.dea(x_f, y_f, rts, ori, o = length(id.soa) + 1)$eff[length(id.soa) + 1]
}
print(cbind(table.4[,1, drop = F], round(table.4[,2,drop = F], 4), table.4[,3, drop = F], 
            format(round(table.4[,4:6], 2), big.mark = ","), round(table.4[,7,drop = F], 4)))
