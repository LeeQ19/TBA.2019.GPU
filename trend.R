#####################################################################################
### Setting up environment
#####################################################################################

# Load library  
pkgs <- c("ggplot2", "DJL")
sapply(pkgs, require, character.only = T)

# Load data
path   <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRozW_LPzi4Objm4RbLzM_cItbMZzPbaVFheZcv__palp9QhA0qUwUidRqeP7SwrHpcfDSAuXYraPjP/pub?output=csv"
df.raw <- read.csv(url(path))
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
ggplot(df.reg.nvd, aes(x = month, y = TDP)) + geom_point() + 
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
ggplot(df.reg.amd, aes(x = month, y = TDP)) + geom_point() + 
  geom_smooth(method = 'lm', se = TRUE)


#####################################################################################
### Trends of Outputs per Input of All DMU
#####################################################################################

# Cleansed data
df.eff  <- df.raw[-id.out,]

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

# Plot TDP - Month
ggplot(df.trend, aes(x = Month, y = TDP, color = MF)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)

# Plot FPP - Month
ggplot(df.trend, aes(x = Month, y = FPP, color = MF)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)

# Plot TR - Month
ggplot(df.trend, aes(x = Month, y = TR, color = MF)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)

# Plot PR - Month
ggplot(df.trend, aes(x = Month, y = PR, color = MF)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)

# Plot FPP/TDP - Month
ggplot(df.trend, aes(x = Month, y = FPP.TDP, color = MF)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)

# Plot TR/TDP - Month
ggplot(df.trend, aes(x = Month, y = TR.TDP, color = MF)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)

# Plot PR/TDP - Month
ggplot(df.trend, aes(x = Month, y = PR.TDP, color = MF)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)


#####################################################################################
### Trends of Outputs per Input of DMUs on the Frontier
#####################################################################################

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

# Plot FPP/TDP - Month
ggplot(df.trend.frt, aes(x = Month, y = FPP.TDP, color = MF)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)

# Plot TR/TDP - Month
ggplot(df.trend.frt, aes(x = Month, y = TR.TDP, color = MF)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)

# Plot PR/TDP - Month
ggplot(df.trend.frt, aes(x = Month, y = PR.TDP, color = MF)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)


#####################################################################################
### Trends of Outputs per Input of All DMUs & DMUs on the Frontier
#####################################################################################

# Plot FPP/TDP - Month
ggplot(df.trend) + 
  geom_point(data = df.trend[df.trend$MF == "NVIDIA", ], aes(x = Month, y = FPP.TDP), color = "#7777DD", shape = 1) + 
  geom_line(data = df.trend[df.trend$MF == "NVIDIA", ], stat = "smooth", method = lm, aes(x = Month, y = FPP.TDP), size = 1.0, color = "#7777DD", linetype = "dashed") + 
  geom_point(data = df.trend[df.trend$MF == "AMD", ], aes(x = Month, y = FPP.TDP), color = "#CC6666", shape = 1) + 
  geom_line(data = df.trend[df.trend$MF == "AMD", ], stat = "smooth", method = lm, aes(x = Month, y = FPP.TDP), size = 1.0, color = "#CC6666", linetype = "dashed") + 
  geom_point(data = df.trend.frt[df.trend.frt$MF == "NVIDIA", ], aes(x = Month, y = FPP.TDP), color = "#7777DD") + 
  geom_line(data = df.trend.frt[df.trend.frt$MF == "NVIDIA", ], stat = "smooth", method = lm, aes(x = Month, y = FPP.TDP), size = 1.0, color = "#7777DD") + 
  geom_point(data = df.trend.frt[df.trend.frt$MF == "AMD", ], aes(x = Month, y = FPP.TDP), color = "#CC6666") + 
  geom_line(data = df.trend.frt[df.trend.frt$MF == "AMD", ], stat = "smooth", method = lm, aes(x = Month, y = FPP.TDP), size = 1.0, color = "#CC6666")

# Plot TR/TDP - Month
ggplot(df.trend) + 
  geom_point(data = df.trend[df.trend$MF == "NVIDIA", ], aes(x = Month, y = TR.TDP), color = "#7777DD", shape = 1) + 
  geom_line(data = df.trend[df.trend$MF == "NVIDIA", ], stat = "smooth", method = lm, aes(x = Month, y = TR.TDP), size = 1.0, color = "#7777DD", linetype = "dashed") + 
  geom_point(data = df.trend[df.trend$MF == "AMD", ], aes(x = Month, y = TR.TDP), color = "#CC6666", shape = 1) + 
  geom_line(data = df.trend[df.trend$MF == "AMD", ], stat = "smooth", method = lm, aes(x = Month, y = TR.TDP), size = 1.0, color = "#CC6666", linetype = "dashed") + 
  geom_point(data = df.trend.frt[df.trend.frt$MF == "NVIDIA", ], aes(x = Month, y = TR.TDP), color = "#7777DD") + 
  geom_line(data = df.trend.frt[df.trend.frt$MF == "NVIDIA", ], stat = "smooth", method = lm, aes(x = Month, y = TR.TDP), size = 1.0, color = "#7777DD") + 
  geom_point(data = df.trend.frt[df.trend.frt$MF == "AMD", ], aes(x = Month, y = TR.TDP), color = "#CC6666") + 
  geom_line(data = df.trend.frt[df.trend.frt$MF == "AMD", ], stat = "smooth", method = lm, aes(x = Month, y = TR.TDP), size = 1.0, color = "#CC6666")

# Plot PR/TDP - Month
ggplot(df.trend) + 
  geom_point(data = df.trend[df.trend$MF == "NVIDIA", ], aes(x = Month, y = PR.TDP), color = "#7777DD", shape = 1) + 
  geom_line(data = df.trend[df.trend$MF == "NVIDIA", ], stat = "smooth", method = lm, aes(x = Month, y = PR.TDP), size = 1.0, color = "#7777DD", linetype = "dashed") + 
  geom_point(data = df.trend[df.trend$MF == "AMD", ], aes(x = Month, y = PR.TDP), color = "#CC6666", shape = 1) + 
  geom_line(data = df.trend[df.trend$MF == "AMD", ], stat = "smooth", method = lm, aes(x = Month, y = PR.TDP), size = 1.0, color = "#CC6666", linetype = "dashed") + 
  geom_point(data = df.trend.frt[df.trend.frt$MF == "NVIDIA", ], aes(x = Month, y = PR.TDP), color = "#7777DD") + 
  geom_line(data = df.trend.frt[df.trend.frt$MF == "NVIDIA", ], stat = "smooth", method = lm, aes(x = Month, y = PR.TDP), size = 1.0, color = "#7777DD") + 
  geom_point(data = df.trend.frt[df.trend.frt$MF == "AMD", ], aes(x = Month, y = PR.TDP), color = "#CC6666") + 
  geom_line(data = df.trend.frt[df.trend.frt$MF == "AMD", ], stat = "smooth", method = lm, aes(x = Month, y = PR.TDP), size = 1.0, color = "#CC6666")


#####################################################################################
### Calculate Efficiency of all DMUs at each Year
#####################################################################################

# Make empty data.frame
eff <- data.frame(Name = df.eff$Name)

# Run
for (i in 2007:2018){
  res.roc.all <- roc.dea(df.eff[, id.x], df.eff[, id.y], df.eff[, id.t], i, rts, ori)
  colnames(res.roc.all$eff_t) <- i
  eff <- cbind(eff, res.roc.all$eff_t)
}

# Display at least once efficient DMUs

select.eff <- apply(eff[-1], 1, function(x){1 %in% round(x, 7)})

eff.frt <- eff[select.eff, ]
eff.frt