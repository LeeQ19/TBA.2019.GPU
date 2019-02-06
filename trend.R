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
id.out <- c(18, 46, 47, 50, 51, 52, 57, 70, 78, 124, 125, 141, 143, 144, 150, 155, 165, 166, 217, 252, 268)
id.x   <- c(4)
id.y   <- c(8:10)
id.t   <- 15
fy     <- 2018
rts    <- "vrs"
ori    <- "o"


#####################################################################################
### Trends of Outputs per Input of All DMU
#####################################################################################

# Cleansed data
df.eff  <- df.raw[-id.out,]

# Make electric efficiency
df.trend <- data.frame(DMU     = df.eff$DMU, 
                       Name    = df.eff$Name, 
                       FPP.TDP = df.eff$Floating.point.performance / df.eff$TDP, 
                       TR.TDP  = df.eff$Texture.rate / df.eff$TDP, 
                       PR.TDP  = df.eff$Pixel.rate / df.eff$TDP, 
                       Month   = df.eff$month, 
                       MF      = df.eff$mf)

# Plot FPP/TDP - Month
ggplot(df.trend, aes(x = Month, y = FPP.TDP, color = MF)) + 
  geom_point() + scale_size(guide = "none") + 
  geom_smooth(method = 'lm', se = FALSE)

# Plot TR/TDP - Month
ggplot(df.trend, aes(x = Month, y = TR.TDP, color = MF)) + 
  geom_point() + scale_size(guide = "none") + 
  geom_smooth(method = 'lm', se = FALSE)

# Plot PR/TDP - Month
ggplot(df.trend, aes(x = Month, y = PR.TDP, color = MF)) + 
  geom_point() + scale_size(guide = "none") + 
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

df.frt <- na.omit(df.frt)

# Make electric efficiency
df.trend.frt <- data.frame(DMU     = df.frt$DMU, 
                           Name    = df.frt$Name, 
                           FPP.TDP = df.frt$Floating.point.performance / df.frt$TDP, 
                           TR.TDP  = df.frt$Texture.rate / df.frt$TDP, 
                           PR.TDP  = df.frt$Pixel.rate / df.frt$TDP, 
                           Month   = df.frt$month, 
                           MF      = df.frt$mf)

# Plot FPP/TDP - Month
ggplot(df.trend.frt, aes(x = Month, y = FPP.TDP, color = MF)) + 
  geom_point() + scale_size(guide = "none") + 
  geom_smooth(method = 'lm', se = FALSE)

# Plot TR/TDP - Month
ggplot(df.trend.frt, aes(x = Month, y = TR.TDP, color = MF)) + 
  geom_point() + scale_size(guide = "none") + 
  geom_smooth(method = 'lm', se = FALSE)

# Plot PR/TDP - Month
ggplot(df.trend.frt, aes(x = Month, y = PR.TDP, color = MF)) + 
  geom_point() + scale_size(guide = "none") + 
  geom_smooth(method = 'lm', se = FALSE)
