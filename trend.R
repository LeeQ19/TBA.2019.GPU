#####################################################################################
### Trends of Outputs per Input
#####################################################################################

# Load library  
pkgs <- c("ggplot2")
sapply(pkgs, require, character.only = T)

# Load data
path   <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRozW_LPzi4Objm4RbLzM_cItbMZzPbaVFheZcv__palp9QhA0qUwUidRqeP7SwrHpcfDSAuXYraPjP/pub?output=csv"
df.raw <- read.csv(url(path))

# Make electric efficiency
df.trend <- data.frame(DMU     = df.raw$DMU, 
                       Name    = df.raw$Name, 
                       FPP.TDP = df.raw$Floating.point.performance / df.raw$TDP, 
                       TR.TDP  = df.raw$Texture.rate / df.raw$TDP, 
                       PR.TDP  = df.raw$Pixel.rate / df.raw$TDP, 
                       Month   = df.raw$month, 
                       MF      = df.raw$mf)

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
