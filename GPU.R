#####################################################################################
### Project  : Technology forecasting of GPU
### Script   : GPU.R
### Contents : GPU paper for PICMET2019
#####################################################################################

#####################################################################################
### Setting up environment
#####################################################################################

# Load library  
pkgs <- c("DJL")
sapply(pkgs, require, character.only = T)

# Load data
path   <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRozW_LPzi4Objm4RbLzM_cItbMZzPbaVFheZcv__palp9QhA0qUwUidRqeP7SwrHpcfDSAuXYraPjP/pub?output=csv"
df.raw <- read.csv(url(path))

# Parameter
id.out <- c(18, 46, 47, 50, 51, 52, 57, 70, 78, 124, 125, 141, 143, 144, 150, 217, 252, 268)
id.x   <- c(4:7)
id.y   <- c(8:10)
id.t   <- 14

#####################################################################################
### NDF: Banker and Morey's Internal Model
#####################################################################################

# Cleansed data
df.eff  <- df.raw[-id.out,]

# Run
res.roc <- roc.dea(df.eff[, id.x], df.eff[, id.y], df.eff[, id.t], 7, "vrs", "o")

# Efficient GPU in 7
df.eff[round(res.roc$eff_t, 5) == 1,]

# RoC
df.eff[!is.na(res.roc$roc_local),]
