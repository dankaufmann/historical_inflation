# ------------------------------------------------------------------------
# Daniel Kaufmann, "Is Deflation Costly After All? The Perils of Erroneous 
#   Historical Classifications," Journal of Applied Econometrics, forthcoming
# ------------------------------------------------------------------------
#
# Simulates the probability limit under various assumptions on the 
# measurement error process 
#
# ------------------------------------------------------------------------
# Daniel Kaufmann, 2020, daniel.kaufmann@unine.ch
# ------------------------------------------------------------------------
rm(list=ls())
library('reshape2')
library('ggplot2')
library(extrafont)

# ------------------------------------------------------------------------
# Function to simulate the probability limit
# ------------------------------------------------------------------------
simBias <- function(Settings){
  
  beta <- Settings$beta
  alpha <- Settings$alpha
  
  sig <- Settings$sig
  rho0 <- Settings$rho0
  rho1 <- Settings$rho1
  c <- Settings$c
  N <- Settings$N
  s2n <- Settings$s2n
  dep <- Settings$dep

  Bias <- array(NA, c(length(s2n), 3))
  
  for (s in 1:length(s2n)) {
    
    # 1) Compute variances based on signal2noise ratio
    sigo = sqrt(sig^2*(1/(1+s2n[s])))
    sigp = sqrt(sig^2 - sigo^2)
    
    # 2) Simulate actual and error ridden inflation
    pia = rnorm(N, 0, sigp)
    pie = rho0+rho1*pia+rnorm(N, 0, sigo)
    
    # Calculate the bias
    # (1 - P[dt = 0 | xt = 1] - P[dt = 1 | xt = 0])
    dt = pia < c
    xt = pie < c
    
    #print(dt)
    dx1 = dt[which(xt == TRUE)]
    dx0 = dt[which(xt == FALSE)]
    
    Pd0x1 = mean(dx1 == FALSE)
    Pd1x0 = mean(dx0 == TRUE)
    
    Bias[s, 1] = alpha+beta*Pd1x0
    Bias[s, 2] = beta*(1-Pd0x1-Pd1x0)
    Bias[s, 3] = Bias[s, 1]+Bias[s, 2]
    
    
    # Add the dependend variable bias
    # + E[-(pie-pia)| xt = 1] - E[-(pie-pia)| xt = 0]
    if(dep == TRUE){
      diffp <- -(pie-pia)
      diffpx1 = diffp[which(xt == TRUE)]
      diffpx0 = diffp[which(xt == FALSE)]
      
      Ediffpx1 = mean(diffpx1)
      Ediffpx0 = mean(diffpx0)
      
      Bias[s, 1] <- Bias[s, 1] + Ediffpx0
      Bias[s, 2] <- Bias[s, 2] + Ediffpx1 - Ediffpx0
      Bias[s, 3] <- Bias[s, 1]+Bias[s, 2]
    }
  }
  
  return(Bias)
}

# ------------------------------------------------------------------------
# Do the simulations
# ------------------------------------------------------------------------
N = 1000000

sett1 <- list(alpha = 1, beta = -1, "sig" = 6, "rho0" = 0, "rho1" = 1, "c" = 0, "s2n" = seq(0.001, 6, by = 0.05), "N" = N, dep = FALSE)
Bias1 = simBias(sett1)

sett2 <- list(alpha = 1, beta = -1, "sig" = 6, "rho0" = 0, "rho1" = 1, "c" = 5, "s2n" = seq(0.001, 6, by = 0.05), "N" = N, dep = FALSE)
Bias2 = simBias(sett2)

sett3 <- list(alpha = 1, beta = -1, "sig" = 6, "rho0" = 5, "rho1" = 1, "c" = 0, "s2n" = seq(0.001, 6, by = 0.05), "N" = N, dep = FALSE)
Bias3 = simBias(sett3)

sett4 <- list(alpha = 1, beta = -1, "sig" = 6, "rho0" = 0, "rho1" = 3, "c" = 0, "s2n" = seq(0.001, 6, by = 0.05), "N" = N, dep = FALSE)
Bias4 = simBias(sett4)

sett5 <- list(alpha = 1, beta = -1, "sig" = 6, "rho0" = 0, "rho1" = 1, "c" = 0, "s2n" = seq(0.001, 6, by = 0.05), "N" = N, dep = TRUE)
Bias5 = simBias(sett5)

# ------------------------------------------------------------------------
# Do the figures
# ------------------------------------------------------------------------
# Plim Alpha+Beta independent
data.df = data.frame(sett1$s2n, Bias1[, 3], Bias2[, 3], Bias3[, 3], Bias4[, 3])
colnames(data.df) <- c("s2n", "Baseline", "Larger threshold", "Larger intercept", "Larger slope")
melted.df = melt(data.df, id.vars="s2n")

g <- ggplot(data=melted.df, aes(x=s2n, y=value, colour=variable, linetype = variable, shape = factor(variable))) +geom_line(size = 0.8)
g <- g + scale_color_manual(values=c("black", "blue", "magenta", "indianred3", "lightgoldenrod3", "gray60"))
g <- g + scale_linetype_manual(values = c("dotted", "longdash", "dotdash", "1F")) 
g <- g + geom_hline(yintercept=c(sett1$alpha+sett1$beta), linetype="solid", colour = "black", size = 0.8)
g <- g + ggtitle("") + ylab(bquote(paste("Probability limit ", alpha[ols]+beta[ols], "", sep =""))) + xlab(bquote(paste("Signal-to-noise ratio (", sigma[pi]^2/sigma[omega]^2, ")", sep ="")))
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_continuous(limits = c(0, 6), expand = c(0, 0))+scale_y_continuous(limits = c(-0.1, 0.5), expand = c(0, 0))  
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,.6))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_C1a.pdf", width = 11, height = 10, units="cm")

# Plim Alpha+Beta correlated
data.df = data.frame(sett1$s2n, Bias1[, 3], Bias5[, 3])
colnames(data.df) <- c("s2n", "Baseline",  "Correlated measurement error")
melted.df = melt(data.df, id.vars="s2n")

g <- ggplot(data=melted.df, aes(x=s2n, y=value, colour=variable, linetype = variable, shape = factor(variable))) +geom_line(size = 0.8)
g <- g + scale_color_manual(values=c("black", "blue", "magenta", "indianred3", "lightgoldenrod3", "gray60"))
g <- g + scale_linetype_manual(values = c("dotted", "longdash", "dotdash", "1F")) 
g <- g + geom_hline(yintercept=c(sett1$alpha+sett1$beta), linetype="solid", colour = "black", size = 0.8)
g <- g + ggtitle("") + ylab(bquote(paste("Probability limit ", alpha[ols]+beta[ols], "", sep =""))) + xlab(bquote(paste("Signal-to-noise ratio (", sigma[pi]^2/sigma[omega]^2, ")", sep ="")))
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_continuous(limits = c(0, 6), expand = c(0, 0))+scale_y_continuous(limits = c(-1, 6), expand = c(0, 0))  
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,.55))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_C1b.pdf", width = 11, height = 10, units="cm")

# Plim Beta independent
data.df = data.frame(sett1$s2n, Bias1[, 2], Bias2[, 2], Bias3[, 2], Bias4[, 2])
colnames(data.df) <- c("s2n", "Baseline", "Larger threshold", "Larger intercept", "Larger slope")
melted.df = melt(data.df, id.vars="s2n")

g <- ggplot(data=melted.df, aes(x=s2n, y=value, colour=variable, linetype = variable, shape = factor(variable))) +geom_line(size = 0.8)
g <- g + scale_color_manual(values=c("black", "blue", "magenta", "indianred3", "lightgoldenrod3", "gray60"))
g <- g + scale_linetype_manual(values = c("dotted", "longdash", "dotdash", "1F")) 
g <- g + geom_hline(yintercept=c(sett1$beta), linetype="solid", colour = "black", size = 0.8)
g <- g + ggtitle("") + ylab(bquote(paste("Probability limit ", beta[ols], "", sep =""))) + xlab(bquote(paste("Signal-to-noise ratio (", sigma[pi]^2/sigma[omega]^2, ")", sep ="")))
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_continuous(limits = c(0, 6), expand = c(0, 0))+scale_y_continuous(limits = c(-1.1, 0), expand = c(0, 0))  
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,.6))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_1b.pdf", width = 11, height = 10, units="cm")

# Plim Beta correlated
data.df = data.frame(sett1$s2n, Bias1[, 2], Bias5[, 2])
colnames(data.df) <- c("s2n", "Baseline",  "Correlated measurement error")
melted.df = melt(data.df, id.vars="s2n")

g <- ggplot(data=melted.df, aes(x=s2n, y=value, colour=variable, linetype = variable, shape = factor(variable))) +geom_line(size = 0.8)
g <- g + scale_color_manual(values=c("black", "blue", "magenta", "indianred3", "lightgoldenrod3", "gray60"))
g <- g + scale_linetype_manual(values = c("dotted", "longdash", "dotdash", "1F")) 
g <- g + geom_hline(yintercept=c(sett1$beta), linetype="solid", colour = "black", size = 0.8)
g <- g + ggtitle("") + ylab(bquote(paste("Probability limit ", beta[ols], "", sep =""))) + xlab(bquote(paste("Signal-to-noise ratio (", sigma[pi]^2/sigma[omega]^2, ")", sep ="")))
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_continuous(limits = c(0, 6), expand = c(0, 0))+scale_y_continuous(limits = c(-2, 10), expand = c(0, 0))  
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,.55))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_1d.pdf", width = 11, height = 10, units="cm")

# Plim Alpha independent
data.df = data.frame(sett1$s2n, Bias1[, 1], Bias2[, 1], Bias3[, 1], Bias4[, 1])
colnames(data.df) <- c("s2n", "Baseline", "Larger threshold", "Larger intercept", "Larger slope")
melted.df = melt(data.df, id.vars="s2n")

g <- ggplot(data=melted.df, aes(x=s2n, y=value, colour=variable, linetype = variable, shape = factor(variable))) +geom_line(size = 0.8)
g <- g + scale_color_manual(values=c("black", "blue", "magenta", "indianred3", "lightgoldenrod3", "gray60"))
g <- g + scale_linetype_manual(values = c("dotted", "longdash", "dotdash", "1F")) 
g <- g + geom_hline(yintercept=c(sett1$alpha), linetype="solid", colour = "black", size = 0.8)
g <- g + ggtitle("") + ylab(bquote(paste("Probability limit ", alpha[ols], "", sep =""))) + xlab(bquote(paste("Signal-to-noise ratio (", sigma[pi]^2/sigma[omega]^2, ")", sep ="")))
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_continuous(limits = c(0, 6), expand = c(0, 0))+scale_y_continuous(limits = c(0, 1.1), expand = c(0, 0))  
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,.05))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_1a.pdf", width = 11, height = 10, units="cm")

# Plim Alpha correlated
data.df = data.frame(sett1$s2n, Bias1[, 1], Bias5[, 1])
colnames(data.df) <- c("s2n", "Baseline",  "Correlated measurement error")
melted.df = melt(data.df, id.vars="s2n")

g <- ggplot(data=melted.df, aes(x=s2n, y=value, colour=variable, linetype = variable, shape = factor(variable))) +geom_line(size = 0.8)
g <- g + scale_color_manual(values=c("black", "blue", "magenta", "indianred3", "lightgoldenrod3", "gray60"))
g <- g + scale_linetype_manual(values = c("dotted", "longdash", "dotdash", "1F")) 
g <- g + geom_hline(yintercept=c(sett1$alpha), linetype="solid", colour = "black", size = 0.8)
g <- g + ggtitle("") + ylab(bquote(paste("Probability limit ", alpha[ols], "", sep =""))) + xlab(bquote(paste("Signal-to-noise ratio (", sigma[pi]^2/sigma[omega]^2, ")", sep ="")))
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_continuous(limits = c(0, 6), expand = c(0, 0))+scale_y_continuous(limits = c(-5, 2), expand = c(0, 0))  
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,.25))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_1c.pdf", width = 11, height = 10, units="cm")

