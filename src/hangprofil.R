# INFOS ########################################################################
#                                                                              #
#   File:       hangprofil.R                                                   #
#   Topic:      Auswertung Hangmessung                                         #
#   Project:    P2_Standortkundliche Feldmethoden                              #
#                                                                              #
#______________________________________________________________________________#

# INSTALL AND LOAD PACKAGES ####################################################

# set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(readxl)
# PREPARE DATA #################################################################

tbl_frankenhaag <- read_xlsx("Hangprofil.xlsx", sheet = 1, range = "A4:E22") #Einlesen der Daten 
tbl_frankenhaag <- tbl_frankenhaag[,c(2,5)] # FIXME Aktualisierung der Ausgangstabelle

tbl_neubuerg <- read_xlsx("Hangprofil.xlsx", sheet = 2, range = "A4:B12") #Einlesen der Daten 

referenzhoehe <- 430
names(tbl_frankenhaag) <- c("Distanz", "Winkel") # Umbenennung der Tabellenüberschriften

head(tbl_frankenhaag)

# AUSWEWRTUNG ##################################################################
attach(tbl_frankenhaag)     # Speicherung der beiden Spalten im golbal env
vc_length = Distanz[-1] - Distanz[-length(Distanz)] # Berechnung der Strecken zwischen den Abschnitten

height <- vc_length * sin(Winkel*(pi/180)) #https://stackoverflow.com/questions/21402259/radian-measure-in-sin-in-r
height = c(0,height[-length(height)])
cbind(tbl_frankenhaag, height)

sumheight = 0
for(i in 2:length(height)){
    sumheight[i] =  sum(sumheight[i-1], -height[i], na.rm = TRUE) 
}
sumheight_ges <- sumheight + referenzhoehe





## Plot ========================================================================
plot(sumheight_ges~Distanz , type="o" , lwd=3, col=1 , 
     ylim = c(min(sumheight_ges)-10, referenzhoehe) , ylab="Höhe über NN (m)",
     xlim = c(0, max(Distanz)+10),  xlab="Strecke (m)", 
     main = "Höhenprofil Frankenhaag",  bty="l" , pch=20 , cex=1)
abline(h=seq(350,430,10) , col="grey", lwd=0.8)


detach(tbl_frankenhaag)
# CLEAN UP #####################################################################

# clear Environment
rm(list=ls())

# clear packages
detach("package:datasets", unload = TRUE) # For base-packages

# reset plot window
par(mfrow = c(1,1))

# clear plots
dev.off() # But only if there is a plot

# clear console
cat("\014") # ctrl+L



