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

tbl_frankenhaag <- read_xlsx("Hangprofil.xlsx", sheet = 1, range = "A4:B22") #Einlesen der Daten 
tbl_neubuerg <- read_xlsx("Hangprofil.xlsx", sheet = 2, range = "A4:B12") #Einlesen der Daten 

attr(tbl_neubuerg, "referenzhoehe") <- 550
attr(tbl_frankenhaag, "referenzhoehe") <- 430

names(tbl_frankenhaag) <- c("Distanz", "Winkel") # Umbenennung der Tabellenüberschriften
names(tbl_neubuerg) <- c("Distanz", "Winkel") # Umbenennung der Tabellenüberschriften

head(tbl_frankenhaag)

# FUNCTIONS  ###################################################################

getheight <- function(distance, degree) {
    vc_length = distance[-1] - distance[-length(distance)] # Berechnung der Strecken zwischen den Abschnitten
    
    height <- vc_length * sin(degree*(pi/180)) #https://stackoverflow.com/questions/21402259/radian-measure-in-sin-in-r
    height = c(0,height[-length(height)])
    
    return(height)
}

getgesheight <- function(height, refheight){
    sumheight = refheight
    for(i in 2:length(height)){
        sumheight[i] =  sumheight[i-1] - height[i]
    }
    return(sumheight)
}

getplot <- function(distance, gesheight, refheight, title){
    plot(gesheight~distance , type="o" , lwd=3, col=1 , 
         ylim = c(min(gesheight)-10, refheight) , ylab="Höhe über NN (m)",
         xlim = c(0, max(distance)+10),  xlab="Strecke (m)", 
         main = title,  bty="l" , pch=20 , cex=1)
    abline(h=seq(max(round(refheight, -1)), min(gesheight)*0.9,-10) , col="grey", lwd=0.8)
}

# AUSWERTUNG  ###################################################################


nb_h <- getheight(tbl_neubuerg$Distanz, tbl_neubuerg$Winkel)
nb_gh <- getgesheight(nb_h, attributes(tbl_neubuerg)$referenzhoehe)

fh_h <- getheight(tbl_frankenhaag$Distanz, tbl_frankenhaag$Winkel)
fh_gh <- getgesheight(fh_h, attributes(tbl_frankenhaag)$referenzhoehe)



## Plot ========================================================================
getplot(tbl_neubuerg$Distanz, nb_gh, attributes(tbl_neubuerg)$referenzhoehe, 
        "Höhenprofil Neubürg")

getplot(tbl_frankenhaag$Distanz, fh_gh, attributes(tbl_frankenhaag)$referenzhoehe, 
        "Höhenprofil Frankenhaag")

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