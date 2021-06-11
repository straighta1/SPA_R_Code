
#BARPLOTS

Escapement1_Plot <- function(data1 = Returns1a, data2= Returns1b, axis_labs = Escape1_axis_labs, legend_names = Escape1_legend_names, cols = pal1, main = Main, PST = "NA", Habitat = "NA", Percent_Rem = "Yes", Hab_Labs = c("25% SMSY", "85% SMSY")){ 
  par(mar = c(6.5,4.5,2,4))
    if(PST != "NA"){
      Lines <- c(PST,NA)
      legNames <-c(legend_names, "PST Escape Goal")
    } else {
    if(length(Habitat) >1){
      Lines <- Habitat
      legNames <- c(legend_names, Hab_Labs)
    } else {
      Lines <- c(NA,NA)
      legNames <- legend_names
    }}    
    if(Percent_Rem == "N"){
      data2 <- "NA"
    }
    
    xx<- barplot(data1, 
                ylab = axis_labs[2] , xlab = axis_labs[1] ,
                col = cols[1:nrow(data1)],
                ylim = c(0,max(colSums(data1, na.rm =T))),
                yaxt = "n")
    abline(h = Lines, 
                col = c("Red","Green"), lwd = 2, lty = 2)
    par(new = T)
    axis(2, at = axTicks(2), 
                las =1, cex.axis = .6, line = 0, 
                labels = formatC(axTicks(2),format = "d", big.mark= ","))
    
    if(length(data2) > 1){
    par(new = T)
    plot(xx+.5,data2$Percent,  
                type = "l", axes = FALSE,
                ylim= c(0, max(data2$Percent, na.rm = T)), xlim = c(min(xx),max(xx)+1),
                lwd = 2, col = "black",
                xlab = "", ylab = "",  
                yaxs = "i")
    axis(4, las = 1, 
                cex.axis = .7, line = 0)
    mtext("% of Run Removed", 
                side = 4, line = 2.5)
    legNames   <- legNames[2:length(legNames)]}
    mtext(paste(main,"Escapement", sep = " "), 
                side = 3, cex = 1.5)
    par(xpd = TRUE)
    legend("bottom",inset = c(0,-.55),
                legNames, horiz = TRUE,
                col = c(cols[1:nrow(data1)],"black", "Red", "Green" ),
                pch = c(22,22,NA,NA,NA),
                lwd = c(NA,NA,2,2,2),
                pt.bg  =c(cols[1:nrow(data1)],"black", "Red", "Green"),
                cex = 0.8,bg = "white")
}
#Two Bars - 1 Line
Release_StageT1_Plot <- function(data1 = Releases1, data2 = Tagged, axis_lab = ReleaseT1_axis_labs, cols = pal1, main = Main, Dens=dens1){ 
    par(mar = c(6.5,4.5,1.5,0),xpd = TRUE)
    xx<- barplot(data1, ylab = axis_lab[2] ,xlab = axis_lab[1] ,col = cols[1:(nrow(data1))],
          ylim = c(0,max(colSums(data1),na.rm=T)), yaxt = "n")
    par(new = T)
    axis(2, at = axTicks(2), las =1, cex.axis = .6, line = 0, labels = formatC(axTicks(2),
           format = "d", big.mark= ","))
    par(new = T)
    plot(xx+.5,data2[,2],  type = "l", ylim = c(0,max(colSums(data1),na.rm=T)),  
          axes = FALSE, yaxs = "i",xlim = c(min(xx),max(xx)+1),lwd = 2, xlab = "", ylab = "", col = "black")
  mtext(paste(main,"Releases", sep = " "), side = 3, cex = 1.5)
  legend("bottom",inset = c(0,-.55), c("Tagged",gsub("Release.", "", rownames(data1))), 
          horiz = TRUE,
          col = c("black",cols[1:nrow(data1)]),
         pch = 22,
         pt.bg  = c("black",cols[1:nrow(data1)]),
         cex = 0.8,
         bg = "white")
}

Release_Site1_Plot <- function(data1 = Releases1, axis_lab = ReleaseT1_axis_labs, cols = pal1, main = Main, Dens=dens1){ 
    par(mar = c(6.5,4.5,1.5,0),xpd = TRUE)
    xx<- barplot(data1, ylab = axis_lab[2] ,xlab = axis_lab[1] ,col = cols[1:(nrow(data1))], density = Dens[1:(nrow(data1))], 
          ylim = c(0,max(colSums(data1),na.rm=T)), yaxt = "n")
    par(new = T)
    axis(2, at = axTicks(2), las =1, cex.axis = .6, line = 0, labels = formatC(axTicks(2),
           format = "d", big.mark= ","))
    par(new = T)
  mtext(paste(main,"Releases by Site", sep = " "), side = 3, cex = 1.5)
  legend("bottom",inset = c(0,-.55), gsub("Release.", "", rownames(data1)), 
          ncol = 4,
          #col = ,
          density = Dens[1:(nrow(data1))],
         #pch = 22,
          fill =cols[1:nrow(data1)],
         pt.bg  = cols[1:nrow(data1)],
         cex = 0.8,
         bg = "white")
}
#EggFry1_Plot
EggFry1_Plot <- function(data1 = EggFry1, data2 = Fry1, axis_labs = EggFry_axis_labs, legend_names = EggFry_legend_names, cols = pal1,  main = Main, Scaling = 1){ 
  par(mar = c(6.5,4,1.5,4.5), xpd = TRUE)
   
   if(Scaling == 1000){
    data2 = data2/Scaling 
    axis_labs[2] <- paste(axis_labs[2],"(Thousands)", sep = " ")
   } 
   if(Scaling == 1000000){
     data2 = data2/Scaling 
     axis_labs[2] <- paste(axis_labs[2],"(Millions)", sep = " ")
   }
   xx<- barplot(data2, ylab = axis_labs[2] ,xlab = axis_labs[1] ,col = cols[1],
          ylim = c(0,max(data2*1.1, na.rm =T)), yaxt = "n")
  par(new = T)
    axis(2, at = axTicks(2), las =1, cex.axis = .6, line = 0, labels = formatC(axTicks(2),
           format = "d", big.mark= ","))
  par(new = T)
    plot(xx+.5,data1[,"Surv"],  type = "b", ylim= c(0, max(data1[,"Surv"], na.rm = T)), 
          axes = FALSE, xlim = c(min(xx),max(xx)+1),lwd = 2, xlab = "", ylab = "", col = "black", yaxs = "i" )
    axis(4, las = 1, cex.axis = .7, line = 0)
    mtext(axis_labs[3], side = 4, line = 2.5)
    mtext(paste(main,"Releases", sep = " "), side = 3, cex = 1.5)
    legend("bottom",inset = c(0,-.55),legend_names,
          horiz = TRUE, 
          col = c(cols[1],"black"),
         pch = c(22,NA),
         lwd = c(NA,2),
         pt.bg  = cols[1],
         cex = 0.8,
         bg = "white")
}


#1Bar 1 line
SR_ER1_Plot <- function(data1 = S_R1_Bar, data2 = S_R1, axis_labs = SR_ER1_axis_labs, legend_names = SR_ER1_legend_names, cols = pal1, main = Main){ 
  par(mar = c(6.5,4,1.5,4.5), xpd = TRUE)
    xx<- barplot(data1, ylab = axis_labs[2] ,xlab = axis_labs[1] ,col = cols[1],
          ylim = c(0,max(data1*1.1, na.rm =T)), yaxt = "n")
    par(new = T)
    axis(2, at = axTicks(2), las =1, cex.axis = .6, line = 0, labels = formatC(axTicks(2),
           format = "d", big.mark= ","))
  par(new = T)
    plot(xx+.5,data2[,"S_Rate"],  type = "b", ylim= c(0, max(data2[,"S_Rate"], na.rm = T)), 
          axes = FALSE, xlim = c(min(xx),max(xx)+1),lwd = 2, xlab = "", ylab = "", col = "black", yaxs = "i" )
    axis(4, las = 1, cex.axis = .7, line = 0)
  mtext(axis_labs[3], side = 4, line = 2.5)
  mtext(paste(main,"Survival & Exploitation", sep = " "), side = 3, cex = 1.5)
  legend("bottom",inset = c(0,-.55),legend_names,
          horiz = TRUE, 
          col = c(cols[1],"black"),
         pch = c(22,NA),
         lwd = c(NA,2),
         pt.bg  = cols[1],
         cex = 0.8,
         bg = "white")
}

#This function plots broodstock with holding morts and other morts using function BroodStock1
BroodStock1_Plot<- function(data1 = Brood1, axis_labs = Brood1_axis_labs, legend_names = Brood1_legend_names, cols = pal1, main = Main){ 
  par(mar = c(6.5,4.5,1.5,0), xpd = TRUE)
    xx<- barplot(data1, ylab = axis_labs[2] ,xlab = axis_labs[1] ,col = cols[1:nrow(data1)],
          ylim = c(0,max(colSums(data1), na.rm =T)), yaxt = "n")
    par(new = T)
    axis(2, at = axTicks(2), las =1, cex.axis = .6, line = 0, labels = formatC(axTicks(2),
           format = "d", big.mark= ","))
  mtext(paste(main,"Broodstock", sep = " "), side = 3, cex = 1.5)
  legend("bottom",inset= c(0,-.55),rownames(data1), 
          horiz = TRUE,
          col = cols[1:nrow(data1)],
         pch = c(22,22,22),
         pt.bg  = cols[1:nrow(data1)],
         cex = 0.8,
         bg = "white")
}

#This function plots broodstock with holding morts and other morts using function BroodStock1
Removals1_Plot<- function(data1 = Brood1, axis_labs = Brood1_axis_labs, legend_names = Brood1_legend_names, cols = pal1, main = Main){ 
  par(mar = c(6.5,4.5,1.5,0), xpd = TRUE)
    xx<- barplot(data1, ylab = axis_labs[2] ,xlab = axis_labs[1] ,col = cols[1:nrow(data1)],
          ylim = c(0,max(colSums(data1), na.rm =T)), yaxt = "n")
    par(new = T)
    axis(2, at = axTicks(2), las =1, cex.axis = .6, line = 0, labels = formatC(axTicks(2),
           format = "d", big.mark= ","))
  mtext(paste(main,"Removals", sep = " "), side = 3, cex = 1.5)
  legend("bottom",inset= c(0,-.55),rownames(data1), 
          horiz = TRUE,
          col = cols[1:nrow(data1)],
         pch = c(22,22,22),
         pt.bg  = cols[1:nrow(data1)],
         cex = 0.8,
         bg = "white")
}


### Barpltos for Age comp
 #AGECWT1 is easily adapted for scale data
  AgeCWT1_Plot <- function(data1 = Age_Bar , axis_labs = AgeCWT1_axis_labs, cols = pal1, main = Main){
                  par(mar = c(6.5,4.5,1.5,0), xpd = TRUE)
                  barplot(data1,
                  col = cols,
                  main = paste(main,"Age Composition", sep = " "),
                  ylab = axis_labs[1],
                  xlab = axis_labs[2],
                  las = 1,
                  #xlim = c(0,ncol(data1)+2.5),
                  legend = TRUE,
                  args.legend = list(x = "bottom", bg = "white", horiz = TRUE, inset= c(0,-.55), cex = .8))
  }

  SexRatio1_Plot <- function(data1 = Sex_Bar , axis_labs = SexRatio_axis_labs, cols = pal1, main = Main){
                  par(mar = c(6.5,4.5,1.5,0), xpd = TRUE)
                  barplot(data1,
                  col = cols,
                  main = paste(main,"Sex Composition", sep = " "),
                  ylab = axis_labs[1],
                  xlab = axis_labs[2],
                  las = 1,
                  #xlim = c(0,ncol(data1)+2.5),
                  legend = TRUE,
                  args.legend = list(x = "bottom", bg = "white", horiz = TRUE, inset= c(0,-.55), cex = .8))
  }

#SCATTER/LINE
    #3var scatter plot with ablines - PNI with pNOB = 1
  PNI1_Plot <- function(type = PNI, data1 = Enhance1, cols = pal1, main = Main, yr1, Lines = PNI_RefLines){
     par(mar = c(6.5,4.5,1.5,0))
     if(type == "PNI"){
      a           <- c("PNI","PNI4","PNIone")
      Line        <- Lines
      axis_labs   <- c( "PNI","Return Year")
      Title       <- "PNI"
      Legend_Names<- c("PNI", "PNI 4 Yr Avg","pNOB = 1","0.5","0.72")
     }else{
      a           <- c("PNI","pNOB","pHOS")
      Line        <- c(NA,NA)
      axis_labs   <- c("Proportion","Return Year")
      Title       <- "Enhanced Contribution"
      Legend_Names<- c("PNI","pNOB", "pHOS")
     }
     plot(data1[,"Year.y"], data1[,a[1]], 
                      ylim = c(0,1), 
                      xlim = c(yr1,max(data1[,"Year.y"])),
                      ylab = axis_labs[1],xlab = axis_labs[2],
                      col = cols[1], type = "b", lwd = 3, las = 1, 
                      pch = 1, panel.first = grid())
     points(x = data1[,"Year.y"], y =  data1[,a[2]], 
                      col = cols[2], type = "b",  lwd = 3) 
     points(x = data1[,"Year.y"], y =  data1[,a[3]], 
                      col = cols[3], type = "b",  lwd = 3) 
     abline(h = Line, col = c("Red","Green"), lwd = 2, lty = 2)
     #points(data1)
     mtext(paste(main,Title, sep = " "), side = 3, cex = 1.5)
    par(xpd = TRUE)
     legend("bottom",inset = c(0,-.55),Legend_Names, horiz = TRUE,
                      col = c(cols[1:3],"Red","Green"), lwd = c(2,2,2,2), cex = 0.7, lty = c(1,1,1,2,2))
   }
  
  PNI1_Plot_Gap <- function(type = PNI, data1 = Enhance1, cols = pal1, main = Main, yr1, Lines = PNI_RefLines, Gfrom, Gto){
     par(mar = c(6.5,4.5,1.5,0))
          if(type == "PNI"){
      a           <- c("PNI","PNI4","PNIone")
      Line        <- Lines
      axis_labs   <- c( "PNI","Return Year")
      Title       <- "PNI"
      Legend_Names<- c("PNI", "PNI 4 Yr Avg","pNOB = 1","0.5","0.72")
     }else{
      a           <- c("PNI","pNOB","pHOS")
      Line        <- c(NA,NA)
      axis_labs   <- c("Proportion","Return Year")
      Title       <- "Enhanced Contribution"
      Legend_Names<- c("PNI","pNOB", "pHOS")
     }
     gap.plot(data1[,xdat], data1[,ydat[1]], 
                      gap=c(Gfrom,Gto), gap.axis="x",
                      ylim = c(0,1), 
                      xlim = c(yr1,max(data1[,xdat])),
                      ylab = axis_labs[1],xlab = axis_labs[2],
                      col = cols[2], type = "b", lwd = 3, las = 1, 
                      pch = 1, panel.first = grid())
      #axis.break(1, Gfrom, breakcol="snow", style="gap")
      axis.break(1, Gfrom*(1+0.02), breakcol="black", style="slash")
      axis.break(3, Gfrom*(1+0.02), breakcol="black", style="slash")
      
      #axis(1, at=Gfrom)

     points(x = data1[,xdat], y =  data1[,ydat[2]], 
                      col = cols[3], type = "b",  lwd = 3) 
     

      abline(h = Lines, col = c("Red","Green"), lwd = 2, lty = 2)
     #points(data1)
    par(xpd = TRUE)
     legend("bottom",inset= c(0,-.55),legend_names, horiz = TRUE,
                      col = c(cols[2],cols[3],"Red","Green"), lwd = c(3,3,2,2), cex = 0.7, lty = c(1,1,2,2))
   }