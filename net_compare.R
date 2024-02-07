# barchart of juveniles vs adults by site
#load packages!

library(reshape2)
library(ggplot2)
library(dplyr)

#reshape and split original dataset
allages_nets_CPUE <- melt(allages_nets_CPUE, id.vars=c("site", "month", "net"))
numb <- allages_nets_CPUE[1:2172,]
cpue <- allages_nets_CPUE[2173:4344,]

##add names
names(numb)[4:5] <- c("matcode", "total")
names(cpue)[4:5] <- c("matcode", "cpue")

#subset down to the correct maturity codes, month and nets
numb <- subset(numb, matcode != c("adult#", "larv#"))

cpue <- subset(cpue, matcode %in% c("adultCpue","juvCpue"))
cpue <- subset(cpue, month != "5")
cpue <- subset(cpue, net %in% c("HOPS", "GIL10"))

#concatenate net and matcode for plotting, rearrange
cpue$net_mcode <- paste(cpue$net, cpue$matcode)
cpue <- subset(cpue, select= -c(net,matcode))
cpue_june <-subset(cpue, month =="6")            
cpue_july <-subset(cpue, month == "7")
               
#multiline plot of net/maturity code for each month
##NOT USEFUL HERE BUT GOOD CODE
hop_gil <- ggplot(cpue, aes(x= site, y=cpue, colour=net_mcode, group=net_mcode)) +
  facet_wrap(~month)+
  geom_point()+
  geom_line()+
  theme(panel.background= element_rect(color="black"),
        legend.position = "bottom", axis.title.y= element_text(size=14)) +
  labs(x=" ", y="CPUE", colour="Net and Maturity") +
  scale_fill_brewer(palette= "BrBG")
hop_gil

#export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "HoopVGilLINE.png", units = "in", width = 8, height = 6, res=300)
hop_gil
dev.off()

# Stacked barplot of hoop versus gill
#vector for facet names 
month_names <- c("6"="June", "7"="July")
hop_gil_bar <- ggplot(data=cpue) + 
  facet_wrap(~month, labeller = as_labeller(month_names))+
  geom_bar(mapping= aes(x= site, y= cpue, fill= net_mcode, group=net_mcode),
           position="stack", 
           stat="identity")+
  theme(panel.background= element_rect(fill="white", color="black"),
        legend.position = "bottom", 
        axis.title.y= element_text(size=14)) +
  labs(x=" ", 
       y="CPUE", 
       fill="Net and Maturity")+
  scale_fill_brewer(palette= "BrBG")
hop_gil_bar 
  
#export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "hoopVgilBAR.png", units = "in", width = 8, height = 6, res=300)
hop_gil_bar
dev.off()

