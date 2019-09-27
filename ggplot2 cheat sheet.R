
#install the packages using install.packages()

library(ggplot2)
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(extrafont)
library(MAP)
ggplot(data=USArrests)

#1
a <- USArrests %>% ggplot() 
a + geom_area(aes(Murder,Assault),fill="red",color="blue",alpha=0.5, linetype= "dashed", size=1) +
  ggtitle("Area Plot")

#2
a + geom_density(aes(UrbanPop),fill= "blue",col="black",alpha=0.7, linetype= "solid", size=1.5, position = "stack") +
  ggtitle("Density Plot")

#3
a + geom_dotplot(aes(Rape),fill= "blue",col="black",alpha=1, linetype= "dotted",binwidth = 1.5,stackdir = "center") +
  ggtitle("Dot Plot")

#4
a + geom_freqpoly(aes(Assault),col="green",alpha=1, linetype= "solid", size=1, binwidth=10) +
  ggtitle("Dot Plot")

#5
a + geom_histogram(aes(x=Murder,y=..density..), fill="magenta",col="black",alpha=1, position = "identity", linetype= "solid", size=1, binwidth=0.5) +
  scale_x_continuous() +
  ggtitle("Histogram")

#6
a+geom_bar(aes(UrbanPop),fill= "green",col="white",alpha=1, linetype="dashed",width = 1.5,position = "jitter") +
  ggtitle("Bar Plot")


#7
ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))

values <- data.frame(
  id = ids,
  value = c(7, 7.2,7.5, 7.4, 7.08, 7.8)
)

locations <- data.frame(
  id = rep(ids, each = 3),
  x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
        0.5, 1.2, 2.5, 1.3),
  y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
        2.2, 2.1, 1.7, 3.2)
)

polygroup <- merge(values, locations, by = c("id"))
b <- ggplot(polygroup,aes(x=x,y=y))
b+geom_polygon(aes(fill= value,group=id), col="black",alpha=1, linetype="dashed",position = "identity") +
  ggtitle("Polygon")


#8
a <- USArrests %>% ggplot() 
a + geom_blank(aes(Murder,Rape), position = "identity")+
ggtitle("Blank") 

#9
a + geom_jitter(aes(Assault,UrbanPop), alpha=1, fill="red", col="orange",width=0.5, height = 0.5, size = 1.5, shape = 23) +
 ggtitle("Jitter")  

#10
a + geom_point(aes(Rape,UrbanPop), alpha=1, fill="dark green", col="orange", size = 1.5, shape = 3) +
  ggtitle("Point")  

#11
a + geom_quantile(aes(Assault,Murder), alpha=1, linetype="solid", col="sky blue", size = 2, quantiles =0.5) +
  geom_point(aes(Assault,Murder)) +
  ggtitle("Quantile")  

#12
a + geom_rug(aes(UrbanPop,Murder),sides="trbl", alpha=1, col="yellow", size = 4.5, position = "identity") +
  geom_point(aes(UrbanPop,Murder), color = "magenta",alpha = 1) +
  ggtitle("Rug")  

#13
a + geom_smooth(aes(Murder,Rape), alpha=1, fill="dark green", col="orange", size = 1.5, linetype = "dashed") +
  ggtitle("Smooth")  

#14
a + geom_text(aes(Assault, Rape, label=rownames(USArrests)),check_overlap = TRUE,family="TT Times New Roman",lineheight=1,angle=45,alpha=1,col="blue",size=3,hjust=0,nudge_x=0.5) +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle("Text")  
  
#15
a + geom_bar(aes(Murder),fill= "sky blue",col="sky blue",alpha=1, linetype="solid",size = 2,position = "identity") +
  scale_x_continuous()+
  scale_y_continuous()+
  ggtitle("Bar Plot")

#16
c <- USArrests %>% ggplot(aes(Rape,UrbanPop))
c + geom_boxplot(aes(group=cut_width(Rape,0.25)),varwidth = TRUE,outlier.shape=NA,outlier.color = "red",position = "identity",alpha=0.1,col="sky blue", size = 5,linetype = "dotted") +
   geom_jitter(width = 0.2) +
   ggtitle("Box Plot")

#17
a + geom_dotplot(aes(Assault, col=factor(skew)),fill="pink",col="blue",alpha=1,binwidth=10,stackdir = "center") +
  ggtitle("Dot Plot")


#18
a + geom_violin(aes(Murder,Rape),linetype="dotted",scale = "area",fill="grey",size=1,col="red",position = "jitter",trim=TRUE, show.legend = TRUE) +
  ggtitle("Violin")

#19
h <- ggplot(diamonds, aes(cut, color))
h + geom_jitter(alpha=1, col="red", shape=20) +
  ggtitle("Discrete Jitter")

#20
d <- ggplot(economics, aes(date,unemploy))
d + geom_path(lineend = "butt", linejoin = "round", linemitre = 1, alpha=1, col = "green", linetype="solid") +
  ggtitle("Path")

#21
d + geom_ribbon(aes(ymin=unemploy-900,ymax=unemploy+900), alpha=0.54,fill="red", col="blue", linetype="dashed") +
  ggtitle("Ribbon")


#22
e <- ggplot(seals, aes(x = long, y = lat))
e + geom_segment(aes( xend = long + delta_long,  yend = lat + delta_lat),col="dark green") +
  ggtitle("Segment")

#23
e + geom_rect(aes(xmin = long, ymin = lat, xmax= long + delta_long, ymax = lat + delta_lat), col="black") +
  ggtitle("Rect")

#24
i <- ggplot(economics, aes(date, unemploy))
i + geom_bin2d(binwidth = c(5, 0.5), col="magenta", fill = "blue", linetype = "dashed",size=5) +
  ggtitle("BIN2D")


#25
i + geom_density2d(alpha=0.5, col= "blue", size=5 ) +
  ggtitle("density2d")


#26
i + geom_area(alpha=0.5, col= "blue", size=1, fill="light blue") +
  ggtitle("Area")

#27
i + geom_line(alpha=0.5, col= "green", size=1.5, linetype="dashed") +
  ggtitle("Line")

#28
i + geom_step(alpha=1, col= "red", size=2, linetype="dotted") +
  ggtitle("step")

#29
df <- data.frame(grp = c("A", "B"), fit = 4:5, se = 1:2)
k <- ggplot(df, aes(grp, fit, ymin = fit-se, ymax = fit+se))
k + geom_crossbar(fatten=0.5,fill="red",col="yellow",linetype="dotted",width=0.4) +
  ggtitle("crossbar")

#30
k + geom_errorbar(fill="blue",col="red",linetype="dashed",width=3, size=1) +
  ggtitle("errorbar")

#31
k + geom_linerange(col="grey",linetype="solid", size=2) +
  ggtitle("linerange")

#32
k + geom_pointrange(col="blue",linetype="dotted", size=2,shape=15, fill="red") +
  ggtitle("Pointrange")

#33
data <- data.frame(murder = USArrests$Murder,
                   state = tolower(rownames(USArrests)))
map <- map_data("state")
l <- ggplot(data, aes(fill = murder))
l + geom_map(aes(map_id = state), map = map, alpha=0.8,col="light blue",size=1,linetype="dashed") + expand_limits(x = map$long, y = map$lat)+
  ggtitle("US Map")


