#This script was used to the data analysis in the paper: 
#"title + autors"
#*Corresponding author: awegier@st.ib.unam.mx 

# packages 
library(lme4)
library(multcomp)

### Outliers detection --- Cooks distance as a multivariate detection method ### 

FinalT2 <- read.delim("clipboard") 
FinalT2 <- read.csv("FinalT2.csv", header=TRUE, sep=",")

mod <- lm(P.S ~ ., data=FinalT2)
cooksd <- cooks.distance(mod)

#Plot with cutoff line
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

#Influential observations
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
head(FinalT2[influential, ]) # influential observations

#Generalized Linear Model (GLM). The family was chosen according to the distribution of each subset

model1 <-glm(value~type, data=data, family = x)
model1 <- lmer(P.S~Tratamiento+(1|Ind), data= FinalOax)
anova(model1, test= "Chi")
summary(model1)

Tukey_glm<- glht(GLMM, linfct = mcp(Tratamiento= "Tukey")) #comparacion multiple 
summary(Tukey_glm)
plot(Tukey_glm)


#### PLOTS ####

library(ggplot2)

p1<- ggplot(Base, aes(Tratamiento, Factor_1, fill= Tratamiento))+ 
  geom_boxplot() +xlab("Treatment")+
  theme(axis.text.x=element_text(angle=90))+guides(fill=FALSE)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+ 
  scale_fill_manual(values=c("#e41a1c",
                             "#377eb8",
                             "#4daf4a",
                             "#984ea3",
                             "#ff7f00"))

p1

# multiplot fuction #

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p2, p3, p1, p4,p6, p7, p5, p8, cols=2)

###################### E N D #####################################

#### plots germination ####

library(extrafont)
library(wesanderson)

# GERMINACION #
# WILD/DOMESTICATED/INTROGRESSION # 

germination<-ggplot(base_1, aes(x = Days, y = Percent, color = Traits))+ geom_line() + 
  geom_point(na.rm = TRUE)+ scale_size_area()+
  geom_hline(yintercept = .50, linetype = "longdash", color="black") + 
  geom_hline(yintercept = .95, linetype = "longdash", color="black") + 
  scale_x_continuous(breaks = c(1,10,20,30,40,50,60,70)) +
  scale_color_manual(values=wes_palette(n=5, name="Darjeeling"))+ 
  theme(legend.position="none",
        legend.justification = c("right"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.background = element_rect(fill="white", size=.5, linetype="solid", colour="black"),
        legend.text = element_text(colour="black", size = 12),
        legend.title = element_text(colour="black", size=12), axis.title.x=element_blank(),
        axis.text.x=element_blank())

germination

# GERMINACION #
# AUTOGAMIA/OPEN-POLLINATION/XENOGAMIA #

xen<-ggplot(XA, aes(x = Days, y = Porcent, color = Traits)) +geom_line() + 
  geom_point(na.rm = TRUE)+ scale_size_area()+
  geom_hline(yintercept = .50, linetype = "longdash", color="black") + 
  geom_hline(yintercept = .95, linetype = "longdash", color="black") + 
  scale_x_continuous(breaks = c(1,10,20,30,40,50,60,70)) +
  scale_color_manual(values=wes_palette(n=3, name="Cavalcanti"))+ 
  theme(legend.position="none")

xen

multiplot(traits, xen)

############# END #############

