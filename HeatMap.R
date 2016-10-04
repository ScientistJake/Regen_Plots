#
#
#
# The following code chunks will generate different plots for staged two condition regeneration data
#
#
#


library(ggplot2)
library(data.table)

# This outputs everything to the desktop but you can change this call as you see fir:
setwd("~/Desktop")

# Change the datafile as needed.
# If you want to just try with practice data uncomment the following line:
#df2 <- data.frame(Treatment=c(rep("Control", 12), rep("Treated", 12)), Stage=c(1,2,3,3,3,3,4,4,4,4,4,0,1,1,2,1,2,2,3,4,1,1,2,2))
df2 <- read.table(file='data.txt', sep = '\t', header =T )

levels(df2$Treatment) <- c("Control", "Treated")

wc <- wilcox.test(Stage ~ Treatment, data=df2, alternative = c("greater"), correct =F)
wc$p.value
label <- data.frame(Treatment = c("Treated"),
                    Stage = c(4.5))

if (wc$p.value < 0.05) {
  sig <- c("*")
} else {
  sig <- c("NS")
}

pv <- signif(wc$p.value, digits=4)


# spine plot
#pdf(file = "spineplot.pdf", width = 6, height = 6)
#spineplot(df2$Treatment, factor(df2$Stage), col = c("gray50",  "blue", "aliceblue", "steelblue", "cornflowerblue"))
#dev.off()

# Heatmap
runningcounts.df <- as.data.frame(table(df2$Treatment, df2$Stage))
colnames(runningcounts.df) <- c("Treatment", 'Stage', "Freq")

Con <- runningcounts.df[runningcounts.df$Treatment == "Control",]
Con$Prop <- Con$Freq/sum(Con$Freq)

Treat <- runningcounts.df[runningcounts.df$Treatment == "Treated",]
Treat$Prop <- Treat$Freq/sum(Treat$Freq)

ConTreat <- rbind(Con, Treat)

g1 <- ggplot(ConTreat, aes(Treatment, Stage)) +
  geom_tile(aes(fill = Prop), colour = "black") +
  scale_y_discrete(expand=c(0,1)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  annotate("text", x = c("Treated"), y = 4.55, label = c(paste(sig)), size=8) +
  annotate("text", x = c("Treated"), y = 4.8, label = c(paste(pv)))
ggsave(file = "heatmap.pdf", g1)

#grid
theme_nogrid <- function (base_size = 12, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(panel.grid = element_blank())   
}

g2 <- ggplot(ConTreat, aes(Treatment, Stage)) +
  geom_point(aes(size = Freq, color = Prop, stat = "identity", position = "identity"), shape = 16) + #use shape = 15 for squares
  scale_size_continuous(range = c(3,15)) + 
  scale_color_gradient(low = "white", high = "steelblue") +
  annotate("text", x = c("Treated"), y = 4.25, label = c(paste(sig)), size=8) +
  annotate("text", x = c("Treated"), y = 4.5, label = c(paste(pv)))
theme_nogrid()
#ggsave(file = "gridplot.pdf", g2)

#google river
## Uncomment the next 3 lines to install the developer version of googleVis
#install.packages(c("devtools","RJSONIO", "knitr", "shiny", "httpuv"))
#library(devtools)
#install_github("mages/googleVis")

#violin plot
g3 <- ggplot(df2, aes(x = Treatment, y = Stage, fill=factor(Treatment))) +
  geom_violin(adjust = 0.5) +
  #geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.1, stackratio = 0.8, dotsize =0.5, fill = 'grey') +
  scale_y_continuous(limits=c(0,4.5)) +
  stat_summary(fun.y = 'mean', fun.ymin = 'mean', fun.ymax = 'mean',
               geom = "crossbar", width = 0.5) +
  geom_text(data = label, label = c(paste(sig)), size = 4) +
  annotate("text", x = c("Treated"), y = 4.3, label = c(paste(pv)))
#ggsave(file = "violinplot.pdf", g3)
