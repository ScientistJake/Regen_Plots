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
pdf(file = "spineplot.pdf", width = 6, height = 6)
spineplot(df2$Treatment, factor(df2$Stage), col = c("gray50",  "blue", "aliceblue", "steelblue", "cornflowerblue"))
dev.off()
