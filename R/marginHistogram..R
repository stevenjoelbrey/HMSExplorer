# library
library(ggplot2)
library(ggExtra)


library(PtProcess)
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)

source("R/marginal_plot.R")


# Load test data, this will becoming appended dataframes from different years
AQ_df <- get(load("data/AQS/ozone/ozone_2012.RData"))
AQ_df$X1st.Max.Value <- AQ_df$X1st.Max.Value * 1000

ID <- "01-003-0010-44201-1"
IDMask <- AQ_df$ID == ID

# get rid of rows where plumeMask == NA
hasPlumeFile <- !is.na(AQ_df$plumeMask)

df <- AQ_df[IDMask & hasPlumeFile,]

# classic plot :
p1 = ggplot(df, aes(x=Date.Local, y=X1st.Max.Value, color=plumeMask)) +
     geom_point() +
     theme(plot.margin = unit(c(0,-.5,0,.5), "lines"),
           plot.background = element_blank()) +
     theme(legend.position="none") 
     #ggtitle("Time Series") + 
     #theme(legend.position="none")

p2 <- ggplot(df, aes(x = X1st.Max.Value, colour=plumeMask)) +
      geom_density() + 
      #geom_vline(xintercept = 0, size = 1, colour = "black", linetype = "solid") +
      geom_hline(yintercept = 0, size = 1, colour = "black", linetype = "solid") +
      
      theme(axis.text.y = element_blank(), 
      #axis.ticks.y = element_blank(), 
      axis.title.y = element_blank(),
      plot.margin = unit(c(3,1,0,-1), "lines")) 
      #ggtitle("density estimate")

p2 = p2 + coord_flip()

library(cowplot)
plot_grid(p1, p2, ncol=2, align="h", rel_widths=c(3,1))




gt1 <- ggplotGrob(p1)
gt2 <- ggplotGrob(p2)

#newWidth = unit.pmax(gt1$widths[2:3], gt2$widths[2:3])

# gt1$widths[2:3] = as.list(newWidth)
# gt2$widths[2:3] = as.list(newWidth)

# New gtable with space for the three plots plus a right-hand margin
gt = gtable(widths = unit(c(2, 1, .1), "null"), height = unit(1, "null"))

# Instert gt1, gt2 and gt2 into the new gtable
gt <- gtable_add_grob(gt, gt1, 1, 1)
gt <- gtable_add_grob(gt, gt2, 1, 2)
#gt <- gtable_add_grob(gt, gt2, 1, 3)

grid.newpage()
grid.draw(gt)


#grid.arrange(gt1, gt2, ncol=2)





marginal_plot(x=Date.Local, y=X1st.Max.Value, group = plumeMask, data = AQ_df, 
              lm_show = FALSE, bw = "nrd0", adjust = 1, 
              alpha = 1, plot_legend = T)



plumeMask <- AQ_df$plumeMask



plume_AQ   <- AQ_df[plumeMask,]
noPlume_AQ <- AQ_df[!plumeMask,] 


p <- ggplot() +
  # blue plot
  geom_point(data=plume_AQ, aes(x=Date.Local, y=X1st.Max.Value)) + 
  geom_smooth(data=plume_AQ, aes(x=Date.Local, y=X1st.Max.Value), fill="blue",
              colour="darkblue", size=1) +
  # red plot
  geom_point(data=noPlume_AQ, aes(x=Date.Local, y=X1st.Max.Value)) + 
  geom_smooth(data=noPlume_AQ, aes(x=Date.Local, y=X1st.Max.Value), fill="red",
              colour="red", size=1)


p1 <- ggplot(plume_AQ, aes(x=Date.Local, y=X1st.Max.Value, color="red")) +
             geom_point() +
             theme(legend.position="left")

p2 <- ggplot(noPlume_AQ, aes(x=Date.Local, y=X1st.Max.Value, colors="blue")) +
             geom_point() +
             theme(legend.position="none")

ggMarginal(p, margins = 'y', color="red", size=4)
ggMarginal(p2, margins = 'y', color="blue", size=4)

# The mtcars dataset is proposed in R
head(mtcars)

# classic plot :
p=ggplot(mtcars, aes(x=wt, y=mpg, color=cyl, size=cyl)) +
  geom_point() +
  theme(legend.position="none")



# with marginal histogram
ggMarginal(p, type="histogram")

# marginal density
ggMarginal(p, type="density")

# marginal boxplot
ggMarginal(p, type="boxplot")


# Set relative size of marginal plots (main plot 10x bigger than marginals)
ggMarginal(p, type="histogram", size=10)

# Custom marginal plots:
ggMarginal(p, type="histogram", fill = "slateblue", xparams = list(  bins=10))

# Show only marginal plot for x axis
ggMarginal(p, margins = 'y', color="purple", size=4)