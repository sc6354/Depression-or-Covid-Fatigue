
library(ggplot2)
library(scatterpie)
#library(RColorBrewer)
library(randomcoloR)
# Define the number of colors you want
nb.cols <- 17
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)
colors <- c('#808080', '#000000', '#FF0000', '#800000', '#808000', 
            '#008000', '#008080' , '#0000FF', '#000080', '#FF00FF', 
            '#800080', '#FA8800' , '#419474', '#FFD700', '#8000FF', 
            '#00BFFF', '#7B7D7D')
      
      
# Read in topic model 
df <- read.csv('/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/trainingTM.csv', header = TRUE)
col_list <- colnames(df)[2:16]
# high resolution tiff image
#tiff("/Users/susanchen/Documents/Depression-or-Covid-Fatigue/data/scatterpie.png", units="in", width=12, height=9, res=500)
# plot
ggplot() + 
      geom_scatterpie(aes(x=x_tsne, y=y_tsne, group=row_id, r=max_topic_prob), data=df, cols=col_list, color=NA, alpha=0.7) + 
      coord_equal() + 
      geom_label() + 
      ggtitle("Training set Topics Representation") + 
      xlab("") + ylab("") + labs(subtitle="Topics Colored and Sized by Top Topic Probability") +
      theme_minimal() + 
      scale_fill_manual(values = colors)+
      theme(text = element_text(color="black"),
            panel.background = element_rect(fill = "white"), 
            plot.background = element_rect(fill = "white"),
            #panel.grid.major = element_line(colour = "gray25"),
            #panel.grid.minor = element_line(colour = "gray25"),
            axis.text = element_text(color="black")) 

ggplot(df, aes(x=x_tsne, y=y_tsne)) + 
      geom_point(aes(size=max_topic_prob, col = max_topic), alpha=0.5) + 
      coord_equal() + 
      ggtitle("Training set Topics Representation") + 
      xlab("") + ylab("") + 
      labs(subtitle="Topics Colored and Sized by Top Topic Probability",
           size="Probability",col="Topics") +
      theme_minimal() + 
      scale_fill_manual(values = colors)+
      theme(axis.text = element_text(size = 15)) +
      theme(axis.title = element_text(size = 15)) +
      theme(text = element_text(color="black"),
            #legend.position = 'none',
            legend.text=element_text(size=12),
            panel.background = element_rect(fill = "white"), 
            plot.background = element_rect(fill = "white"),
            #panel.grid.major = element_line(colour = "gray25"),
            panel.grid.minor = element_line(colour = "gray25"),
            axis.text = element_text(color="black")) +
      annotate("text", x =13 , y = 0, label = "Topic 1", size =5) +
      annotate("text", x =5 , y = 40, label = "Topic 2", size =5) +
      annotate("text", x =-10 , y = 0, label = "Topic 3", size =5) +
      annotate("text", x =-20 , y = 10, label = "Topic 4", size =5) +
      annotate("text", x =30 , y = 12, label = "Topic 5", size =5) +
      annotate("text", x =-2 , y = -25, label = "Topic 6", size =5) +
      annotate("text", x =-33 , y = 8, label = "Topic 7", size =5) +
      annotate("text", x =-20 , y = -33, label = "Topic 8", size =5) +
      annotate("text", x =-25 , y = -17, label = "Topic 9", size =5) +
      annotate("text", x =-21, y = 25, label = "Topic 10", size =5) +
      annotate("text", x =15 , y = -12, label = "Topic 11", size =5) +
      annotate("text", x =18 , y = 27, label = "Topic 12", size =5) +
      annotate("text", x =30, y = 0, label = "Topic 13", size =5) +
      annotate("text", x =26 , y = -34, label = "Topic 14", size =5) +
      annotate("text", x =-5 , y = 16, label = "Topic 15", size =5)+
      guides(color = FALSE)






