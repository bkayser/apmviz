library(ggplot2)
library(ggthemes)

blank_theme <- theme(axis.text=element_blank(),
                     axis.title=element_blank(),
                     axis.ticks=element_blank(),
                     legend.position='none',
                     panel.grid=element_blank(),
                     panel.border=element_blank(),
                     strip.text=element_blank(),
                     panel.background=element_rect(fill='white'))


my_plot_theme <- theme_minimal() + 
    theme(legend.position = "top",
          legend.key.width=unit(2, 'cm'),
          panel.border=element_rect(fill=NA))

legend_right <-  theme(legend.position='right', 
                       legend.margin =   unit(0.2, "cm"),
                       legend.key.width= unit(1, 'cm'),
                       legend.key.height=unit(2, 'cm'))

# These are handy for consistent line properties
# for the summary values.

summary_colorscale <- scale_color_manual(name=NULL,
                                         values=c('mean'='black',
                                                  'median'='#33AA33', 
                                                  'sd'='yellow',
                                                  'throughput'='#991111',  
                                                  'q25'='#EE0033', 
                                                  'q75'='#EE0033', 
                                                  'q95'='#FF2222',
                                                  'q99'='#FF2222',
                                                  'gmean' = 'maroon',
                                                  'score' = 'darkgreen')) 

summary_linescale <- scale_linetype_manual(name=NULL,
                                           values=c('mean'=1,
                                                    'median'=1, 
                                                    'sd'=1,
                                                    'throughput'=1,
                                                    'q25'=5, 
                                                    'q75'=5, 
                                                    'q95'=4,
                                                    'q99'=4,
                                                    'gmean'=44,
                                                    'score'=2))

show_my_aesthetics <- function(measures=c('mean', 'median', 'sd', 'throughput', 'q25', 'q75', 'q95', 'q99', 'gmean', 'score')) {
    
    mdata <- expand.grid(measure=measures, x=0:1)
    mdata[,'y'] <- rep(1:length(measures), 2)
    
    ggplot(mdata) + 
        geom_line(aes(x=x, y=y, color=measure, linetype=measure)) + 
        summary_linescale + summary_colorscale + my_plot_theme +
        geom_text(data=data.frame(x=0.5, y=0.25+1:length(measures), label=measures),
                  aes(x, y, label=label)) +
        theme(legend.position='none')
}

# Color to use highlighting bands in histograms
region_fill <- 'green'

