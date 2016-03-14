

summary_plot <- function(data) {
    ggplot(data) +
    aes(x=timestamp, y=value, color=measure, linetype=measure) +
    summary_colorscale + summary_linescale +
    geom_line() +
    my_plot_theme
}

scatterplot <- function(data) {
    ggplot(data$events) +
        aes(x=timestamp, y=value) +
        geom_point(size=0.2, alpha=0.4) +
        coord_cartesian(ylim=c(0,data$ylimit)) +
        my_plot_theme
}

histogram <- function(data) {
    ggplot(data$events) +
        aes(value) +
        geom_histogram(bins=120, fill='#CCCCCC') +
        summary_colorscale + summary_linescale +
        my_plot_theme +
        legend_right
}

apdex_grid <- function(datasets) {

    data <- lapply(datasets, function(dataset){
        data.frame(apdex_eval(dataset$events$value, dataset$apdex_t))
    }) %>% bind_rows
    ggplot(data) +
        aes(x=0, y=1:length(datasets))  +
            geom_point(aes(size=1), colour='green', show.legend = F) +
            geom_point(aes(size=(t+f)/total), colour='red', alpha=0.5, show.legend = F) +
            geom_point(aes(size=f/total), colour='red', show.legend = F) +
            scale_size_area(max_size=25) + #, trans='sqrt') +
            my_plot_theme +
            theme(rect=element_blank(),
                  text=element_blank(),
                  line=element_blank()) +
        ylim(0.5, length(datasets)+0.5) +
        xlim(-4, 4) +
        theme(panel.background=element_rect(fill='#CCCCCC'))
    
}

add_plotlines <- function(g, data, plots=c('mean', 'median')) {
    g + geom_line(data=filter(data$plotlines, measure %in% plots), 
                  aes(x=timestamp, y=value, color=measure, linetype=measure),
                  size=1.5) +
        summary_colorscale + summary_linescale +
        theme(legend.position = "top",
              legend.key.width=unit(2, 'cm'))
}

# Build the cost vs benefit matrix one entry at a time
cvb_add <- function(df=NULL, Name, Efficiency, Value, Notice=T) {
    row <- list(Name=Name, Efficiency=Efficiency, Value=Value, Notice=Notice)
    if (is.null(df)) { 
        df <- data.frame(row, stringsAsFactors = F)   
    } else {
        if (nrow(df) > 0) df[,'Notice'] <- F
        df[nrow(df)+1,] <- row
    }
    df    
}



# Get a dataframe with columns 'Name', 'Cost', 'Value', 'Notice'
# where Notice is a boolean indicating the entry should be highlighted.
empty_cvb <- function() {
    ggplot() + 
        xlim(c(0,10)) + ylim(c(0,10)) +
        geom_segment(aes(x=1, y=0, xend=10, yend=0), arrow=arrow(), color='red', size=1.4) +
        geom_segment(x=0, y=1, xend=0, yend=10, arrow=arrow(), color='darkgreen', size=1.4) +
        #ggtitle("APM Statistics: Cost vs Value") +
        theme(line=element_blank(),
              axis.text=element_blank(),
              rect=element_blank(),
              text=element_text(size=28),
              legend.position='None')
}

# Get a dataframe with columns 'Name', 'Cost', 'Value', 'Notice'
# where Notice is a boolean indicating the entry should be highlighted.
cvb_plot <- function(cvb) {
    ggplot(cvb) + 
        xlim(c(0,10)) + ylim(c(0,10)) +
        geom_label(aes(x=Efficiency, y=Value,
                       label=Name,
                       fill=as.factor(Notice),
                       color=as.factor(Notice),
                       size=as.factor(Notice)),
                   vjust='middle',
                   hjust='middle',
                   label.padding=unit(0.6, 'lines')) +
        scale_fill_manual(values=c('FALSE'='#DDDDDD','TRUE'='#CCFFCC')) +
        scale_color_manual(values=c('FALSE'='black', 'TRUE'='red')) +
        scale_size_manual(values=c('FALSE'=6, 'TRUE'=10)) +
        geom_segment(aes(x=1, y=0, xend=10, yend=0), arrow=arrow(), color='red', size=1.4) +
        geom_segment(aes(x=0, y=1, xend=0, yend=10), arrow=arrow(), color='darkgreen', size=1.4) +
        #ggtitle("APM Statistics: Cost vs Value") +
        theme(line=element_blank(),
              axis.text=element_blank(),
              rect=element_blank(),
              text=element_text(size=28),
              legend.position='None')
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, widths=NULL) {
    library(grid)
    
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
        if (is.null(widths)) 
            pushViewport(viewport(layout = grid.layout(nrow(layout), 
                                                       ncol(layout))))
        else
            pushViewport(viewport(layout = grid.layout(nrow(layout), 
                                                       ncol(layout), widths=widths)))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}
