# Interactive charts

library(lubridate)
library(dplyr)
library(data.table)
library(stringi)
histogram_demo <- function(file, value_field) {
    data <- read_transactions(file, value_field)
    normalspace <- data$events$value
    logspace <- log(data$events$value)
    sd <- sd(logspace)
    gmean <- mean(logspace)
    # The estimates for 25 & 75 percentile in log space
    inner_quartile_estimate <- c(lower=gmean - sd * qnorm(0.75),
                                 upper=gmean + sd * qnorm(0.75))
    parameters <- list(
        normal=list(data=normalspace,
                    gmean=exp(gmean),
                    median=median(normalspace),
                    ymax=max(density(normalspace[normalspace <= data$xlimit])$y),
                    xlimit=data$xlimit,
                    inner_quartile_estimate=exp(inner_quartile_estimate)),
        logspace=list(data=logspace,
                      gmean=gmean,
                      median=median(logspace),
                      ymax=max(density(logspace)$y),
                      xlimit=NA,
                      inner_quartile_estimate=inner_quartile_estimate)
    )
    
    minimal_theme <- my_plot_theme + theme(legend.position='none',
                                      panel.border=element_rect(fill=NA),
                                      plot.margin=unit(c(0,0,0,0), units='cm'),
                                      text=element_blank())
    
    hist <- function(input) {
        if (input$in_logspace) {
            p <- parameters$logspace
        } else {
            p <- parameters$normal
        }
        g <- ggplot() + aes(p$data) +
            geom_density(fill='#CCCCCC') +
            summary_colorscale +
            scale_linetype_manual(values=c(median=1, 
                                           q25=3,
                                           q75=3,
                                           gmean=22)) +
            minimal_theme +
            xlim(c(0, p$xlimit)) 
        if (input$show_median) {
            g <- g + geom_vline(aes(color='median', linetype='median', 
                                    xintercept=p$median), size=2)              
        }
        if (input$show_gmean) {
            g <- g + geom_vline(aes(color='gmean', linetype='gmean', 
                                    xintercept=p$gmean), size=2)              
        }
        if (input$show_quartile_est) {
            g <- g + geom_rect(aes(xmin=p$inner_quartile_estimate['lower'], 
                                   xmax=p$inner_quartile_estimate['upper'],
                                   ymin=0, ymax=1.05 * p$ymax),
                               alpha=0.2,
                               fill=region_fill,
                               inherit.aes=F)
        }
        #g + ylim(0, p$ymax)
        if (input$show_quartile_act) {
            quartiles = summary(p$data)
            g <- g + geom_rect(aes(xmin=quartiles[[2]], 
                                   xmax=quartiles[[5]],
                                   ymin=0, ymax=1.05 * p$ymax),
                               alpha=0.3,
                               fill='red',
                               inherit.aes=F)
        }
        g
    }
    scatterplot <- function(input) {
        ylimit <- quantile(data$events$value, 0.94)
        g <- ggplot(data$events) +
            aes(x=timestamp) +
            coord_cartesian(ylim=c(0,ylimit)) +
            summary_colorscale + summary_linescale +
            minimal_theme  
        if (input$scatterplot) {
            g <- g + geom_point(aes(y=value), size=0.1, alpha=0.4) 
        }
        if (input$scatter_gmean) {
            gmean <- filter(data$plotlines, measure=='gmean')
            g <- g + geom_line(data=gmean, size=1.5,
                               aes(x=timestamp, y=value, color='gmean', linetype='gmean'))
        }
        if (input$scatter_mean) {
            mean <- filter(data$plotlines, measure=='mean')
            g <- g + geom_line(data=mean, size=1.5,
                               aes(x=timestamp, y=value, color='mean', linetype='mean'))
        }
        if (input$scatter_quartiles_est) {
            quartiles <- dcast(data$plotlines, timestamp ~ measure, drop=F)
            g <- g + geom_ribbon(aes(x=timestamp,
                                     ymax=gmean /(gsd ^ 0.68), 
                                     ymin=gmean *(gsd ^ 0.68)),
                                 fill=region_fill,
                                 alpha=0.2,
                                 data=quartiles)
        }
        g
              
    }
    list(hist=hist, scatterplot=scatterplot)
}

apdex_hist <- function(data) {
    limit <- quantile(data$events$value, 0.98)
    dd <- density(filter(data$events, value < limit)$value)
    
    ddata <- data.frame(x=dd$x, d=dd$y) %>% filter(x >=0 & x <= limit)
    
    bucket_colors <- c(Satisfied='green', 
                       Tolerating='#ffcc33',
                       Failed='red')
    
    function(apdex_t) {
        bucketed_data <- mutate(ddata, 
                                bucket=as.factor(c('Satisfied', 'Tolerating', 'Failed')[findInterval(x, c(apdex_t, 4*apdex_t))+1]))
        
        g_apdex <- ggplot(bucketed_data) +
            aes(fill=bucket, x=x, y=d) +
            geom_area() +
            scale_fill_manual(values=bucket_colors, name=NULL) +
            scale_color_manual(values=c(bucket_colors), name=NULL) +
            geom_text(inherit.aes = F, 
                       x=0.6 * limit, 
                       y=0.8 * max(ddata$d), 
                       size=14,
                       label=stri_c('Score: ',100*round(apdex_score(data$events$value, apdex_t), 2))) +
            geom_vline(xintercept=apdex_t) +
            geom_text(aes(x=apdex_t, y=max(d)),
                      #nudge_x=10,
                      size=12,
                      hjust='left', 
                      vjust='top',
                      label=stri_c(' T = ',apdex_t)) +
            geom_rug(aes(color=bucket), sides="b", show.legend = F) +
            my_plot_theme +
            theme(legend.position = c(0.7, 0.5), 
                  text=element_text(size=32),
                  legend.justification = "left",
                  legend.key.height=unit(1.2, 'cm'),
                  legend.key.width=unit(1.2, 'cm'),
                  axis.text.y=element_blank(),
                  axis.title = element_blank())
        g_apdex 
    }
}

apdex_button <- function(data, t) {
    apdex <- data.frame(apdex_eval(data$events$value, t))
    total <- nrow(data$events)
    ggplot(apdex) +
        aes(x=as.factor(T), y=0)  +
        geom_point(aes(size=1), colour='green', show.legend = F) +
        geom_point(aes(size=(t+f)/total), colour='red', alpha=0.5, show.legend = F) +
        geom_point(aes(size=f/total), colour='red', show.legend = F) +
        scale_size_area(max_size=40, trans='sqrt') +
        geom_text(aes(label=paste('t=', T, sep='')), nudge_y=0.1, size=6, vjust='inward') +
        geom_text(aes(label=round(score, 2)), nudge_y=-0.1, size=6, vjust='inward') +
        my_plot_theme +
        theme(rect=element_blank(),
              text=element_blank(),
              line=element_blank())
}

mixed_charts <- function(data) {
    
    boxplot_data <- mutate(data$events, 
                           timestamp=floor_to_period(timestamp, 4 * data$period))
    
    function(input) {
        g <- ggplot() +
            coord_cartesian(ylim=c(0,data$ylimit)) +
            my_plot_theme
        if (input$bp == 'scatter') {
            g <- g + geom_point(data=data$events,
                                aes(x=timestamp, y=value),
                                size=0.2, alpha=0.4)
        }
        if (input$bp == 'boxplot') {
            g <- g + geom_boxplot(data=boxplot_data,
                                  aes(x=timestamp, group=timestamp, y=value),
                                  fill='#CCCCCC', 
                                  color='blue',
                                  alpha=0.4)
        }
        g
    }
}

if (F) {

  data <- read_transactions('./data/timeseries/storefront-deploy.rds', 'frontend')
  
  f <- mixed_charts(data)
  f(list(bp='boxplot'))
  
  f <- apdex_hist(data)
  
  manipulate(f(t), t=slider(100, 2000))
  
  f(apdex_t=500)
    
  chart <- histogram_demo('./data/raw/ecommerce-normal-activity.rds', 'backend')
     # './data/raw/ecommerce-friday-deploys.rds', 'backend'
  d <- histogram_demo('./data/raw/ecommerce-friday-deploys.rds', 'backend')
  chart$hist(list(in_logspace=T, 
             show_median=T,
             show_gmean=T,
             show_quartile_est=T,
             show_quartile_act=T))
  
  chart <- histogram_demo('./data/raw/slow-safari.rds', 'backend')
  
  chart$scatterplot(list(scatterplot=F, 
                         scatter_gmean=T,
                         scatter_mean=T,
                         scatter_quartiles_est=T))  
  chart$scatterplot(list(scatterplot=T, 
                         scatter_gmean=T,
                         scatter_quartiles_est=T))
  
  chart(list(in_logspace=T, 
             show_median=T,
             show_gmean=T,
             show_quartile_est=T,
             show_quartile_act=T))
  
  chart(list(in_logspace=T, 
             show_median=T,
             show_gmean=T,
             show_quartile_est=T,
             show_quartile_act=T))
  
  
  
  }