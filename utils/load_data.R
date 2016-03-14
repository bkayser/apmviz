
library(dplyr)
library(stringi)
library(data.table)
library(lubridate)

read_transactions <- function(filename, value_column, facet=NULL, limit=NULL, apdex_t=NULL) {
    message('Reading ', filename)
    events <- filename %>%
        readRDS() %>%
        tbl_df
    events[,'value'] <- events[,value_column]
    if (!is.null(facet)) {
        events[,'name'] <- events[,facet]
        events$name <- basename(as.character(events$name))
    }
    events <- filter(events, value > 0)
    if (!is.null(facet)) {
        top_n <- count(events, name, sort=T)$name %>% head(limit)
        events <- filter(events, name %in% top_n)
    }

    if (is.null(apdex_t)) {
        # 75% quantile is a good starting point for apdex T.
        apdex_t <- signif(quantile(events$value, 0.75), 2)
    }    
    end_ts <- max(events$timestamp)
    duration <- as.numeric(max(events$timestamp) - min(events$timestamp), units='mins')
    if (duration < 10) {
        p <- 5
        u <- 'sec'
    } else if (duration < 30) {
        p <- 10
        u <- 'sec'
    } else if (duration < 90) {
        p <- 30
        u <- 'sec'
    } else if (duration < 360) {
        p <- 1
        u <- 'min'
    } else {
        p <- 5
        u <- 'min'
    }
    period <- period(p, u)
    histogram_xlimit <- quantile(events$value, 0.96)
    timeseries_ylimit <- quantile(events$value, 0.94)
    
    z <- qnorm(p=0.75)
    
    if (!is.null(facet)) {
        e <- group_by(events, name)
    } else {
        e <- events
    }
    summary <- summarize(e, mean=mean(value),
                         median=median(value),
                         sd=sd(value),
                         q25=quantile(value, 0.25),
                         q75=quantile(value, 0.75),
                         q95=quantile(value, 0.95),
                         score=apdex_score(value, apdex_t))
    
    plots <- events %>%
        mutate(timestamp=floor_to_period(timestamp, p, u)) %>%    
        filter(timestamp > min(timestamp) & timestamp < max(timestamp))
    if (is.null(facet)) {
        plots <- group_by(plots, timestamp)
        group_vars <- 'timestamp'
    } else {
        group_vars <- c('timestamp', 'name')
        plots <- group_by(plots, timestamp, name)
    }
    plots <- plots %>%
        summarize(mean=mean(value),
                  median=median(value),
                  throughput=as.double(n()),
                  sd=sd(value),
                  gmean=exp(mean(log(value))),
                  gsd=exp(sd(log(value))),
                  q25=quantile(value, 0.25),
                  q75=quantile(value, 0.75),
                  q95=quantile(value, 0.95),
                  q99=quantile(value, 0.99),
                  q25_est=mean-z*sd,
                  q75_est=mean+z*sd,
                  score=apdex_score(value, apdex_t)) %>%
        melt(id.vars=group_vars, variable.name='measure') %>%
        tbl_df
    
    return (list(
        events=events,
        xlimit=histogram_xlimit,
        ylimit=timeseries_ylimit,
        plotlines = plots,
        summary = summary,
        period = period,
        start = min(events$timestamp),
        apdex_t = apdex_t,
        name = stri_match(basename(filename), regex='[^.]*')[1,1]))
}
