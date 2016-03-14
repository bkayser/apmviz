
horizon_data <- function(ex, value='mean', n=6) {
    plotlines <- data.table::dcast(ex$plotlines, name + timestamp ~ measure, drop=F)
    plots <- split(plotlines, plotlines$name)
    top <- max(plotlines[,value], na.rm=T)
    hsize <- round(1.05 * top / n)
    df <- lapply(plots, function(data){
        name <- data$name[1]
        s <- spline(data$timestamp, data[,value])
        timestamp <- as.POSIXct(s$x,  origin="1970-01-01")
        value <-s$y

        m <- list()
        for (level in 1:n) {
            h = stri_c('H',level)
            m[[h]] <- pmax(value, (level-1)*hsize, na.rm=T) %>% pmin(level*hsize, na.rm=T)
            m[[h]] <- m[[h]] - (level-1)*hsize
        }
        m %>%
            rbind_list() %>%
            bind_cols(data.frame(timestamp=timestamp, name=as.character(name))) %>%
            melt(id.vars=c('timestamp','name'), variable.factor=F, variable.name='horizon', value.name='mean')
    }) %>% bind_rows
    return(list(df=df, horizon=hsize))
}    
   