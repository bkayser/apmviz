library(stringi)
library(data.table)
library(lubridate)

floor_to_period <- function(t, p, units='min') {
    t <- round(t, units)
    period_secs <- as.integer(duration(p, units))
    t_seconds <- (as.numeric(t) %/% period_secs) * period_secs
    as.POSIXct(t_seconds, origin='1970/01/01')
}

apdex_eval <- function(x, t) {
    counts <- sapply(t, function(ti) { summary(factor(levels=c(0,1,2), findInterval(x, c(ti, 4*ti)))) })
    
    df <- data.frame(t(counts))
    names(df) <- c('s', 't','f')
    df[,'T'] <- t
    df[,'total'] <- length(x)
    mutate(df, score= (s + t/2)/(s+t+f))
}

apdex_score <- function(x, t) {
    v <- summary(factor(levels=c(0,1,2), findInterval(x, c(t, 4*t))))
    (v[1] + v[2]/2)/length(x)
}

