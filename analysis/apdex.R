
f <- 50

v <- rgamma(5000, shape=2, scale=400)
v <- rlnorm(5000, meanlog=log(500), sdlog=log(2))
x <- 0:1000
qplot(v, xlim=c(0, 5000), bins=100)

apdex_eval <- function(t, v) {
    counts <- table(cut(v, c(0, t, 4*t, max(v)+1), labels=c('s','t','f')))
    return(c(s=counts['s'],
             t=counts['t'],
             f=counts['f'],
             score=(counts['s'] +(0.5 * counts['t']))/length(v)))
}
t_values <- c(400, 600, 800, 1000, 1200, 1400, 1600)
apdex <- data.frame(t(sapply(t_values, apdex_eval, v)))
names(apdex) <- c('s','t','f', 'score')
apdex <- mutate(apdex,
                name=factor(t_values, labels=paste("t", t_values)),
                total=t+f+s)

ggplot(apdex) +
    aes(x=as.factor(name), y=0)  +
    geom_point(aes(size=1), colour='green', alpha=0.4, show.legend = F) +
    geom_point(aes(size=(t+f)/total), colour='#FFBB00', show.legend = F) +
    geom_point(aes(size=f/total), colour='red', show.legend = F) +
    scale_size_area(max_size=40, trans='sqrt') +
    geom_text(aes(label=paste('t=', t_values, sep='')), nudge_y=0.1, size=6, vjust='inward') +
    geom_text(aes(label=round(score, 2)), nudge_y=-0.1, size=6, vjust='inward') +
    theme_bw()

