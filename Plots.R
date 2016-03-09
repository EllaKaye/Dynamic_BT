library(ggplot2)
library(grid)
# pt is for 'premier top'
# p is for 'plot'
# mt is for 'minus ties' (i.e. draws)
# it is for 'including ties'
# .x gives value of rho i.e. .7 is rho = 0.7
# ha for 'home and away'

years <- UK.years.teams$years
even_years <- c(2000,2002,2004,2006,2008)
year <- rep(years, each = 7)
teams <- c("Chelsea", "Everton", "Aston Villa", "Liverpool", "Arsenal", "Tottenham", "Man U")
team <- rep(teams, 10)

# plotting functions
# function to print list of n graphs in a column
nGraphsCol <- function(graphs) {
  # graphs is a list of graphs
  n <- length(graphs)
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(n,1)))
  vplayout <- function(x,y) viewport(layout.pos.row=x, layout.pos.col=y)
  for (i in 1:n) {
    print(graphs[[i]], vp=vplayout(i,1))
  }
}

# function to print list of n graphs in a row
nGraphsRow <- function(graphs) {
  # graphs is a list of graphs
  n <- length(graphs)
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(1,n)))
  vplayout <- function(x,y) viewport(layout.pos.row=x, layout.pos.col=y)
  for (i in 1:n) {
    print(graphs[[i]], vp=vplayout(1,i))
  }
}

means_to_df <- function(means_mat) {
  df <- data.frame(beta = as.vector(means_mat), year = year, team = team)
  df <- transform(df, model.rank = ave(beta, year, FUN = function(x) rank(-x, ties.method = "first")))
  df
}
rank_to_df <- function(rank_mat) {
  df <- data.frame(rank = as.vector(rank_mat), year = year, team = team)
  df
}

# plotting data frames
pt.it.davidson.mat <- read.csv("Davidson_ties.csv") # needs redoing
dim(pt.it.davidson.mat)

pt.mt <- means_to_df(means_over_time0.7sigma0.7)
pt.true.mt <- rank_to_df(top7.ranks.mt) # from True_ranks.R
pt.true.it <- rank_to_df(top7.ranks) # from True_ranks.R
pt.it <- means_to_df(tie_means0.7sigma0.7pho[1:7,])
pt.mt.ha <- means_to_df(means_over_time0.7sigma0.7_ha[1:7,])

pt.mt.5 <- means_to_df(means_over_time0.5sigma0.7)
pt.mt.7 <- pt.mt
pt.mt.9 <- means_to_df(means_over_time0.9sigma0.7)

pt.mt.rho <- rbind(pt.mt.5, pt.mt.7, pt.mt.9)
rho <- rep(c(0.5, 0.7, 0.9), each = 70)
pt.mt.rho <- cbind(pt.mt.rho, rho = as.factor(rho))

# plots
# plot of BT model rank, minus ties
p.mt.rank <- ggplot(pt.mt, aes(year, model.rank, group=team, colour=team)) + geom_line() + geom_point()
p.mt.rank <- p.mt.rank + scale_y_reverse(breaks=1:7) + ylab("rank")
p.mt.rank <- p.mt.rank + scale_x_continuous(breaks=years) + theme_bw()
#p.mt.rank <- p.mt.rank + ggtitle("Model rank, no draws")
p.mt.rank

# plot of BT true rank, minus ties
p.mt.true.rank <- ggplot(pt.true.mt, aes(year, rank, group=team, colour=team)) + geom_line() + geom_point()
p.mt.true.rank <- p.mt.true.rank + scale_y_reverse(breaks=1:7) + ylab("rank")
p.mt.true.rank <- p.mt.true.rank + scale_x_continuous(breaks=years) + theme_bw()
#p.mt.true.rank <- p.mt.true.rank + ggtitle("True rank, no draws")
p.mt.true.rank

# plot of BT model betas, minus ties
p.mt.beta <- ggplot(pt.mt, aes(year, beta, group=team, colour=team)) + geom_line() + geom_point()
p.mt.beta <- p.mt.beta + scale_x_continuous(breaks=years) + theme_bw()
p.mt.beta <- p.mt.beta + ylim(c(-1.3, 1.3))
p.mt.beta <- p.mt.beta + ggtitle("Beta over time, standard BT model")
p.mt.beta

# plot of BT true rank, including ties
p.it.true.rank <- ggplot(pt.true.it, aes(year, rank, group=team, colour=team)) + geom_line() + geom_point()
p.it.true.rank <- p.it.true.rank + scale_y_reverse(breaks=1:7) + ylab("rank")
p.it.true.rank <- p.it.true.rank + scale_x_continuous(breaks=years) + theme_bw()
#p.it.true.rank <- p.it.true.rank + ggtitle("True rank, with draws")
p.it.true.rank

# plot of BT model betas, including ties
p.it.beta <- ggplot(pt.it, aes(year, beta, group=team, colour=team)) + geom_line() + geom_point()
p.it.beta <- p.it.beta + scale_x_continuous(breaks=years) + theme_bw()
p.it.beta

# plot of BT model rank, including ties
p.it.rank <- ggplot(pt.it, aes(year, model.rank, group=team, colour=team)) + geom_line() + geom_point()
p.it.rank <- p.it.rank + scale_y_reverse(breaks=1:7) + ylab("rank")
p.it.rank <- p.it.rank + scale_x_continuous(breaks=years) + theme_bw()
p.it.rank <- p.it.rank + ggtitle("Model rank, with draws")
p.it.rank

# plot of BT home/away model rank, minus ties
p.mt.ha.rank <- ggplot(pt.mt.ha, aes(year, model.rank, group=team, colour=team)) + geom_line() + geom_point()
p.mt.ha.rank <- p.mt.ha.rank + scale_y_reverse(breaks=1:7) + ylab("rank")
p.mt.ha.rank <- p.mt.ha.rank + scale_x_continuous(breaks=years) + theme_bw()
p.mt.ha.rank

# plot of BT home/away model betas, minus ties
p.mt.ha.beta <- ggplot(pt.mt.ha, aes(year, beta, group=team, colour=team)) + geom_line() + geom_point()
p.mt.ha.beta <- p.mt.ha.beta + scale_x_continuous(breaks=years) + theme_bw()
p.mt.ha.beta <- p.mt.ha.beta + ylim(c(-1.3, 1.3))
p.mt.ha.beta <- p.mt.ha.beta + ggtitle("Beta over time, with home advantage")
p.mt.ha.beta

# comparing different rho values, for Arsenal
p.mt.rho.beta <- ggplot(pt.mt.rho[team=="Arsenal",], aes(year, beta, group=rho, colour=rho)) + geom_line() + geom_point()
p.mt.rho.beta <- p.mt.rho.beta + scale_x_continuous(breaks=years) + theme_bw()
p.mt.rho.beta <- p.mt.rho.beta + ggtitle("Arsenal betas over time, for different rhos")
pdf("Arsenal_rho.pdf")
p.mt.rho.beta
dev.off()

# plot comparisons
pt.mt$model <- "Standard BT"
pt.mt.ha$model <- "Home advantage"
pt.mt.std.vs.ha <- rbind(pt.mt, pt.mt.ha)
pt.mt.std.vs.ha$model <- factor(pt.mt.std.vs.ha$model, levels = c("Standard BT", "Home advantage"))

p.mt.std.vs.ha.beta <- ggplot(pt.mt.std.vs.ha, aes(year, beta, group=team, colour=team)) + geom_line() + geom_point() + facet_wrap(~ model) + scale_x_continuous(breaks=even_years) + theme_bw()
p.mt.std.vs.ha.beta

pdf("Standard_vs_HA.pdf", width=10, height=5)
#nGraphsRow(list(p.mt.beta, p.mt.ha.beta))
p.mt.std.vs.ha.beta
dev.off()

# tm for "truth" or "model"
# itmt for "with draws", "without draws"
pt.it$tm <- "model"
pt.it$itmt <- "with draws"
pt.it$rank <- pt.it$model.rank
pt.mt$tm <- "model"
pt.mt$itmt <- "without draws"
pt.mt$rank <- pt.mt$model.rank
pt.true.it$tm <- "truth"
pt.true.it$itmt <- "with draws"
pt.true.mt$tm <- "truth"
pt.true.mt$itmt <- "without draws"
cols <- c("rank", "year", "team", "tm", "itmt")

quartet <- rbind(pt.mt[,cols], pt.it[,cols], pt.true.mt[,cols], pt.true.it[,cols])
p.tm.itmt <- ggplot(quartet, aes(year, rank, group=team, colour=team)) + geom_line() + geom_point()
p.tm.itmt <- p.tm.itmt + facet_grid(tm ~ itmt)
p.tm.itmt <- p.tm.itmt + scale_x_continuous(breaks=even_years) + scale_y_reverse(breaks=1:7) + theme_bw()
p.tm.itmt

pdf("Model_draw_grid.pdf", width=8, height=7)
p.tm.itmt
dev.off()

pdf("Model_no_ties_vs_ties.pdf", width=12, height=5)
nGraphsRow(list(p.mt.rank, p.it.rank))
dev.off()

pdf("True_no_ties_vs_ties.pdf", width=12, height=5)
nGraphsRow(list(p.mt.true.rank, p.it.true.rank))
dev.off()