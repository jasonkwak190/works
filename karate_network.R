rm(list = ls())
#install.packages("igraphdata")
library(igraphdata)
library(igraph)
data(karate)
?karate
data(kite)
?kite

# 1.  Focus on the karate network. Create noisy datasets. Do this by deleting 5% of the edges
# randomly (track which ones they are). Perform MCMC for a random graph model (as in
# Clauset et al.) on this data followed by link-prediction. Are you able to predict the edges that
# you deleted?
set.seed(100)

g <- karate

#delete 5% edges
g_del <- delete_edges(g, sample(E(g),ecount(g)*0.05))
g_del


#predict edges
p_edges <- predict_edges(g_del)
p_edges$edges
p_edges$prob
plot(p_edges$prob)


#2. Focus on the yeast network (or kite network). Create noisy datasets. Do this by deleting 5%
# of the edges randomly (track which ones they are). Perform MCMC on this data followed by
# link-prediction. Are you able to predict the edges that you deleted at random well?

g1 <- kite

#delete 5% edges
g_del1 <- delete_edges(g1, sample(E(g1),ecount(g1)*0.05))
g_del1

#predict edges
p_edges1 <- predict_edges(g_del1)
p_edges1$edges
p_edges1$prob
plot(p_edges1$prob)


#3. Repeat the exercise in part (a) and (b) after deleting 15%, and 40% of the edges. Comment on
# your findings.
#(a)
#delete 15% edges karate
g_del2 <- delete_edges(g, sample(E(g),ecount(g)*0.15))
g_del2

#predict edges
p_edges2 <- predict_edges(g_del2)
p_edges2$edges
p_edges2$prob
plot(p_edges2$prob)

##
#delete 40% edges
g_del3 <- delete_edges(g, sample(E(g),ecount(g)*0.4))
g_del3

#predict edges
p_edges3 <- predict_edges(g_del3)
p_edges3$edges
p_edges3$prob
plot(p_edges3$prob)


#(b)
#delete 15% edges kite
g_del4 <- delete_edges(g1, sample(E(g1),ecount(g1)*0.15))
g_del4

#predict edges
p_edges4 <- predict_edges(g_del4)
p_edges4$edges
p_edges4$prob
plot(p_edges4$prob)

##
#delete 40% edges
g_del5 <- delete_edges(g1, sample(E(g1),ecount(g1)*0.4))
g_del5

#predict edges
p_edges5 <- predict_edges(g_del5)
p_edges5$edges
p_edges5$prob
plot(p_edges5$prob)
