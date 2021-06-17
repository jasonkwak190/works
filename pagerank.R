library(igraph)
library(Rgraphviz)

#1. Compute the page rank vector the graph below for damping constants:
# p =0, p =.10, p=.15, p=.25, p = .50 and p=1.

nodes <- data.frame(names = c("Page1", "Page2", "Page3", "Page4", "Page5", "Page6"))
relations <- data.frame(
  from = c("Page6","Page5","Page4","Page4","Page3","Page2","Page2"),
  to = c("Page3","Page4","Page5","Page2","Page1","Page3","Page5")
)

g<- graph.data.frame(relations, directed = TRUE, vertices = nodes)
plot(g)

?page.rank
pg0 <- page.rank(g, damping = 0)
pg0$vector  #0.1666667 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667 

pg01 <- page.rank(g, damping = 0.1)
pg01$vector #0.1704602 0.1613300 0.1761916 0.1697807 0.1693965 0.1528410 

pg15 <- page.rank(g, damping = 0.15)
pg15$vector #0.1729613 0.1588612 0.1798039 0.1716071 0.1707758 0.1459907

pg25 <- page.rank(g, damping = 0.25)
pg25$vector #0.1786588 0.1544288 0.1848587 0.1758772 0.1737324 0.1324441 

pg05 <- page.rank(g, damping = 0.5)
pg05$vector #0.19227231 0.14719411 0.18583257 0.19135235 0.18399264 0.09935603

pg1 <- page.rank(g, damping = 1)
pg1$vector  #0.16216216 0.16216216 0.13513514 0.27027027 0.24324324 0.02702703


#######################################################################################
#2. Compute the page rank for the graph below. Use a damping constant p=0.15. Interpret
# the results. Are they within your expectation?

nodes1 <- data.frame(names = c("Page1", "Page2", "Page3", "Page4", "Page5", "Page6", "Page7", "Page8"))
relations1 <- data.frame(
  from = c("Page8","Page7","Page6","Page5","Page4","Page3","Page2"),
  to = c("Page3","Page3","Page3","Page2","Page2","Page1","Page1")
)

g1<- graph.data.frame(relations1, directed = TRUE, vertices = nodes1)
plot(g1)

pg <- page.rank(g1, damping = 0.15)
pg$vector #0.1541610 0.1418827 0.1582538 0.1091405 0.1091405 0.1091405 0.1091405 0.1091405

# I expected page 1 to have the highest vector, because all number points to number 1
# and it should eventually have the highest value.
# As you can see, Page1 has the highest vector, and Page 2 and Page 3 consequently.