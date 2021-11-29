setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanEvolution/Models/EvolutionInnovation'))

library(ggplot2)
library(dplyr)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))


res <- as_tibble(read.csv('openmole/calibration/CALIBRATION_REPLICATIONS_LOCAL_20211129_115043/population10000.csv'))


g <- ggplot(res,aes(x=oppAverageUtility, y = averageGravityFlow, color=gravityDecay, size = innovationDecay))
g+geom_point(alpha=0.5)

g <- ggplot(res,aes(x=oppAverageUtility, y = averageGravityFlow, color=newInnovationHierarchy, size = innovationDecay, shape=utilityDistribution))
g+geom_point(alpha=0.5)


## several Pareto fronts: initial hierarchy
res1 <- as_tibble(read.csv('openmole/calibration/CALIBRATION_REPLICATIONS_HIERARCHY_0_5_LOCAL_20211129_165201/population3000.csv'))
res2 <- as_tibble(read.csv('openmole/calibration/CALIBRATION_REPLICATIONS_HIERARCHY_1_5_LOCAL_20211129_165057/population3000.csv'))

allres = rbind(cbind(res,hierarchy=rep("1",nrow(res))),cbind(res1,hierarchy=rep("0.5",nrow(res1))),cbind(res2,hierarchy=rep("1.5",nrow(res2))))

g <- ggplot(allres,aes(x=oppAverageUtility, y = averageGravityFlow, color=gravityDecay, size = innovationDecay, shape=hierarchy))
g+geom_point(alpha=0.5)
