setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanEvolution/Models/EvolutionInnovation'))

library(ggplot2)
library(dplyr)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

resprefix = 'CALIBRATION_REPLICATIONS_LOCAL_20211129_115043'
res <- as_tibble(read.csv(paste0('openmole/calibration/',resprefix,'/population10000.csv')))
resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanEvolution/Results/EvolutionInnovation/',resprefix,'/');dir.create(resdir)

g <- ggplot(res,aes(x=oppAverageUtility, y = averageGravityFlow, color=gravityDecay, size = innovationDecay, shape=utilityDistribution))
g+geom_point(alpha=0.5)
ggsave(file=paste0(resdir,'pareto-oppAverageUtility-averageGravityFlow_color-gravityDecay_size-innovationDecay_shape-utilityDistribution.png'),width=22,height=18,units='cm')


#g <- ggplot(res,aes(x=oppAverageUtility, y = averageGravityFlow, color=newInnovationHierarchy, size = innovationDecay, shape=utilityDistribution))
#g+geom_point(alpha=0.5)


## several Pareto fronts: initial hierarchy
res1 <- as_tibble(read.csv('openmole/calibration/CALIBRATION_REPLICATIONS_HIERARCHY_0_5_LOCAL_20211129_165201/population10000.csv'))
res2 <- as_tibble(read.csv('openmole/calibration/CALIBRATION_REPLICATIONS_HIERARCHY_1_5_LOCAL_20211129_165057/population10000.csv'))

allres = rbind(cbind(res,hierarchy=rep("1",nrow(res))),cbind(res1,hierarchy=rep("0.5",nrow(res1))),cbind(res2,hierarchy=rep("1.5",nrow(res2))))

g <- ggplot(allres,aes(x=oppAverageUtility, y = averageGravityFlow, color=gravityDecay, size = innovationDecay, shape=hierarchy))
g+geom_point(alpha=0.5)
ggsave(file=paste0(resdir,'pareto-oppAverageUtility-averageGravityFlow_VARYINGHIERARCHY_color-gravityDecay_size-innovationDecay.png'),width=22,height=18,units='cm')


## innovation hierarchy
res1 <- as_tibble(read.csv('openmole/calibration/CALIBRATION_REPLICATIONS_INNOVHIERARCHY_0_5_LOCAL_20211129_205704//population10000.csv'))
res2 <- as_tibble(read.csv('openmole/calibration/CALIBRATION_REPLICATIONS_INNOVHIERARCHY_1_LOCAL_20211129_221145//population10000.csv'))
res3 <- as_tibble(read.csv('openmole/calibration/CALIBRATION_REPLICATIONS_INNOVHIERARCHY_1_5_LOCAL_20211129_214916///population10000.csv'))


allres = rbind(cbind(res2,innovHierarchy=rep("1",nrow(res2))),cbind(res1,innovHierarchy=rep("0.5",nrow(res1))),cbind(res3,innovHierarchy=rep("1.5",nrow(res3))))

g <- ggplot(allres,aes(x=oppAverageUtility, y = averageGravityFlow, color=gravityDecay, size = innovationDecay, shape=innovHierarchy))
g+geom_point(alpha=0.5)
ggsave(file=paste0(resdir,'pareto-oppAverageUtility-averageGravityFlow_VARYINGINNOVHIERARCHY_color-gravityDecay_size-innovationDecay.png'),width=22,height=18,units='cm')



