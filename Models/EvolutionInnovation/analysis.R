
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanEvolution/Models/EvolutionInnovation'))

library(dplyr)
library(ggplot2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

params<-c("gravityDecay","innovationDecay","mutationRate","newInnovationHierarchy","utilityStd","earlyAdoptersRate","utilityDistribution")#,"initialHierarchy")
paramnames=list(gravityDecay=expression(d[G]),innovationDecay=expression(d[I]),mutationRate=expression(beta),newInnovationHierarchy=expression(alpha[I]),utilityStd=expression(sigma[U]),earlyAdoptersRate=expression(delta[0]),utilityDistribution="Distribution")
indics<-c("averageDiversity","averageInnovation","averageUtility","finalHierarchy")
indicnames = list(averageDiversity="Diversity",averageInnovation="Innovation",averageUtility="Utility",finalHierarchy="Population hierarchy")

# stochasticity
resprefix = '20200429_150801_STOCHASTICITY_GRID'
resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanEvolution/Results/EvolutionInnovation/',resprefix,'/');dir.create(resdir)

res <- as.tbl(read.csv(file=paste0('openmole/exploration/',resprefix,'.csv')))

# filter NAs and absurb utility values
res=res[res$averageUtility<quantile(res$averageUtility,c(0.99),na.rm = T)&!is.na(res$averageUtility),]

# sample hists
#seed=0
seed=42
set.seed(seed)
res$id = paste0(res$gravityDecay,res$innovationDecay,res$mutationRate,res$newInnovationHierarchy,res$utilityStd,res$earlyAdoptersRate,res$utilityDistribution)
intid = 1:length(unique(res$id));names(intid)<-unique(res$id)
res$id = as.character(intid[res$id])
ids = sample(unique(res$id),size = 20,replace = F)

for(indic in indics){
  g=ggplot(res[res$id%in%ids,],aes_string(x=indic,color="id",group="id"))
  g+geom_density()+facet_wrap(~utilityDistribution)+scale_color_discrete()+stdtheme
  ggsave(file=paste(resdir,indic,'_seed',seed,'.png'),width=20,height=15,units='cm')
}


# sharpes
sres = res %>% group_by(id) %>% summarize(
  sdAverageDiversity=sd(averageDiversity),meanAverageDiversity=mean(averageDiversity),medianAverageDiversity=median(averageDiversity),sharpeAverageDiversity=abs(meanAverageDiversity/sdAverageDiversity),
  sdAverageInnovation=sd(averageInnovation),meanAverageInnovation=mean(averageInnovation),medianAverageInnovation=median(averageInnovation),sharpeAverageInnovation=abs(meanAverageInnovation/sdAverageInnovation),
  sdAverageUtility=sd(averageUtility),meanAverageUtility=mean(averageUtility),medianAverageUtility=median(averageUtility),sharpeAverageUtility=abs(meanAverageUtility/sdAverageUtility),
  sdFinalHierarchy=sd(finalHierarchy),meanFinalHierarchy=mean(finalHierarchy),medianFinalHierarchy=median(finalHierarchy),sharpeFinalHierarchy=abs(meanFinalHierarchy/sdFinalHierarchy)
)
summary(sres)

# relative distances
reldistance <- function(indic,sdindic){
  c(2*abs(matrix(rep(sres[[indic]],nrow(res)),nrow = nrow(res),byrow = T) - matrix(rep(sres[[indic]],nrow(res)),nrow = nrow(res),byrow = F))/(matrix(rep(sres[[sdindic]],nrow(res)),nrow = nrow(res),byrow = T) + matrix(rep(sres[[sdindic]],nrow(res)),nrow = nrow(res),byrow = F)))
}

summary(reldistance("meanAverageDiversity","sdAverageDiversity"))
summary(reldistance("medianAverageDiversity","sdAverageDiversity"))

summary(reldistance("meanAverageInnovation","sdAverageInnovation"))
summary(reldistance("medianAverageInnovation","sdAverageInnovation"))

summary(reldistance("meanAverageUtility","sdAverageUtility"))
summary(reldistance("medianAverageUtility","sdAverageUtility"))

summary(reldistance("meanFinalHierarchy","sdFinalHierarchy"))
summary(reldistance("medianFinalHierarchy","sdFinalHierarchy"))


####
# exploration
resprefix = '20200429_181134_GRID_GRID'
resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanEvolution/Results/EvolutionInnovation/',resprefix,'/');dir.create(resdir)

res <- as.tbl(read.csv(file=paste0('openmole/exploration/',resprefix,'.csv')))
res=res[res$averageUtility<quantile(res$averageUtility,c(0.95),na.rm = T)&!is.na(res$averageUtility),]
res1=res

for(indic in indics){
  
  for(distrib in unique(res$utilityDistribution)){
    for(earlyAdoptersRate in unique(res$earlyAdoptersRate)){
      for(newInnovationHierarchy in unique(res$newInnovationHierarchy)){
        
        g=ggplot(res[res$utilityDistribution==distrib&res$earlyAdoptersRate==earlyAdoptersRate&res$newInnovationHierarchy==newInnovationHierarchy,],
                 aes_string(x="gravityDecay",y=indic,group="innovationDecay",color="innovationDecay")
                 )
        g+geom_point(pch='.')+geom_smooth()+facet_grid(mutationRate~utilityStd,scales = 'free')+stdtheme
        ggsave(filename = paste0(resdir,indic,'-gravityDecay_color-innovationDecay_facet-mutationRate-utilityStd_newInnovationHierarchy',newInnovationHierarchy,'_earlyAdoptersRate',earlyAdoptersRate,'_distrib',distrib,'.png'),width=30,height=20,units='cm')
        
      }
    }
  }
  
}

#
resprefix= '20200429_215249_GRID_GRID'
resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanEvolution/Results/EvolutionInnovation/',resprefix,'/');dir.create(resdir)

res <- as.tbl(read.csv(file=paste0('openmole/exploration/',resprefix,'.csv')))
res=res[res$averageUtility<quantile(res$averageUtility,c(0.95),na.rm = T)&!is.na(res$averageUtility),]
res2=res

for(indic in indics){
  for(distrib in unique(res$utilityDistribution)){
      for(newInnovationHierarchy in unique(res$newInnovationHierarchy)){
        
        g=ggplot(res[res$utilityDistribution==distrib&res$newInnovationHierarchy==newInnovationHierarchy,],
                 aes_string(x="gravityDecay",y=indic,group="innovationDecay",color="innovationDecay")
        )
        g+geom_smooth(se = F)+facet_grid(mutationRate~earlyAdoptersRate,scales = 'free')+
          xlab(expression(d[G]))+ylab(indicnames[[indic]])+scale_color_continuous(name=expression(d[I]))+stdtheme
        ggsave(filename = paste0(resdir,indic,'-gravityDecay_color-innovationDecay_facet-mutationRate-earlyAdoptersRate_newInnovationHierarchy',newInnovationHierarchy,'_distrib',distrib,'.png'),width=30,height=20,units='cm')
        
        #g+geom_point(pch='.')+geom_smooth(se = F)+facet_grid(mutationRate~earlyAdoptersRate,scales = 'free')+
        #  xlab(expression(d[G]))+ylab(indicnames[[indic]])+scale_color_continuous(name=expression(d[I]))+stdtheme
        #ggsave(filename = paste0(resdir,indic,'POINTS-gravityDecay_color-innovationDecay_facet-mutationRate-earlyAdoptersRate_newInnovationHierarchy',newInnovationHierarchy,'_distrib',distrib,'.png'),width=30,height=20,units='cm')
        
        #g+geom_boxplot()+facet_grid(mutationRate~earlyAdoptersRate,scales = 'free')+
        #  xlab(expression(d[G]))+ylab(indicnames[[indic]])+scale_color_continuous(name=expression(d[I]))+stdtheme
        #ggsave(filename = paste0(resdir,indic,'BOXPLOT-gravityDecay_color-innovationDecay_facet-mutationRate-earlyAdoptersRate_newInnovationHierarchy',newInnovationHierarchy,'_distrib',distrib,'.png'),width=30,height=20,units='cm')
        
      }
    }
}

# correlation matrices

allres=rbind(res1[,indics],res2[,indics])
cor(res1[,indics])
cor(res2[,indics])
cor(allres)
cormat=matrix(0,length(indics),length(indics))
cormatmin=cormat=matrix(0,length(indics),length(indics))
cormatmax=cormat=matrix(0,length(indics),length(indics))
for(i in 1:length(indics)){
  for(j in 1:length(indics)){
    rho=cor.test(unlist(allres[,i]),unlist(allres[,j]))
    cormat[i,j]=rho$estimate;cormatmin[i,j]=rho$conf.int[1];cormatmax[i,j]=rho$conf.int[2]
  }
}
cormatnames=c("D","I","U","P")
rownames(cormat)=cormatnames;colnames(cormat)=cormatnames
rownames(cormatmin)=cormatnames;colnames(cormatmin)=cormatnames
rownames(cormatmax)=cormatnames;colnames(cormatmax)=cormatnames


library(corrplot)
png(filename = paste0(resdir,'corrmat_indics.png'),width = 20,height = 20,units = 'cm',res = 150)
corrplot(cormat,lowCI.mat = cormatmin,uppCI.mat = cormatmax,mar = c(0,0,0,0))
dev.off()

m=rbind(res1[,indics],res2[,indics])
for(j in 1:ncol(m)){m[,j]=(m[,j]-min(m[,j]))/(max(m[,j])-min(m[,j]))}
summary(prcomp(m))



############
# calibration

resprefix = 'CALIBRATION_GRID_20200429_220019'
resdir = paste0(Sys.getenv('CS_HOME'),'/UrbanEvolution/Results/EvolutionInnovation/',resprefix,'/');dir.create(resdir)

res <- as.tbl(read.csv(file=paste0('openmole/calibration/',resprefix,'/population9000.csv')))

res = res[res$evolution.samples>20,]
res$averageUtility = - res$oppAverageUtility
res$averageDiversity = - res$oppAverageDiversity

for(param in params){
  g=ggplot(res,aes_string(x="averageDiversity",y="averageUtility",color=param,size="evolution.samples"))
  g+geom_point(alpha=0.6)+xlab("Diversity")+ylab("Utility")+scale_color_continuous(name=paramnames[[param]])+scale_size_continuous(name="Samples")+stdtheme
  ggsave(filename = paste0(resdir,"paretoDiversity-Utility_color",param,'.png'),width=20,height=18,units='cm')
}






