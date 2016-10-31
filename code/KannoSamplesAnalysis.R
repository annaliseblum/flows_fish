## Output Analysis - Yoichiro's full model
##Modified: Oct 27,2016
#run My_Kanno.R

names(outpaperFull_df)

MeansoutpaperFull<-colMeans(outpaperFull_df)

#### Effects boxplots (Figure 2 in Kanno et al)  ####
#to replicate Figure 2: (yess)
summary(outpaperFull_df$`g.0[1,1]`)
summary(outpaperFull_df$`g.0[2,1]`)
summary(outpaperFull_df$`g.0[3,1]`)
summary(outpaperFull_df$`g.0[4,1]`)
summary(outpaperFull_df$`g.0[5,1]`)
summary(outpaperFull_df$`g.0[6,1]`)
summary(outpaperFull_df$`g.0[7,1]`)
summary(outpaperFull_df$`g.0[8,1]`)

load("~/flows_fish/output/outpaperFull_df.rdata")
pdf("plots/My_Fig2.pdf") #
labels=paste(c("Summer Precip","Fall Precip","Winter Precip","Spring Precip","Summer Temp","Fall Temp","Winter Temp","Spring Temp"))
par(mar = c(7, 4, 4, 2) + 0.1)
boxplot(outpaperFull_df$`g.0[1,1]`,outpaperFull_df$`g.0[2,1]`,outpaperFull_df$`g.0[3,1]`,outpaperFull_df$`g.0[4,1]`,
        outpaperFull_df$`g.0[5,1]`,outpaperFull_df$`g.0[6,1]`,outpaperFull_df$`g.0[7,1]`,outpaperFull_df$`g.0[8,1]`,
        xaxt="n", xlab="",ylab="effect size",
        main="Figure 2 from Kanno et al (2016) - All weather covariates")
axis(1, at=1:8,labels = FALSE)
## Plot x axis labels at default tick marks
text(1:8, par("usr")[3] - 0.03, srt = 45, adj = 1,
     labels = labels, xpd = TRUE)
abline(0,0)
dev.off()

##Kanno model
pdf("plots/MyFig2Kanno.pdf") #
#labels
labels=paste(c("Summer Low Flow","Fall Low Flow","Winter High Flow","Spring High Flow","Summer Temp","Fall Temp","Winter Temp","Spring Temp"))
par(mar = c(7, 4, 4, 2) + 0.1)
## Create plot with no x axis and no x axis label
boxplot(outJUI4$sims.list$g.0[,1,1],outJUI4$sims.list$g.0[,2,1],outJUI4$sims.list$g.0[,3,1],outJUI4$sims.list$g.0[,4,1],
        outJUI4$sims.list$g.0[,5,1],outJUI4$sims.list$g.0[,6,1],outJUI4$sims.list$g.0[,7,1],outJUI4$sims.list$g.0[,8,1],
          xaxt="n", xlab="",ylab="effect size",
        main="Duration of flows (High=above 95%ile; Low= below 5%ile)")
axis(1, at=1:8,labels = FALSE)
## Plot x axis labels at default tick marks
text(1:8, par("usr")[3] - 0.03, srt = 45, adj = 1,
     labels = labels, xpd = TRUE)
abline(0,0)
dev.off()

####with JAGSUI output
#to replicate Figure 2: $mean   ..$ g.0     : num [1:8, 1] 0.0786 0.1867 -0.3545 0.0402 -0.0632 ...
plot(outJUI$mean$g.0)
boxplot(outJUI$sims.list$g.0[,1,1])

pdf("plots/My_Fig2.pdf") #
boxplot(outJUI$sims.list$g.0[,1,1],outJUI$sims.list$g.0[,2,1],outJUI$sims.list$g.0[,3,1],outJUI$sims.list$g.0[,4,1],
        outJUI$sims.list$g.0[,5,1],outJUI$sims.list$g.0[,6,1],outJUI$sims.list$g.0[,7,1],outJUI$sims.list$g.0[,8,1])
abline(0,0)
dev.off()

pdf("plots/My_Fig2AvgQwinter.pdf") #
boxplot(outJUI2$sims.list$g.0[,1,1],outJUI2$sims.list$g.0[,2,1],outJUI2$sims.list$g.0[,3,1],outJUI2$sims.list$g.0[,4,1],
        outJUI2$sims.list$g.0[,5,1],outJUI2$sims.list$g.0[,6,1],outJUI2$sims.list$g.0[,7,1],outJUI2$sims.list$g.0[,8,1])
abline(0,0)
dev.off()

pdf("plots/My_Fig2AvgQflows.pdf") #
boxplot(outJUI3$sims.list$g.0[,1,1],outJUI3$sims.list$g.0[,2,1],outJUI3$sims.list$g.0[,3,1],outJUI3$sims.list$g.0[,4,1],
        outJUI3$sims.list$g.0[,5,1],outJUI3$sims.list$g.0[,6,1],outJUI3$sims.list$g.0[,7,1],outJUI3$sims.list$g.0[,8,1])
abline(0,0)
dev.off()

par(mar=c(6, 4.1, 4.1, 2.1))

pdf("plots/EffectAvgSflows.pdf") #
#labels
labels=paste(c("Summer Flow","Fall Flow","Winter Flow","Spring Flow","Summer Temp","Fall Temp","Winter Temp","Spring Temp"))
par(mar = c(7, 4, 4, 2) + 0.1)
## Create plot with no x axis and no x axis label
boxplot(outJUI3$sims.list$g.0[,1,1],outJUI3$sims.list$g.0[,2,1],outJUI3$sims.list$g.0[,3,1],outJUI3$sims.list$g.0[,4,1],
        outJUI3$sims.list$g.0[,5,1],outJUI3$sims.list$g.0[,6,1],outJUI3$sims.list$g.0[,7,1],outJUI3$sims.list$g.0[,8,1],
        col = c("gold","orange","lightblue","lightgreen"),  xaxt="n", xlab="",ylab="effect size",
        main="Predicted average seasonal flows")
axis(1, at=1:8,labels = FALSE)
## Plot x axis labels at default tick marks
text(1:8, par("usr")[3] - 0.03, srt = 45, adj = 1,
     labels = labels, xpd = TRUE)
abline(0,0)
dev.off()

pdf("plots/EffectDurationflows.pdf") #
#labels
labels=paste(c("Summer Low Flow","Fall Low Flow","Winter High Flow","Spring High Flow","Summer Temp","Fall Temp","Winter Temp","Spring Temp"))
par(mar = c(7, 4, 4, 2) + 0.1)
## Create plot with no x axis and no x axis label
boxplot(outJUI4$sims.list$g.0[,1,1],outJUI4$sims.list$g.0[,2,1],outJUI4$sims.list$g.0[,3,1],outJUI4$sims.list$g.0[,4,1],
        outJUI4$sims.list$g.0[,5,1],outJUI4$sims.list$g.0[,6,1],outJUI4$sims.list$g.0[,7,1],outJUI4$sims.list$g.0[,8,1],
        col = c("gold","orange","lightblue","lightgreen"),  xaxt="n", xlab="",ylab="effect size",
        main="Duration of flows (High=above 95%ile; Low= below 5%ile)")
axis(1, at=1:8,labels = FALSE)
## Plot x axis labels at default tick marks
text(1:8, par("usr")[3] - 0.03, srt = 45, adj = 1,
     labels = labels, xpd = TRUE)
abline(0,0)
dev.off()

#mu is average fish abundance
summary(outpaperFull_df$`mu[1]`)

#first 3335 are YOY age, confirm, yes!
115*29

#after N names:
# "b.day[1]"     
# [6672] "b.day[2]"      "b.site[1,1]"   "b.site[2,1]"   "b.site[3,1]"   "b.site[1,2]"   "b.site[2,2]"   "b.site[3,2]"  
# [6679] "g.0[1,1]"      "g.0[2,1]"      "g.0[3,1]"      "g.0[4,1]"      "g.0[5,1]"      "g.0[6,1]"      "g.0[7,1]"     
# [6686] "g.0[8,1]"      "g.0[1,2]"      "g.0[2,2]"      "g.0[3,2]"      "g.0[4,2]"      "g.0[5,2]"      "g.0[6,2]"     
# [6693] "g.0[7,2]"      "g.0[8,2]"      "mu[1]"         "mu[2]"         "p.b[1,1]"      "p.b[2,1]"      "p.b[3,1]"     
# [6700] "p.b[1,2]"      "p.b[2,2]"      "p.b[3,2]"      "p.mean[1]"     "p.mean[2]"     "sigma2.b[1,1]" "sigma2.b[2,1]"
# [6707] "sigma2.b[3,1]" "sigma2.b[4,1]" "sigma2.b[5,1]" "sigma2.b[6,1]" "sigma2.b[7,1]" "sigma2.b[8,1]" "sigma2.b[1,2]"
# [6714] "sigma2.b[2,2]" "sigma2.b[3,2]" "sigma2.b[4,2]" "sigma2.b[5,2]" "sigma2.b[6,2]" "sigma2.b[7,2]" "sigma2.b[8,2]"
# [6721] "sigmaN2[1]"    "sigmaN2[2]"   

##### detection rate  ####
summary(outpaperFull_df$`p.mean[1]`)

#pull YOY abundance, then take means
MeanN_YOY<-MeansoutpaperFull[1:3335]
meanN<-as.data.frame(MeanN_YOY)
meanN$site<- rep(1:115,29) #substr(names(MeanN_YOY))  #maybe don't use substrings
meanN$year<- sort(rep(1:29,115))

#get first pass counts
countYOYP1<-as.data.frame(countAr[,,1,1])
countYOYP1$site<- 1:nrow(countYOYP1) #label sites 1 to 115
YOYP1_L<-melt(countYOYP1, id = "site")
names(YOYP1_L)<-c("site","yearfull","P1count")
YOYP1_L$year<-as.integer(as.factor(YOYP1_L$yearfull))

##merge two datasets together
class(YOYP1_L$site);class(meanN$site)
class(YOYP1_L$year);class(meanN$year)
N_CP1<-merge(YOYP1_L,meanN,by=c("site","year"))
ccN_CP1<-N_CP1[complete.cases(N_CP1),]

##plot P1 count vs abundance
plot(ccN_CP1$P1count,ccN_CP1$MeanN_YOY)
text("corr=0.9928, n=1182")

#ggplot2
ggplot(ccN_CP1, aes(x = P1count, y = MeanN_YOY)) + geom_point() + annotate("text", x = 50, y = 600, label = "corr=0.9928 (n=1182)") + 
  labs(x="First Pass Count",y="Abundance estimated by model in Kanno et al (2016)")+
  ggtitle("Comparison of first pass counts to abundance for YOY in SNP 1982-2010")+
  geom_abline(intercept = 0, slope = 1) + annotate("text", x = 320, y = 280, label = "1:1 line") 

#### BIAS ####
##Detection
# for(i in 1:nSites) {
#   for(t in 1:nYears){
#     for(j in 1:nAges){
#       y[i,t,j,1] ~ dbin(p[i,t,j], N[i,t,j])
#       y[i,t,j,2] ~ dbin(p[i,t,j]*(1-p[i,t,j]), N[i,t,j])
#       y[i,t,j,3] ~ dbin(p[i,t,j]*(1-p[i,t,j])*(1-p[i,t,j]), N[i,t,j])  
#       
#       p[i,t,j] <- 1/(1 + exp(-lp.lim[i,t,j]))
#       lp.lim[i,t,j] <- min(999, max(-999, lp[i,t,j]))
#       lp[i,t,j] <- p.mu[j] + p.b[1,j]*julian[i,t] + p.b[2,j]*prcpTot[i,t] + p.b[3,j]*area[i]
#     }
#   } 

#so first pass: y[i,t,1,pass]; Site i, Year t - y is the observed count data

