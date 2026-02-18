## civlcontrolexp analytical codes
## Author: Gento Kato
## Date: 2026/02/19

#################
## Preparation ##
#################

## Clean workspace
rm(list=ls())

## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd() # 確認

## Create a directory to export output（out）
if (!"./out" %in% list.dirs()) dir.create("./out")

## Import required packages
library(estimatr) # lm with robust SE
library(ggplot2) # export plots
library(texreg) # regression tables
library(haven) # dealing with haven-style data
library(labelled) # check haven labels
library(superb)
library(lmtest) # using coefci

## Interaction marginal effect function
intereff <- function(m0,main,mod,modrange,nsim) {
  modval = seq(modrange[1],modrange[2],length=nsim)
  mainmod = paste(main,mod,sep=":")
  if (!mainmod%in%rownames(vcov(m0))) mainmod = paste(mod,main,sep=":")
  if ("df"%in%names(m0)) {
    dfset <- m0$df[1] 
    # Assuming that df is the same across all.
    # CAUTION: The above is not true if lm_robust's se_type="CR2".
  } else {
    dfset <- df.residual(m0)
  }
  cfset <- c(coef(m0)[which(names(coef(m0))==main)],
             coef(m0)[which(names(coef(m0))==mainmod)])
  vcset <- c(vcov(m0)[which(rownames(vcov(m0))==main),
                      which(colnames(vcov(m0))==main)],
             vcov(m0)[which(rownames(vcov(m0))==mainmod),
                      which(colnames(vcov(m0))==mainmod)],
             vcov(m0)[which(rownames(vcov(m0))==main),
                      which(colnames(vcov(m0))==mainmod)])
  
  out = data.frame(mod = modval,
                   est = cfset[1]+cfset[2]*modval,
                   se = sqrt(vcset[1]+modval^2*vcset[2]+2*modval*vcset[3]),
                   qt90 = qt(0.95,dfset),
                   qt95 = qt(0.975,dfset))
  out$lo90 = out$est-out$se*out$qt90
  out$up90 = out$est+out$se*out$qt90
  out$lo95 = out$est-out$se*out$qt95
  out$up95 = out$est+out$se*out$qt95
  out$pval = (1 - pt(abs(out$est/out$se),dfset))*2
  
  return(out)
}

## Generate predictions
genpr <- function(dpr,mpr,setx,setlevx,setlabx,
                  setm=NULL,setmvals=NULL,datalab=NULL) {
  
  ## Generate simulation data
  simdt <- na.omit(dpr[,all.vars(mpr$terms)])
  ## Extract all x values (if more than 11, limit to 11)
  simx <- sort(unique(simdt[,setx]))
  if(length(simx)>11) simx <- seq(min(simx),max(simx),length=11)

  ## Summarize simulation values (if m not available)
  if (is.null(setm)) {
    
    simv <- data.frame(simx = simx)
    
  ## Setting m values (if available)
  } else {
    
    ## Extract all m values
    simm <- sort(unique(simdt[,setm]))
    if(length(simm)>11) simm <- seq(min(simm),max(simm),length=11)
    if (is.numeric(simm)) {
      if (any(!as.numeric(names(setmvals))%in%simm)) {
        simm <- sort(unique(c(simm,as.numeric(names(setmvals)))))
      }
    }
    ## Summarize simulation values
    simv <- data.frame(simx = rep(simx,each=length(simm)), 
                       simm = simm)
    
  }
  
  ## Export predictions
  prout <- as.data.frame(t(apply(simv, 1, function(k) {
    tmpdt <- simdt
    tmpdt[,setx] <- k[1]
    if (!is.null(setm)) tmpdt[,setm] <- k[2]
    tmp <- colMeans(as.data.frame(predict(mpr, newdata=tmpdt, se.fit=TRUE)))
    tmp <- c(k, tmp[1:2], 
             tmp[1]-tmp[2]*qt(0.975,df=mpr$df[1]),
             tmp[1]+tmp[2]*qt(0.975,df=mpr$df[1]),
             tmp[1]-tmp[2]*qt(0.95,df=mpr$df[1]),
             tmp[1]+tmp[2]*qt(0.95,df=mpr$df[1]))
    if (is.null(setm)) {
      names(tmp) <- c("x","pr","se",
                      "lo95","up95","lo90","up90")
    } else {
      names(tmp) <- c("x","m","pr","se",
                      "lo95","up95","lo90","up90")
    }
    return(tmp)
  })))
  ### Assign labels to x
  prout$labelledx <- factor(prout$x,levels=setlevx,labels=setlabx)
  ### Assign labels to m
  if (!is.null(setm)) {
    prout$labelledm <- 
      factor(unlist(setmvals)[match(prout$m,names(setmvals))],
             levels = unlist(setmvals))
  }
  ### Assign common label to data
  if (!is.null(datalab)) {
    prout$datalab <- datalab
  }
  
  return(prout)
}


## Import data
# d <- readRDS("dataset_g1_main.rds")
# d <- subset(d, id >= 5) # exclude invalid respondents
# d$id <- 1:nrow(d)
# saveRDS(d, "dataset_g1_main_cleaned.rds")
d <- readRDS("dataset_g1_main_cleaned.rds")

## Check age ranges
library(stringi)
tmp <- as.numeric(stringi::stri_trans_nfkc(d$v24))
table(tmp)
hist(tmp)

###############
## Variables ##
###############

## New dataset
dn <- data.frame(id = d$id)

## Trust in the JSDF
table(d$v19, exclude=F) ## distribution
dn$trust_jsdf <- as.numeric(d$v19)　## insert
table(dn$trust_jsdf, exclude=F) ## distribution

## Trust in the PM
table(d$v16, exclude=F) # distribution
dn$trust_pm <- as.numeric(d$v16) # insert 
table(dn$trust_pm, exclude=F) # distribution

## Human casualties
table(d$e01, exclude=F) ## 1,2 are no, 3,4 are yes
dn$casualties <- NA
dn$casualties[which(d$e01%in%c(1,2))] <- 0
dn$casualties[which(d$e01%in%c(3,4))] <- 1
table(dn$casualties, exclude=F) ## distribution

## Failed civilian control (0 = success, 1 = failure)
table(d$e01, exclude=F) ## 1,3 are success、2,4 are failure
dn$failedcontrol <- NA
dn$failedcontrol[which(d$e01%in%c(1,3))] <- 0
dn$failedcontrol[which(d$e01%in%c(2,4))] <- 1
table(dn$failedcontrol, exclude=F) ## distribution

## Ideology (the higher the conservation)
table(d$v03, exclude=F) ## distribution
dn$ideology <- as.numeric(d$v03)
table(dn$ideology, exclude=F)

## Political interest
## 0= no interest to 3 = have interest
table(d$v02, exclude=F)
var_label(d$v02)
val_labels(d$v02)
dn$polint <- 4 - d$v02
table(dn$polint)

## Political knowledge
## 0 all in correct to 3 all correct
table(d$v08, exclude=F)
var_label(d$v08)
val_labels(d$v08)
dn$polkn1 <- ifelse(d$v08%in%2, 1, 0)
table(d$v09, exclude=F)
var_label(d$v09)
val_labels(d$v09)
dn$polkn2 <- ifelse(d$v09%in%4, 1, 0)
table(d$v11, exclude=F)
var_label(d$v11)
val_labels(d$v11)
dn$polkn3 <- ifelse(d$v11%in%1, 1, 0)
dn$polknall <- 
  dn$polkn1 + dn$polkn2 + dn$polkn3
table(dn$polknall)

## Gender (Male)
table(d$v23, exclude=F)
var_label(d$v23)
val_labels(d$v23)
dn$male <- ifelse(d$v23%in%1, 1, 0)
table(dn$male)

## Age
table(d$v24, exclude=F)
var_label(d$v24)
library(stringi)
dn$age <- as.numeric(stringi::stri_trans_nfkc(d$v24))/10
table(dn$age)

## Education
table(d$v25, exclude=F)
var_label(d$v25)
val_labels(d$v25)
dn$edu <- ifelse(d$v25%in%666, NA, 
                 ifelse(d$v25%in%1,0,d$v25-2))
table(dn$edu)

## Marital Status
table(d$v27, exclude=F)
var_label(d$v27)
val_labels(d$v27)
dn$married <- ifelse(d$v27%in%1, 1, 0)
table(dn$married)

## No kids
table(d$v28, exclude=F)
var_label(d$v28)
val_labels(d$v28)
dn$nokid <- ifelse(d$v28%in%1, 1, 0)
table(dn$nokid)

## The place of residence (urban rural)
## 0 = town/villages, 1=small cities, 
## 2 = mid=sized cities, 3=big cities suburbs
## 4 = big cities
table(d$v30, exclude=F)
var_label(d$v30)
val_labels(d$v30)
dn$citysize <- 5 - d$v30
table(dn$citysize)

## Satisficer Detection Question (Pretreatment)
table(d$v10, exclude=F)
var_label(d$v10)
val_labels(d$v10)
dn$nosatisficer <- ifelse(d$v10==5, 1, NA)
table(dn$nosatisficer, exclude=F)

## Manipulation Check Question
var_label(d$v20)
val_labels(d$v20)
table(d$v20, dn$casualties, exclude=F)
var_label(d$v21)
val_labels(d$v21)
table(d$v21, exclude=F)
var_label(d$v22)
val_labels(d$v22)
table(d$v22, dn$failedcontrol, exclude=F)
dn$mchecksuccess <- 1
dn$mchecksuccess[which(!dn$casualties%in%0 & d$v20%in%2)] <- NA
dn$mchecksuccess[which(!dn$casualties%in%1 & d$v20%in%1)] <- NA
dn$mchecksuccess[which(!d$v21%in%2)] <- NA
dn$mchecksuccess[which(!dn$failedcontrol%in%0 & d$v22%in%1)] <- NA
dn$mchecksuccess[which(!dn$failedcontrol%in%1 & d$v22%in%2)] <- NA
table(dn$mchecksuccess, exclude=F)

## Exclude respondents with missing values
dim(dn)
dn <- na.omit(dn)
dim(dn)

##############
## Analysis ##
##############

### Variable Labels
vnmap <- list("(Intercept)" = "(Intercept)",
              "failedcontrol" = "Failed civilian control",
              "ideology" = "Conservative ideology",
              "failedcontrol:ideology" = "Failure * Ideology",
              "casualties" = "Human casualties",
              "polint" = "Political interest (0-3)",
              "polknall" = "Political knowledge (0-3)",
              "male" = "Gender (male)",
              "age" = "Age (by 10 years)",
              "edu" = "Education (0-1)",
              "married" = "Married",
              "nokid" = "No children",
              "citysize" = "City size (0-4)")

## Baseline Models
m00 <- lm_robust(trust_jsdf ~ failedcontrol + casualties + 
                   polint + polknall + male + age + edu + 
                   married + nokid + citysize, data=dn) 
summary(m00)
m01 <- lm_robust(trust_jsdf ~ failedcontrol + 
                   polint + polknall + male + age + edu + 
                   married + nokid + citysize, 
                 data=subset(dn, casualties==0))
summary(m01)
m02 <- lm_robust(trust_jsdf ~ failedcontrol + 
                   polint + polknall + male + age + edu + 
                   married + nokid + citysize, 
                 data=subset(dn, casualties==1))
summary(m02)

## Interacted Models
m00x <- lm_robust(trust_jsdf ~ failedcontrol*ideology + casualties + 
                    polint + polknall + male + age + edu + 
                    married + nokid + citysize, data=dn) 
summary(m00x)
m01x <- lm_robust(trust_jsdf ~ failedcontrol*ideology + 
                    polint + polknall + male + age + edu + 
                    married + nokid + citysize, 
                 data=subset(dn, casualties==0))
summary(m01x)
m02x <- lm_robust(trust_jsdf ~ failedcontrol*ideology + 
                    polint + polknall + male + age + edu + 
                    married + nokid + citysize, 
                 data=subset(dn, casualties==1))
summary(m02x)

## Table E.3
screenreg(list(m00,m00x,m01,m01x,m02,m02x),
          include.ci=FALSE, digits=3,
          stars = c(0.001,0.01,0.05,0.1), symbol="+",
          custom.coef.map = vnmap,
          custom.header = list("All" = 1:2, 
                               "Without Casualties" = 3:4,
                               "With Casualties" = 5:6),
          custom.model.names = rep(c("Baseline","Interacted"),3),
          custom.note = "%stars. Robust standard errors in parentheses.") 
texreg(list(m00,m00x,m01,m01x,m02,m02x),
       include.ci=FALSE, digits=3,
       stars = c(0.001,0.01,0.05,0.1), symbol="\\dagger",
       custom.coef.map = vnmap,
       custom.header = list("All" = 1:2, 
                            "Without Casualties" = 3:4,
                            "With Casualties" = 5:6),
       custom.model.names = rep(c("Baseline","Interacted"),3),
       custom.note = "%stars. Robust standard errors in parentheses.",       
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE,
       table = FALSE, file = "./out/controlled_nosatisficer_tab_all.tex") 
tmp <- readLines("./out/controlled_nosatisficer_tab_all.tex")
writeLines(gsub("{dagger}","{\\dagger}",tmp, fixed=TRUE),
           "./out/controlled_nosatisficer_tab_all.tex")

## PM as an Outcome
m03 <- lm_robust(trust_pm ~ failedcontrol + casualties + 
                   polint + polknall + male + age + edu + 
                   married + nokid + citysize, data=dn) 
summary(m03)
m031 <- lm_robust(trust_pm ~ failedcontrol + 
                    polint + polknall + male + age + edu + 
                    married + nokid + citysize, 
                 data=subset(dn, casualties==0)) 
summary(m031)
m032 <- lm_robust(trust_pm ~ failedcontrol + 
                    polint + polknall + male + age + edu + 
                    married + nokid + citysize, 
                 data=subset(dn, casualties==1)) 
summary(m032)

## Interacted Models
m03x <- lm_robust(trust_pm ~ failedcontrol*ideology + casualties + 
                    polint + polknall + male + age + edu + 
                    married + nokid + citysize, data=dn) 
summary(m00x)
m031x <- lm_robust(trust_pm ~ failedcontrol*ideology + 
                     polint + polknall + male + age + edu + 
                     married + nokid + citysize, 
                  data=subset(dn, casualties==0))
summary(m01x)
m032x <- lm_robust(trust_pm ~ failedcontrol*ideology + 
                     polint + polknall + male + age + edu + 
                     married + nokid + citysize, 
                  data=subset(dn, casualties==1))
summary(m02x)

## Table E.4
screenreg(list(m03,m03x,m031,m031x,m032,m032x),
          include.ci=FALSE, digits=3,
          stars = c(0.001,0.01,0.05,0.1), symbol="+",
          custom.coef.map = vnmap,
          custom.header = list("All" = 1:2, 
                               "Without Casualties" = 3:4,
                               "With Casualties" = 5:6),
          custom.model.names = rep(c("Baseline","Interacted"),3),
          custom.note = "%stars. Robust standard errors in parentheses.") 
texreg(list(m03,m03x,m031,m031x,m032,m032x),
       include.ci=FALSE, digits=3,
       stars = c(0.001,0.01,0.05,0.1), symbol="\\dagger",
       custom.coef.map = vnmap,
       custom.header = list("All" = 1:2, 
                            "Without Casualties" = 3:4,
                            "With Casualties" = 5:6),
       custom.model.names = rep(c("Baseline","Interacted"),3),
       custom.note = "%stars. Robust standard errors in parentheses.",       
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE,
       table = FALSE, file = "./out/controlled_nosatisficer_tab_all_pm.tex") 
tmp <- readLines("./out/controlled_nosatisficer_tab_all_pm.tex")
writeLines(gsub("{dagger}","{\\dagger}",tmp, fixed=TRUE),
           "./out/controlled_nosatisficer_tab_all_pm.tex")

##################
## JSDF OUTCOME ##
##################

## Baseline Treatment Effects
cfdt <- as.data.frame(
  rbind(c(summary(m00)$coefficients[2,],coefci(m00, df=m00$df, level=0.90)[2,]),
        c(summary(m01)$coefficients[2,],coefci(m01, df=m01$df, level=0.90)[2,]),
        c(summary(m02)$coefficients[2,],coefci(m02, df=m02$df, level=0.90)[2,]))
)
colnames(cfdt) <- c("est","se","t","p","lo95","up95","df","lo90","up90")
cfdt$mname <- c("All","Without casualties","With casualties")
cfdt$mname <- factor(cfdt$mname, levels=rev(cfdt$mname))

ggplot(cfdt, aes(x=est, y=mname, xmin=lo95, xmax=up95)) + 
  geom_vline(aes(xintercept=0), linetype=2) + 
  geom_errorbarh(aes(xmin=lo90,xmax=up90), height=0, 
                 linewidth=2, alpha=0.7) + 
  geom_errorbarh(height=0.2) + 
  geom_point(size=3) + 
  coord_cartesian(xlim=c(-0.75,0.1)) + 
  labs(x="Coefficient of failed civilian control\n(with 90% and 95% confidence intervels)",
       y=NULL) + 
  theme_bw() + 
  theme(axis.text.y = element_text(size=12))
ggsave("./out/controlled_nosatisficer_fig_maineff.pdf", width=6, height=3)

## Predicted values (baseline effects)
prbase <- rbind(
  genpr(dpr = dn, 
        mpr = m00,
        setx = "failedcontrol",
        setlevx = c(0,1),
        setlabx = c("Success","Failure"),
        datalab = "All"),
  genpr(dpr = dn, 
        mpr = m01,
        setx = "failedcontrol",
        setlevx = c(0,1),
        setlabx = c("Success","Failure"),
        datalab = "Without casualties"),
  genpr(dpr = dn, 
        mpr = m02,
        setx = "failedcontrol",
        setlevx = c(0,1),
        setlabx = c("Success","Failure"),
        datalab = "With casualties")
)
prbase$datalab <- 
  factor(prbase$datalab, levels=unique(prbase$datalab))

ggplot(prbase, aes(x=labelledx, y=pr)) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95,
                    color=labelledx), width=0) + 
  geom_errorbar(aes(ymin=lo90, ymax=up90,
                    color=labelledx), width=0,
                linewidth=1.2, alpha=0.5) + 
  geom_point(aes(shape=labelledx,color=labelledx),
             size=2.5) + 
  facet_grid(.~datalab) + 
  coord_cartesian(ylim = c(3.3,6.7)) +
  scale_shape_discrete(name = "", guide = "none") + 
  scale_color_brewer(name = "", guide = "none", 
                     type = "qual", palette = 2) + 
  labs(subtitle="Human casualties conditions",
       x="Civilian control", 
       y="Average predicted value of\ntrust in the JSDF (1-7)",
       caption="Note: Lines indicate 90% and 95% confidence intervals.") + 
  theme_bw() + 
  theme(plot.subtitle = element_text(hjust=0.5))
ggsave("./out/controlled_nosatisficer_fig_pred.pdf", width=7, height=3)

## Moderated treatment effects
cfdt <- rbind(intereff(m00x,"failedcontrol","ideology",c(0,10),11),
              intereff(m01x,"failedcontrol","ideology",c(0,10),11),
              intereff(m02x,"failedcontrol","ideology",c(0,10),11))
cfdt$mname <- rep(c("All","Without casualties","With casualties"),
                  each = 11)
cfdt$mname <- factor(cfdt$mname, levels=(unique(cfdt$mname)))
## Figure E.3
ggplot(cfdt, aes(y=est, x=mod, ymin=lo95, ymax=up95)) + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_ribbon(alpha=0.3) + 
  geom_ribbon(aes(ymin=lo90,ymax=up90), alpha=0.4) + 
  geom_line() +
  facet_grid(.~mname) + 
  scale_x_continuous(breaks=0:10) + 
  coord_cartesian(ylim=c(-2,1.5)) +
  labs(subtitle="Human casualties conditions",
       y="Marginal effect of\nfailed civilian control\non trust in the JSDF",
       x="Ideology (0=strongly liberal; 10=strongly conservative)",
       caption="Note: Shades indicate 90% and 95% confidence intervals.") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust=0.5))
ggsave("./out/controlled_nosatisficer_fig_maineff_interacted.pdf", width=7, height=3)

## Predicted values (moderated effects)
printeracted <- rbind(
  genpr(dpr = dn, 
        mpr = m00x,
        setx = "ideology", 
        setlevx = c(0:10),
        setlabx = c(0:10),
        setm = "failedcontrol", 
        setmvals = list("0"="Success (0)",
                        "1"="Failure (1)"),
        datalab = "All"),
  genpr(dpr = dn, 
        mpr = m01x,
        setx = "ideology", 
        setlevx = c(0:10),
        setlabx = c(0:10),
        setm = "failedcontrol", 
        setmvals = list("0"="Success (0)",
                        "1"="Failure (1)"),
        datalab = "Without casualties"),
  genpr(dpr = dn, 
        mpr = m02x,
        setx = "ideology", 
        setlevx = c(0:10),
        setlabx = c(0:10),
        setm = "failedcontrol", 
        setmvals = list("0"="Success (0)",
                        "1"="Failure (1)"),
        datalab = "With casualties")
)
printeracted$datalab <- 
  factor(printeracted$datalab, levels=unique(printeracted$datalab))

ggplot(na.omit(printeracted), aes(x=labelledx, y=pr)) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95,
                    color = labelledm), width=0,
                position = position_dodge(width=0.3)) + 
  geom_errorbar(aes(ymin=lo90, ymax=up90,
                    color = labelledm), 
                width=0, linewidth = 1.2, alpha=0.5,
                position = position_dodge(width=0.3)) + 
  geom_point(aes(color=labelledm,shape=labelledm),
             size=2.5,
             position = position_dodge(width=0.3)) + 
  coord_cartesian(ylim = c(3.3,6.7)) + 
  facet_grid(.~datalab) + 
  scale_shape_discrete(name = "Civilian control") + 
  scale_linetype_discrete(name = "Civilian control") + 
  scale_color_brewer(name = "Civilian control", 
                     type = "qual", palette = 2) + 
  labs(subtitle="Human casualties conditions",
       x="Ideology", 
       y="Average predicted value of\ntrust in the JSDF (1-7)",
       caption = "Note: Lines indicate 90% and 95% confidence intervals.") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
ggsave("./out/controlled_nosatisficer_fig_pred_interacted.pdf", width=7, height=3)

################
## PM OUTCOME ##
################

## Baseline Treatment Effects
cfdt <- as.data.frame(
  rbind(c(summary(m03)$coefficients[2,],coefci(m03, df=m03$df, level=0.90)[2,]),
        c(summary(m031)$coefficients[2,],coefci(m031, df=m031$df, level=0.90)[2,]),
        c(summary(m032)$coefficients[2,],coefci(m032, df=m032$df, level=0.90)[2,]))
)
colnames(cfdt) <- c("est","se","t","p","lo95","up95","df","lo90","up90")
cfdt$mname <- c("All","Without casualties","With casualties")
cfdt$mname <- factor(cfdt$mname, levels=rev(cfdt$mname))

ggplot(cfdt, aes(x=est, y=mname, xmin=lo95, xmax=up95)) + 
  geom_vline(aes(xintercept=0), linetype=2) + 
  geom_errorbarh(aes(xmin=lo90,xmax=up90), height=0, 
                 linewidth=2, alpha=0.7) + 
  geom_errorbarh(height=0.2) + 
  geom_point(size=3) + 
  coord_cartesian(xlim=c(-0.75,0.1)) + 
  labs(x="Coefficient of failed civilian control\n(with 90% and 95% confidence intervel)",
       y=NULL) + 
  theme_bw() + 
  theme(axis.text.y = element_text(size=12))
ggsave("./out/controlled_nosatisficer_fig_maineff_pm.pdf", width=6, height=3)

## Predicted values (baseline effects)
prbase <- rbind(
  genpr(dpr = dn, 
        mpr = m03,
        setx = "failedcontrol",
        setlevx = c(0,1),
        setlabx = c("Success","Failure"),
        datalab = "All"),
  genpr(dpr = dn, 
        mpr = m031,
        setx = "failedcontrol",
        setlevx = c(0,1),
        setlabx = c("Success","Failure"),
        datalab = "Without casualties"),
  genpr(dpr = dn, 
        mpr = m032,
        setx = "failedcontrol",
        setlevx = c(0,1),
        setlabx = c("Success","Failure"),
        datalab = "With casualties")
)
prbase$datalab <- 
  factor(prbase$datalab, levels=unique(prbase$datalab))

ggplot(prbase, aes(x=labelledx, y=pr)) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95,
                    color=labelledx), width=0) + 
  geom_errorbar(aes(ymin=lo90, ymax=up90,
                    color=labelledx), width=0,
                linewidth=1.2, alpha=0.5) + 
  geom_point(aes(shape=labelledx,color=labelledx),
             size=2.5) + 
  facet_grid(.~datalab) + 
  coord_cartesian(ylim = c(1.3,4.7)) + 
  scale_shape_discrete(name = "", guide = "none") + 
  scale_color_brewer(name = "", guide = "none", 
                     type = "qual", palette = 2) + 
  labs(subtitle="Human casualties conditions",
       x="Civilian control", 
       y="Average predicted value of\ntrust in the Prime Minister (1-7)",
       caption="Note: Lines indicate 90% and 95% confidence intervals.") + 
  theme_bw() + 
  theme(plot.subtitle = element_text(hjust=0.5))
ggsave("./out/controlled_nosatisficer_fig_pred_pm.pdf", width=7, height=3)

## Moderated treatment effects
cfdt <- rbind(intereff(m03x,"failedcontrol","ideology",c(0,10),11),
              intereff(m031x,"failedcontrol","ideology",c(0,10),11),
              intereff(m032x,"failedcontrol","ideology",c(0,10),11))
cfdt$mname <- rep(c("All","Without casualties","With casualties"),
                  each = 11)
cfdt$mname <- factor(cfdt$mname, levels=(unique(cfdt$mname)))
## Figure E.4
ggplot(cfdt, aes(y=est, x=mod, ymin=lo95, ymax=up95)) + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_ribbon(alpha=0.3) + 
  geom_ribbon(aes(ymin=lo90,ymax=up90), alpha=0.4) + 
  geom_line() +
  facet_grid(.~mname) + 
  scale_x_continuous(breaks=0:10) + 
  coord_cartesian(ylim=c(-2,1.5)) +
  labs(subtitle="Human casualties conditions",
       y="Marginal effect of\nfailed civilian control\non trust in the Prime Minister",
       x="Ideology (0=strongly liberal; 10=strongly conservative)",
       caption="Note: Shades indicate 90% and 95% confidence intervals.") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust=0.5))
ggsave("./out/controlled_nosatisficer_fig_maineff_interacted_pm.pdf", width=7, height=3)

## Predicted values (moderated effects)
printeracted <- rbind(
  genpr(dpr = dn, 
        mpr = m03x,
        setx = "ideology", 
        setlevx = c(0:10),
        setlabx = c(0:10),
        setm = "failedcontrol", 
        setmvals = list("0"="Success (0)",
                        "1"="Failure (1)"),
        datalab = "All"),
  genpr(dpr = dn, 
        mpr = m031x,
        setx = "ideology", 
        setlevx = c(0:10),
        setlabx = c(0:10),
        setm = "failedcontrol", 
        setmvals = list("0"="Success (0)",
                        "1"="Failure (1)"),
        datalab = "Without casualties"),
  genpr(dpr = dn, 
        mpr = m032x,
        setx = "ideology", 
        setlevx = c(0:10),
        setlabx = c(0:10),
        setm = "failedcontrol", 
        setmvals = list("0"="Success (0)",
                        "1"="Failure (1)"),
        datalab = "With casualties")
)
printeracted$datalab <- 
  factor(printeracted$datalab, levels=unique(printeracted$datalab))

ggplot(na.omit(printeracted), aes(x=labelledx, y=pr)) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95,
                    color = labelledm), width=0,
                position = position_dodge(width=0.3)) + 
  geom_errorbar(aes(ymin=lo90, ymax=up90,
                    color = labelledm), 
                width=0, linewidth = 1.2, alpha=0.5,
                position = position_dodge(width=0.3)) + 
  geom_point(aes(color=labelledm,shape=labelledm),
             size=2.5,
             position = position_dodge(width=0.3)) + 
  coord_cartesian(ylim = c(1.3,4.7)) + 
  facet_grid(.~datalab) + 
  scale_shape_discrete(name = "Civilian control") + 
  scale_linetype_discrete(name = "Civilian control") + 
  scale_color_brewer(name = "Civilian control", 
                     type = "qual", palette = 2) + 
  labs(subtitle="Human casualties conditions",
       x="Ideology", 
       y="Average predicted value of\ntrust in the JSDF (1-7)",
       caption = "Note: Lines indicate 90% and 95% confidence intervals.") + 
  theme_bw() + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
ggsave("./out/controlled_nosatisficer_fig_pred_interacted_pm.pdf", width=7, height=3)
