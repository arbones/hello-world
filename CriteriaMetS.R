#' ---
#' title: "Criterios de MetS"
#' date: "Junio/2015"
#' output:
#'   md_document:
#'     variant: markdown_phpextra
#' ---

```{r, message=F, warning=F}
require(compareGroups)

require(dplyr)






load("~/Dropbox/SyncBriefcase/LAB/IACS/AWHS/R_analysis/TABLAS/tE.Rdata")

tE=subset(tE,!is.na(gen)&!is.na(num.ms))


####define la variable 'dosis de alelo E4'
tE$e4[tE$gen%in%c('E2E2','E2E3','E3E3')]='00'
####tE$e4[tE$gen%in%c('E3E4')]='01'
tE$e4[tE$gen%in%c('E2E4','E3E4')]='01'
tE$e4[tE$gen%in%c('E4E4')]='11'
tE$e4=as.factor(tE$e4)



tE$num.ms=ifelse(tE$num.ms%in%c('3','4','5'),'3+',tE$num.ms)
tE$num.ms2=ifelse(tE$num.ms2%in%c('3','4','5'),'3+',tE$num.ms2)
tE$num.ms3=ifelse(tE$num.ms3%in%c('3','4','5'),'3+',tE$num.ms3)

tE[,63:77]=lapply(tE[,63:77],as.factor)

#' ## PREVALENCIA DE SINDROME METABOLICO 
#' ### Segun donde se tenga en cuenta la medicacion hipolipemiante 


######################POBLACION TOTAL
t2=compareGroups(e4~waist.ms+hta.ms+glu.ms+tg.ms+hdl.ms+num.ms+metS
                   ,data=tE)
t3=compareGroups(e4~tg.ms2+hdl.ms2+num.ms2+metS2
                 ,data=tE)
t4=compareGroups(e4~tg.ms3+hdl.ms3+num.ms3+metS3
                 ,data=tE)

c2=createTable(t2,show.p.trend=T,hide.no=0,show.ratio=T)
c3=createTable(t3,show.p.trend=T,hide.no=0,show.ratio=T)
c4=createTable(t4,show.p.trend=T,hide.no=0,show.ratio=T)

tot=rbind('Hypolip. in HDL criterion'=c2,'Hypolip. in TG criterion'=c3,'Hypolip. NOT considered'=c4)###une las tablas
tot

#'##' Por categorias de BMI
#############################POR CATEGORIAS DE BMI

t5=compareGroups(e4~waist.ms+hta.ms+glu.ms+tg.ms+hdl.ms+num.ms+metS+tg.ms2+hdl.ms2+num.ms2+metS2+tg.ms3+hdl.ms3+num.ms3+metS3
                 ,data=tE,subset = bmi<25)

t6=compareGroups(e4~waist.ms+hta.ms+glu.ms+tg.ms+hdl.ms+num.ms+metS+tg.ms2+hdl.ms2+num.ms2+metS2+tg.ms3+hdl.ms3+num.ms3+metS3
                 ,data=tE,subset = bmi>=25&bmi<30)

t7=compareGroups(e4~waist.ms+hta.ms+glu.ms+tg.ms+hdl.ms+num.ms+metS+tg.ms2+hdl.ms2+num.ms2+metS2+tg.ms3+hdl.ms3+num.ms3+metS3
                 ,data=tE,subset = bmi>=30)
c5=createTable(t5,show.p.trend=T,hide.no=0,show.ratio=T)
c6=createTable(t6,show.p.trend=T,hide.no=0,show.ratio=T)
c7=createTable(t7,show.p.trend=T,hide.no=0,show.ratio=T)
#' ### Normal weight
c5
#' ### Overweight
c6
#' ### Obese
c7
tot=cbind('Normal Weight'=c5,'Overweight'=c6,'Obese'=c7)###une las tablas
