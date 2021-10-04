## E.coli

library(ggplot2)
library(data.table)
library(dplyr)

## change doc path
docpath <- 'data.csv'
data <- read.csv(docpath)

## explore
colnames(data); head(data)
summary(data)

ggboxplot(signal, x = 'pH', y = 'c1') ## change pH c1 to each combo
## looks like no interaction between each trts.



###################################################
## randomization!! not by default, from 1 to 10, etc
## autocorrealtion, systematic error etc
order_num <- sample(1:20, size = 10) # change size as needed





###################################################
## ANOVA test --> choose effects, pH? concerntration of which one?
# all, + means itself, * means interaction 
mod.all <- aov(signal ~ pH + c1 + c2 + c3 + c4 +
                   pH * c1 + pH * c2 + pH * c3 + pH * c4 +
                   c1 * c2 + c1 * c3 + c1 * c4 + 
                   c2 * c3 + c2 * c4 + c3 * c4 +
                   pH * c1 * c2 +
                   pH * c1 * c3 +
                   pH * c1 * c4 +
                   pH * c2 * c3 +
                   pH * c2 * c4 +
                   pH * c3 * c4 +
                   c1 * c2 * c3 +
                   c1 * c2 * c4 +
                   c2 * c3 * c4 +
                   c1 * c3 * c4 +
                   pH * c1 * c2 * c3 + 
                   pH * c1 * c3 * c4 + 
                   pH * c2 * c3 * c4 +
                   c1 * c2 * c3 * c4 +
                   pH * c1 * c2 * c3 * c4, data = data)
summary(mod.all) # no 5 way inter

# ignore pH or c1 or c2,..., check 4 way inter
mod.4way <- aov(signal ~ pH + c1 + c2 + c3 + c4 +
                    pH * c1 + pH * c2 + pH * c3 + pH * c4 +
                    c1 * c2 + c1 * c3 + c1 * c4 + 
                    c2 * c3 + c2 * c4 + c3 * c4 +
                    pH * c1 * c2 +
                    pH * c1 * c3 +
                    pH * c1 * c4 +
                    pH * c2 * c3 +
                    pH * c2 * c4 +
                    pH * c3 * c4 +
                    c1 * c2 * c3 +
                    c1 * c2 * c4 +
                    c2 * c3 * c4 +
                    c1 * c3 * c4 +
                    pH * c1 * c2 * c3 + 
                    pH * c1 * c3 * c4 + 
                    pH * c2 * c3 * c4 +
                    c1 * c2 * c3 * c4, data = data)
summary(mod.4way) # no 4 way inter

## repeat check for 3 way. 2way. no inter, now only keep eyes on each trt
## check all, check ignore each compare
mod1 <- aov(signal ~ pH + c1 + c2 + c3 + c4, data = data)
mod2 <- aov(signal ~ pH + c1 + c2 + c3, data = data)
mod3 <- aov(signal ~ pH + c1 + c2 + c4, data = data)
mod4 <- aov(signal ~ pH + c2 + c3 + c4, data = data)
mod5 <- aov(signal ~ c1 + c2 + c3 + c4, data = data)
library(AICcmodavg)
model.set <- list(mod1, mod2, mod3, mod4, mode5)
model.names <- c("all", "no c4", "no c3", "no c2", "no c1")
aictab(model.set, modnames = model.names) ## looks like c2 no so useful

## plot to check again, scatter check
par(mfrow = c(5))
for(i in 1:5){boxplot(signal ~ data[,i])}
## check paper, if c2 is really no use!!!

###################################################
## check, remove
data <- fread(docpath) %>% data.table(keep.rownames = T)
dat <- data[, -3]
m1 <- lm(signal ~ pH, dat)
summary(m1)
shapiro.test(m1$residuals)
library(lmtest); bptest(m1)
dwtest(m1)
## repeat for c1, c3, c4
## pH & c4 potential non-linear!!

## logisitic??
f1 <- glm(signal ~ pH, data = dat, family = "binomial")
summary(f1)
f2 <- glm(signal ~ c4, data = dat, family = "binomial")
summary(f2)

## to enhance signal, increase the c1, c2 if possible,
## refine your search for pH and c4 withn .... range 



###################################################
## results tests

## sensitivity = true pos / (true pos + false neg)
respath <- ''
res <- fread(respath) %>% data.table(keep.rownames = T)
summary(res)

sens <- res$tp / (res$tp + res$fn)
## sens ur ans, lit, paper ans, check p-value!!!
t.test(sens, lit, alternative = "two.sided", var.equal = FALSE)

