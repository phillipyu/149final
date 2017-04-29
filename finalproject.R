### STAT 149 FINAL PROJECT SCRIPT ###
library(mgcv) # for gam

test = read.csv(file.choose())
train = read.csv(file.choose())
sample = read.csv(file.choose())

# from missing data lectures folder
na.convert.mean = function (frame) 
{
  vars <- names(frame)
  if (!is.null(resp <- attr(attr(frame, "terms"), "response"))) {
    vars <- vars[-resp]
    x <- frame[[resp]]
    pos <- is.na(x)
    if (any(pos)) {
      frame <- frame[!pos, , drop = FALSE]
      warning(paste(sum(pos), "observations omitted due to missing values in the response"))
    }
  }
  for (j in vars) {  #j is variable names
    x <- frame[[j]]
    pos <- is.na(x)
    if (any(pos)) {
      if (length(levels(x))) {   # factors
        xx <- as.character(x)
        xx[pos] <- "NA"
        x <- factor(xx, exclude = NULL)
      }
      else if (is.matrix(x)) {   # matrices
        ats <- attributes(x)
        x.na <- 1*pos
        #               x[pos] <- 0
        w <- !pos
        n <- nrow(x)
        TT <- array(1, c(1, n))
        xbar <- (TT %*% x)/(TT %*% w)
        xbar <- t(TT) %*% xbar
        x[pos] <- xbar[pos]
        attributes(x) <- ats
        attributes(x.na) <- ats
        dimnames(x.na)[[2]]=paste(dimnames(x)[[2]],".na",sep='')
        frame[[paste(j,".na",sep='')]] <- x.na 
      } else {   # ordinary numerical vector
        ats <- attributes(x)
        x[pos] <- mean(x[!pos])
        #               x[pos] <- 0
        x.na <- 1*pos
        frame[[paste(j,".na",sep='')]] <- x.na 
        attributes(x) <- ats
      }
      frame[[j]] <- x
    }
  }
  frame
}

train.na = na.convert.mean(train)
test.na = na.convert.mean(test)
summary(train.na)

# simple GLM with missing values factored 
train.glm.na = glm(voted ~ ., family=binomial, data=train.na)
alias(train.glm.na)

anova(train.glm.na) # vccdistance.na, hd.na, cd.na, otherchrst, dbdistance, vccdistance are variables that could be removed
train.glm.v1 = glm(voted ~ . - vccdistance.na - hd.na, family=binomial, data=train.na)
anova(train.glm.na, train.glm.v1, test="Chi") # not significant; removed vccdistance.na, hd.na

train.glm.v2 = glm(voted ~ . - vccdistance.na - hd.na - otherchrst, family=binomial, data=train.na)
anova(train.glm.v1, train.glm.v2, test="Chi") # not significant; removed otherchrst

train.glm.v3 = glm(voted ~ . - vccdistance.na - hd.na - otherchrst - vccdistance, family=binomial, data=train.na)
anova(train.glm.v2, train.glm.v3, test="Chi") # not significant; removed vccdistance

train.glm.v4 = glm(voted ~ . - vccdistance.na - hd.na - otherchrst - vccdistance
                   - dbdistance, family=binomial, data=train.na)
anova(train.glm.v3, train.glm.v4, test="Chi") # not significant; removed dbdistance

predict.glm.v4 = predict(train.glm.v4, test.na, type="response")

write.table(predict.glm.v4, file = "glmaod.csv",sep=",")

# simple GAM with all predictors
train.gam.na = gam(voted ~ gender + s(cd,k=8) + s(hd) + s(age) + s(dbdistance)
                   + s(vccdistance) + party + racename + s(hsonly) + s(mrrg)
                   + s(chldprsnt) + s(cath) + s(evang) + s(nonchrst) + s(otherchrst)
                   + s(days.since.reg) + dbdistance.na + vccdistance.na + cd.na + hd.na,
                   family=binomial, data=train.na)
summary(train.gam.na)
 
predict.gam = predict(train.gam.na, test.na, type="response")

write.table(predict.gam, file = "gam.csv",sep=",")
