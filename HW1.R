# Homework 1: Testing Trading Strategies

# read in the size and book-to-market portfolio data
size <- read.csv("Portfolios_Formed_on_ME.csv", header=TRUE)
btm <- read.csv("Portfolios_Formed_on_BE-ME.csv", header=TRUE)
# read in the excess market returns and Fama-French factors
factors <- read.csv("F-F_Research_Data_Factors.csv", header=TRUE)

# convert the dates (originally in YYYYMM integer form) into years and month
#  size:
years <- substr(as.character(size$date), 1, 4)
months <- substr(as.character(size$date), 5, 6)
size <- cbind(year=years, month=months, size[, -1])

#  btm:
years <- substr(as.character(btm$date), 1, 4)
months <- substr(as.character(btm$date), 5, 6)
btm <- cbind(year=years, month=months, btm[, -1])

#  factors:
years <- substr(as.character(factors$date), 1, 4)
months <- substr(as.character(factors$date), 5, 6)
factors <- cbind(year=years, month=months, factors[, -1])

# merge the factors with the size and btm data frames by year and month
size <- merge(size, factors, by=c("year", "month"))
btm <- merge(btm, factors, by=c("year", "month"))

# sort each data frame by year and month
size <- size[order(btm$year, btm$month),]
btm <- btm[order(btm$year, btm$month),]

# prepare for looping across years
# NOTE: I'm not sure whether we are supposed to filter out years that didn't
#       include the entire fiscal year (trimming the last incomplete year)
size$pyear <- factor(as.character(size$year))
btm$pyear <- factor(as.character(btm$year))
years <- levels(size$pyear)
nyear <- length(years)

# calculate the zero-cost long-short size and book-to-market portfolio returns
size$zcp <- size$Q5 - size$Q1
btm$zcp <- btm$Q5 - btm$Q1

# initialize arrays for the annual coefficient estimates
capm_output.size <- array(NaN, dim=c(nyear, 2))
colnames(capm_output.size) <- c("alpha", "beta")
capm_output.btm <- array(NaN, dim=c(nyear, 2))
colnames(capm_output.btm) <- c("alpha", "beta")
ff_output.size <- array(NaN, dim=c(nyear, 4))
colnames(ff_output.size) <- c("alpha", "beta", "gamma", "delta")
ff_output.btm <- array(NaN, dim=c(nyear, 4))
colnames(ff_output.btm) <- c("alpha", "beta", "gamma", "delta")

# run the regressions for each year
for (t in 1:nyear) {
    # CAPM regressions
    #  size:
    y <- size$zcp[size$pyear == years[t]]
    X <- cbind(rep(1, length(y)), 
               size$EXMKT[size$pyear == years[t]])
    capm_output.size[t,] <- solve(t(X) %*% X, t(X) %*% y)
    #  btm:
    y <- btm$zcp[btm$pyear == years[t]]
    X <- cbind(rep(1, length(y)), 
               btm$EXMKT[btm$pyear == years[t]])
    capm_output.btm[t,] <- solve(t(X) %*% X, t(X) %*% y)
    
    # Fama-French regressions
    #  size:
    y <- size$zcp[size$pyear == years[t]]
    X <- cbind(rep(1, length(y)), 
               size$EXMKT[size$pyear == years[t]], 
               size$SMB[size$pyear == years[t]], 
               size$HML[size$pyear == years[t]])
    ff_output.size[t,] <- solve(t(X) %*% X, t(X) %*% y)
    #  btm:
    y <- btm$zcp[btm$pyear == years[t]]
    X <- cbind(rep(1, length(y)), 
               btm$EXMKT[btm$pyear == years[t]], 
               btm$SMB[btm$pyear == years[t]], 
               btm$HML[btm$pyear == years[t]])
    ff_output.btm[t,] <- solve(t(X) %*% X, t(X) %*% y)
}

# calculate average coefficients and test for significance
#  CAPM:
#   size:
capm_mean.size = apply(capm_output.size, 2, mean)
capm_sd.size = apply(capm_output.size, 2, sd)
capm_se.size = capm_sd.size / sqrt(nyear)
capm_t.size = capm_mean.size / capm_se.size
#   btm:
capm_mean.btm = apply(capm_output.btm, 2, mean)
capm_sd.btm = apply(capm_output.btm, 2, sd)
capm_se.btm = capm_sd.btm / sqrt(nyear)
capm_t.btm = capm_mean.btm / capm_se.btm
#  Fama-French:
#   size:
ff_mean.size = apply(ff_output.size, 2, mean)
ff_sd.size = apply(ff_output.size, 2, sd)
ff_se.size = ff_sd.size / sqrt(nyear)
ff_t.size = ff_mean.size / ff_se.size
#   btm:
ff_mean.btm = apply(ff_output.btm, 2, mean)
ff_sd.btm = apply(ff_output.btm, 2, sd)
ff_se.btm = ff_sd.btm / sqrt(nyear)
ff_t.btm = ff_mean.btm / ff_se.btm

# group and print results
print("Size Portfolio CAPM Results")
print(rbind(mean=capm_mean.size, "t-stat"=capm_t.size))
print("Book-to-Market Portfolio CAPM Results")
print(rbind(mean=capm_mean.btm, "t-stat"=capm_t.btm))
print("Size Portfolio Fama-French Results")
print(rbind(mean=ff_mean.size, "t-stat"=ff_t.size))
print("Book-to-Market Portfolio Fama-French Results")
print(rbind(mean=ff_mean.btm, "t-stat"=ff_t.btm))