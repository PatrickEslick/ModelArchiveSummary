#A function to support array plotting for ggplot2 and related objects
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

findCensored <- function(site, pcode, start, end) {
  wc <- readNWISqw(site, pcode, start, end)
  wc <- wc[wc$remark_cd=="<",c("sample_dt","remark_cd","result_va")]
  wc$value <- paste(wc$remark_cd, wc$result_va, sep=" ")
  wc <- wc[,c("sample_dt","value")]
  names(wc) <- c("date","value")
  wc <- na.omit(wc)
  wc$date <- as.Date(wc$date, format="%Y-%m-%d")
  return(wc)
}

scaleLog <- function(dataset) {
  dMin <- min(dataset)
  dMax <- max(dataset)
  lowerPower <- floor(log10(dMin))
  higherPower <- ceiling(log10(dMax))
  minLimit <- 10^lowerPower
  maxLimit <- 10^higherPower
  return(c(minLimit,maxLimit))
}

scaleLN <- function(dataset) {
  dMin <- min(dataset)
  dMax <- max(dataset)
  lowerPower <- floor(log(dMin))
  higherPower <- ceiling(log(dMax))
  breaks <- exp(lowerPower:higherPower)
  breaks <- signif(breaks,3)
  return(breaks)
} 

cvsummary <- function(data = DAAG::houseprices, form.lm = formula(sale.price ~ 
                                                                    area), m = 3, dots = FALSE, seed = 29, plotit = c("Observed", 
                                                                                                                      "Residual"), main = "Cross-validation", 
                      legend.pos = "topleft", printit = TRUE) {
  gphtype <- ""
  if (is.logical(plotit)) {
    if (plotit) 
      gphtype <- "Observed"
  }
  else if (is.character(plotit)) {
    if (!(plotit[1] %in% c("Observed", "Residual", ""))) 
      stop(paste("Illegal argument plotit =", plotit[1]))
    gphtype <- plotit[1]
    if (plotit[1] %in% c("Observed", "Residual")) 
      plotit <- TRUE
  }
  else stop("Argument plotit must be logical or character")
  if (class(form.lm) == "formula") 
    form <- form.lm
  else if (class(form.lm) %in% c("call", "lm")) 
    form <- formula(form.lm)
  else stop("form.lm must be formula or call or lm object")
  formtxt <- deparse(form)
  mf <- model.frame(form, data = data)
  ynam <- attr(mf, "names")[attr(attr(mf, "terms"), "response")]
  data.lm <- lm(mf)
  tm <- terms(mf)
  xcolumns <- labels(tm)
  n <- nrow(data)
  data[, ynam] <- model.response(mf)
  data[, "Predicted"] <- predict(data.lm)
  data[, "cvpred"] <- numeric(n)
  yval <- mf[, ynam]
  if (gphtype == "Residual") 
    yval <- yval - data[, "Predicted"]
  if (!is.null(seed)) 
    set.seed(seed)
  n <- dim(data)[1]
  rand <- sample(n)%%m + 1
  foldnum <- sort(unique(rand))
  for (i in foldnum) {
    rows.in <- rand != i
    rows.out <- rand == i
    subs.lm <- lm(form, data = data[rows.in, ])
    data[rows.out, "cvpred"] <- predict(subs.lm, newdata = data[rows.out,])
  }
  if (length(xcolumns) == 1) {
    stline <- TRUE
    xnam <- xcolumns
  }
  else {
    stline <- FALSE
    xnam <- "Predicted"
  }
  if (plotit) {
    oldpar <- par(mar = par()$mar - c(1, 0, 2, 0))
    on.exit(par(oldpar))
    coltypes <- palette()[c(2, 3, 6, 1, 4:5, 7)]
    if (m > 7) 
      coltypes <- c(coltypes, rainbow(m - 7))
    ltypes <- 1:m
    ptypes <- 2:(m + 1)
    par(lwd = 2)
    if (stline) 
      xlab <- xnam
    else {
      xlab <- "Predicted (fit to all data)"
      cat("\n")
      warning(paste("\n\n As there is >1 explanatory variable, cross-validation\n", 
                    "predicted values for a fold are not a linear function\n", 
                    "of corresponding overall predicted values.  Lines that\n", 
                    "are shown for the different folds are approximate\n"))
    }
    ylab <- ynam
    if (gphtype == "Residual") 
      ylab <- paste(ynam, " (offset from predicted using all data)")
    plot(as.formula(paste("yval ~", xnam)), data = data, 
         ylab = ylab, type = "p", pch = ptypes[rand], col = coltypes[rand], 
         cex = 1.25, xlab = xlab)
    title(main = main, cex = 1.05)
    if (dots) {
      with(data, points(as.formula(paste("yval ~", xnam)), 
                        data = data, type = "p", pch = 16, col = coltypes[rand], 
                        cex = 1))
    }
  }
  
  fold_number <- vector()
  MSE <- vector()
  if (printit | plotit) {
    sumss <- 0
    sumdf <- 0
    for (i in foldnum) {
      rows.in <- rand != i
      rows.out <- rand == i
      n.out <- sum(rows.out)
      resid <- data[rows.out, ynam] - data[rows.out, "cvpred"]
      ss <- sum(resid^2)
      sumss <- sumss + ss
      if (printit) {
        fold_data <- t(cbind(data[rows.out, c(xnam, "cvpred", ynam)], resid))
        rownames(fold_data) = c(xnam, "cvpred", ynam, "CV residual")
        fold_number[length(fold_number)+1] <- i
        MSE[length(MSE) + 1] <- signif((ss/n.out), 3)
      }
      if (plotit) {
        xval <- data[rows.out, xnam]
        nminmax <- c(which.min(xval), which.max(xval))
        cvpred <- data[rows.out, "cvpred"]
        if (gphtype == "Residual") 
          cvpred <- cvpred - data[rows.out, "Predicted"]
        points(xval, cvpred, col = coltypes[i], pch = ptypes[i], 
               cex = 0.75, lwd = 1)
        n1 <- which.min(xval)
        n2 <- which.max(xval)
        fold.lm <- lm(cvpred ~ xval)
        fold.b <- coef(fold.lm)
        lines(xval[c(n1, n2)], fold.b[1] + fold.b[2] * 
                xval[c(n1, n2)], col = coltypes[i], lty = ltypes[i])
        topleft <- par()$usr[c(1, 4)]
        par(lwd = 1, col = 1)
        legend(x = legend.pos, legend = paste("Fold", 
                                              1:m), pch = ptypes, lty = ltypes, col = coltypes, 
               cex = 0.75)
      }
    }
  }
  sumdf <- sum(!is.na(data[, "Predicted"]))
  data <- data.frame(fold_number, MSE)
  invisible(data)
}

scaleDate <- function(dataset) {
  dataset <- as.numeric(format(dataset, "%Y"))
  wilk_year <- extended(max(dataset),min(dataset),5,Q=c(1,2,4,5),only.loose=TRUE)
  wilk_year <- as.Date(paste(as.character(wilk_year),"-01-01",sep=""),format="%Y-%m-%d")
  return(wilk_year)
}

bigLabel <- function(Abbrev, pCode) {
  info <- tryCatch({readNWISpCode(pCode)}, silent=TRUE)
  if(class(info) == "try-error" | nrow(info)==0) {
    return(Abbrev)
  } else {
    units <- strsplit(info$parameter_nm, ",")[[1]]
    units <- units[length(units)]
    littleunits <- info$parameter_units
    label <- paste(Abbrev," in",units," (",littleunits,")",sep="")
    return(label)
  }
}

multReg2 <- function(object) {
  aovtab <- Anova(object)
  regsum <- summary(object)
  if(length(object$coefficients) > 2) {
    vif <- vif(object)
  } else {
    vif <- vector()
  }
  if (is.null(object[["x"]])) 
    x <- model.matrix(object)[, -1L, drop = FALSE]
  else x <- object$x[, -1L, drop = FALSE]
  if (is.null(object$y)) 
    y <- model.extract(model.frame(object), "response")
  else y <- object$y
  respvar <- make.names(object$terms[[2L]])
  if (length(respvar) > 1L) 
    respvar <- paste(respvar, collapse = ".")
  if (is.null(object$weights)) 
    wt <- rep(1, length(y))
  else wt <- object$weights
  lsout <- lsfit(x, y, wt)
  lsdiag <- ls.diag(lsout)
  lev <- lsdiag$hat
  cooksd <- lsdiag$cooks
  std.res <- lsdiag$std.res
  dfits <- lsdiag$dfits
  stud.res <- lsdiag$stud.res
  p <- ncol(x) + 1
  n <- nrow(x)
  cvlev <- ((3*p)/n)
  cvdfit <- 2 * sqrt(p/n)
  cvcook <- qf(0.1, p + 1, n - p)
  pck <- c(lev > cvlev | cooksd > cvcook | abs(dfits) > cvdfit)
  yhat <- object$fitted.values
  diagstats <- data.frame(y = y, yhat = yhat, resids = lsout$residuals, 
                          stnd.res = std.res, stud.res = stud.res, leverage = lev, 
                          cooksD = cooksd, dfits = dfits)
  names(diagstats)[1L] <- respvar
  cvs <- c(cvlev, cvcook, cvdfit)
  names(cvs) <- c("leverage", "cooksD", "dfits")
  stuffout <- list(aovtab = aovtab, parmests = regsum, vif = vif, 
                   diagstats = diagstats, crit.val = cvs, flagobs = pck, 
                   object = object, x = x)
  oldClass(stuffout) <- "multReg"
  return(stuffout)
}