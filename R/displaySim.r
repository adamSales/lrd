###########################################################################################
### Table 3 simulation (linear)
###########################################################################################
eachCase <- function(case,eff=0)
    sapply(c('sh','ik','cft'),
           function(method) c(bias=case[paste0(method,'.est')]-eff,
                              cover=case[paste0(method,'.CIl')]<=eff &
                                  case[paste0(method,'.CIh')]>=eff,
                              width=case[paste0(method,'.CIh')]-
                                  case[paste0(method,'.CIl')])
        )

levTabCI <- function(res,eff=0){
    out <- NULL
     for(n in c(50,250,2500))
         for(err in c('norm','t')){
             run <- res[[paste(n,eff,err,sep='_')]]
            row <- NULL
            for(meth in c('cft','sh','ik'))
                row <- c(row,
                         if(is.list(run))
                             mean(sapply(run,function(x) x[paste0(meth,'.p')])<0.05,na.rm=TRUE)
                         else mean(run[paste0(meth,'.p'),]<0.05))

             out <- rbind(out,row)
             rownames(out)[nrow(out)] <- paste(err,n)
         }
    colnames(out) <- c('cft','sh','ik')
    out
}

displayCIsimHet <- function(res,tau=0,caption='',label=''){
  ## omit cases where one of the methods (ours?) didn't converge, gave NA
  res <- lapply(res,function(x) t(x[,apply(x,2,function(cc) !any(is.na(cc)))]))

  res <- res[grep(paste0('_tau',tau,'W'),names(res),fixed=TRUE)]
  res <- res[grepl('Wnone|Wt',names(res))]


  tab0 <- lapply(res,function(x)
    apply(sapply(1:nrow(x),function(i) eachCase(x[i,],eff=tau),simplify='array'),
      1:2,mean))

  tab0 <- lapply(tab0,function(xx){
    out <- rbind(sprintf("%.2f",round(xx[1,],2)),sprintf("%i",as.integer(round(xx[2,]*100))),sprintf("%.2f",round(xx[3,],2)))
    dimnames(out) <- dimnames(xx)
    out})

  cat('
\\begin{table}
\\footnotesize
\\begin{tabular}{ccc|ccc|ccc|ccc}
\\hline

&&& \\multicolumn{ 3 }{c}{Permutation}&\\multicolumn{ 3 }{c}{\`\`Limitless\'\'}&\\multicolumn{ 3 }{c}{Local OLS}\\\\
$n$& Effect& Error &', paste(rep(c('Bias','Cover.','Width'),3),collapse='&'),'\\\\
\\hline \n')
  for(n in c(50,250,2500))
#    for(err in c('norm','t')){
    for(rr in c('n0','t0','tt')){
      row <- NULL
      err <- ifelse(rr=='n0','norm','t')
      eff <- ifelse(rr=='tt','$t_3$',0)
      for(meth in c('cft','sh','ik'))
        row <- c(row,tab0[[paste0(n,'_err',err,'_tau',tau,'W',ifelse(rr=='tt','t','none'))]][,meth])
      if(err=='norm'){
        cat('\\hline \n')
        cat('\\multirow{3}{*}{',n,'} &0& $\\mathcal{N}(0,1)$ &')
      } else cat(' & ',eff,'& $t_3$ &')
      cat(paste(row,collapse='&'),'\\\\ \n')
    }
  cat('\\hline
\\end{tabular}
  \\caption{',caption,'}
  \\label{',label,'}\n',sep='')
  cat('\\end{table}\n')
}

dispAllSimp <- function(res){
  res <- sapply(res,function(x) t(x[,apply(x,2,function(cc) !any(is.na(cc)))]),simplify=FALSE)
  tab0 <- lapply(names(res),function(nn){
    x <- res[[nn]]
    tau <- ifelse(grepl('tau0.2',nn,fixed=TRUE),0.2,0)
    apply(sapply(1:nrow(x),function(i) eachCase(x[i,],eff=tau),simplify='array'),
      1:2,mean)
  })

  cond <- strsplit(names(res),'_')
  cond <- lapply(cond,function(x) gsub('err|tau','',x))
  cond <- lapply(cond,function(x) c(x[1],x[2],strsplit(x[3],'W')[[1]]))
  cond <- do.call('rbind',cond)
  out <- data.frame(tab0[[1]],stringsAsFactors=FALSE)
  out <- cbind(cond[rep(1,3),],c('Bias','Coverage','Width'),out)

  for(i in 2:length(tab0))
    out <- rbind(out,cbind(cond[rep(i,3),],
      c('Bias','Coverage','Width'),
      data.frame(tab0[[i]],stringsAsFactors=FALSE)))


  names(out) <- c('n','error','ATE','TE randomness','meas','Limitless','Local-OLS','Permutation')

  out
}


###########################################################################################
### Table 4 simulation (polynomial)
###########################################################################################


#' @export
## dgms <- function(tp){

##     ### the DGMs
##     curve(0.5*x,from=-1,to=1,ylim=c(-2,2),xlab='r',ylab='$\\EE[Y|R=r]$',main='Linear')
##     abline(v=0,lty=2)
##     curve(ifelse(abs(x)>0.5, 3*x+sign(x)*(0.5-3)*0.5,0.5*x),from=-1,to=1,ylim=c(-2,2),xlab='r',ylab='$\\EE[Y|R=r]$',main='Anti-Symmetric')
##     abline(v=0,lty=2)
##   #curve(ifelse(x>0.5,3*x+(0.5-3)*0.5,0.5*x),from=-1,to=1,ylim=c(-2,2),xlab='r',ylab='$\\EE[Y|R=r]$',main='One-Sided')
##   curve(mu4,from=-1,to=1,ylim=c(-2,2),xlab='r',ylab='$\\EE[Y|R=r]$',main='One-Sided')
##     abline(v=0,lty=2)
## }

dgms <- function(){
  lin <- function(x) 0.5*x
  as <- function(x) ifelse(abs(x)>0.5,3*x+sign(x)*(0.5-3)*0.5,lin(x))
  mu4 <- function(x) sin(3*x)
  p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))+xlim(-1,1)+
    theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      #axis.title.x='$r$',
      #axis.title.y='$\\EE[Y|R=r]$',
      legend.position="none",
      ## panel.background=element_blank(),
      ## panel.border=element_blank(),
      ## panel.grid.major=element_blank(),
      ## panel.grid.minor=element_blank(),
      ## plot.background=element_blank()
    )+xlab('$r$')+ylab('$\\EE[Y|R=r]$')

  gridExtra::grid.arrange(p+stat_function(fun=lin)+ggtitle('Linear'),
    p+stat_function(fun=as)+ggtitle('Anti-Symmetric'),
    p+stat_function(fun=mu4)+ggtitle('Sine'),nrow=1)
}

resTab <- function(run,full=FALSE){
    llr <- run[c(nrow(run)-1,nrow(run)),]
    run <- run[-c(nrow(run)-1,nrow(run)),]

    pvals <- run[seq(1,nrow(run)-1,2),]
    ests <- run[seq(2,nrow(run),2),]

    runPsh <- pvals[seq(1,nrow(pvals),2),]
    runPik <- pvals[seq(2,nrow(pvals),2),]

    runEstsh <- ests[seq(1,nrow(ests),2),]
    runEstik <- ests[seq(2,nrow(ests),2),]


    tabFun <- function(runP,runEst,full){
        tab <- rbind(
            level=apply(runP,1,function(x) mean(x<0.05,na.rm=TRUE)),
            RMSE=apply(runEst,1,function(x) sqrt(mean(x^2,na.rm=TRUE))))
        if(full) tab <- rbind(tab,
                              bias=rowMeans(runEst,na.rm=TRUE),
                              sd=apply(runEst,1,sd,na.rm=TRUE))
        tab
    }
    list(tabSH=tabFun(runPsh,runEstsh,full=full),
         tabIK=tabFun(runPik,runEstik,full=full),
         tabLLR=tabFun(rbind(llr[1,]),rbind(llr[2,]),full=full))
}


resTab <- function(run,full=FALSE){
  run <- run[,apply(run,2,function(x) !any(is.na(x)))]
  tabFun <- function(ests)
      rbind(
        bias=rowMeans(rbind(ests)),
        RMSE=sqrt(rowMeans(rbind(ests^2))))

  tabFunP <- function(ps)
    apply(rbind(ps),1,function(x) mean(x<0.05,na.rm=TRUE))


  out <- sapply(c('sh','ik','ll'),function(mm) tabFun(run[grep(paste0(mm,'.est'),rownames(run)),]),
    simplify=FALSE)

  if(full) out <- sapply(c('sh','ik','ll'),
    function(mm) rbind(out[[mm]],level= tabFunP(run[grep(paste0(mm,'.p'),rownames(run)),])),
    simplify=FALSE)

  out
}

#' @export
prntTab <- function(totalPoly,maxDeg=4,full=TRUE,md=FALSE){
    tab <- NULL

    if(maxDeg>(nrow(totalPoly[[1]])-2)/4){
        maxDeg <- (nrow(totalPoly[[1]])-2)/4
        if(!full)
            warning(paste('There don\'t seem to be that many degrees in the simulation.\n Setting maxDeg=',maxDeg))
    }

    ctab <- function(runname,full){
        run <- totalPoly[[runname]]
        res <- resTab(run,full=full)

        cbind(res[['sh']][,1:maxDeg],
              res[['ik']][,1:maxDeg],
              res[['ll']])
    }
    for(dgm in c('lin','antiSym','wass')){
        tab <- rbind(tab,ctab(paste0(dgm,'_t'),full))
        if(full) if(paste0(dgm,'_norm')%in%names(totalPoly)) tab <- rbind(tab,ctab(paste0(dgm,'_norm'),full))
    }
    if(md){
        colnames(tab) <- c(paste('LRD, deg=',1:maxDeg),paste('OLS, deg=',1:maxDeg),'Loc.Lin')
        rownames(tab) <- paste(rep(c('lin','antiSym','sine'),
          each=nrow(tab)/sum(rownames(tab)=='bias')),#,times=2),
          #rep(c('t err','norm err'),each=nrow(tab)/2),
          rownames(tab))
    }
    return(tab)
}

#' @export
polyLatex <- function(tab,full,caption='',label='tab:poly'){
    if(NCOL(tab)!=9) stop('This only works with polynomial degree=1,...,4')
    cat('
        \\begin{table}[ht]
\\centering
\\begin{tabular}{cr|llll|llll|l',ifelse(full,'|llll|llll|l}','}'),'
  \\hline \n')
    if(full) cat('&&\\multicolumn{9}{c|}{$t_3$ Errors} &\\multicolumn{9}{c|}{$\\mathcal{N}(0,1)$ Errors} \\\\ \n')

 cat('&& \\multicolumn{4}{c|}{Limitless} &  \\multicolumn{4}{c|}{OLS} &\\makecell[c]{Local\\\\Linear}',
        ifelse(full,'\\multicolumn{4}{c|}{Limitless} &  \\multicolumn{4}{c|}{OLS} &\\makecell[c]{Local\\\\Linear}',''),'\\\\
 \\multicolumn{2}{r|}{\\makecell[r]{Polynomial\\\\Degree}}&1&2&3&4&1&2&3&4&',ifelse(full,'&1&2&3&4&1&2&3&4&n/a','n/a'),' \\\\
')
    for(rr in 1:nrow(tab)){
        if(rr==1) cat('\\hline\n\\hline\n\\multirow{',ifelse(full,4,2),'}{*}{',ifelse(full,'\\begin{sideways}Linear\\end{sideways}','Linear'),'}')
        if(rr==3) cat('\\hline\n\\hline\n\\multirow{',ifelse(full,4,2),'}{*}{',ifelse(full,'\\begin{sideways}Anti-Sym\\end{sideways}','Anti-Sym'),'}')
        if(rr==5) cat('\\hline\n\\hline\n\\multirow{',ifelse(full,4,2),'}{*}{',ifelse(full,'\\begin{sideways}One-Side\\end{sideways}','One-Side'),'}')
        cat('&',rownames(tab)[rr],'&')
        cat(paste(sprintf("%.1f", round(tab[rr,],1)),collapse='&'))
        cat('\\\\ \n')
    }
    cat('
 \\hline
\\end{tabular}
\\caption{',caption,'}
\\label{',label,'}
\\end{table}\n',sep='')

}

#' @export
polyLatex5 <- function(tab,full,caption='',label='tab:poly'){
  if(NCOL(tab)!=11) stop('This only works with polynomial degree=1,...,5')
  tab2 <- tab
  for(i in 1:nrow(tab)) for(j in 1:ncol(tab))
                          tab2[i,j] <- ifelse(tab[i,j]>10,
                            sprintf("%i",as.integer(round(tab[i,j]))),
                            sprintf("%.1f",round(tab[i,j],1)))

    cat('
        \\begin{table}[ht]
\\centering
\\begin{tabular}{ll|ccccc|ccccc|c',ifelse(full,'|llll|llll|l}','}'),'
  \\hline \n')
    if(full) cat('&&\\multicolumn{11}{c|}{$t_3$ Errors} &\\multicolumn{11}{c|}{$\\mathcal{N}(0,1)$ Errors} \\\\ \n')

 cat('&& \\multicolumn{5}{c|}{Limitless} &  \\multicolumn{5}{c|}{OLS} &Local',
        ifelse(full,'\\multicolumn{5}{c|}{Limitless} &  \\multicolumn{5}{c|}{OLS} &Local',''),'\\\\
 && \\multicolumn{5}{c|}{Polynomial Degree}&\\multicolumn{5}{c|}{Polynomial Degree}&Linear',
 ifelse(full,'\\multicolumn{5}{c|}{Polynomial Degree}&\\multicolumn{5}{c|}{Polynomial Degree}&Linear',''),'\\\\
 DGM&Measure&1&2&3&4&5&1&2&3&4&5&',ifelse(full,'&1&2&3&4&1&2&3&4&5&',''),' \\\\
')
    for(rr in 1:nrow(tab)){
        if(rr==1) cat('\\hline\n\\hline\n\\multirow{',ifelse(full,4,2),'}{*}{',ifelse(full,'\\begin{sideways}Linear\\end{sideways}','Linear'),'}',sep='')
        if(rr==3) cat('\\hline\n\\hline\n\\multirow{',ifelse(full,4,2),'}{*}{',ifelse(full,'\\begin{sideways}Anti-Sym\\end{sideways}','\\makecell[c]{Anti-\\\\Sym}'),'}',sep='')
        if(rr==5) cat('\\hline\n\\hline\n\\multirow{',ifelse(full,4,2),'}{*}{',ifelse(full,'\\begin{sideways}One-Side\\end{sideways}','Sine'),'}',sep='')
        cat('&',rownames(tab)[rr],'&')
        cat(paste(tab2[rr,],collapse='&'))
        cat('\\\\ \n')
    }
    cat('
 \\hline
\\end{tabular}
\\caption{',caption,'}
\\label{',label,'}
\\end{table}\n',sep='')

}


