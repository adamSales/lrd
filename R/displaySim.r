#' @export
dgms <- function(tp){

    ### the DGMs
    curve(0.5*x,from=-1,to=1,ylim=c(-2,2),xlab='r',ylab='$\\EE[Y|R=r]$',main='Linear')
    abline(v=0,lty=2)
    curve(ifelse(abs(x)>0.5, 3*x+sign(x)*(0.5-3)*0.5,0.5*x),from=-1,to=1,ylim=c(-2,2),xlab='r',ylab='$\\EE[Y|R=r]$',main='Anti-Symmetric')
    abline(v=0,lty=2)
  #curve(ifelse(x>0.5,3*x+(0.5-3)*0.5,0.5*x),from=-1,to=1,ylim=c(-2,2),xlab='r',ylab='$\\EE[Y|R=r]$',main='One-Sided')
  curve(mu4,from=-1,to=1,ylim=c(-2,2),xlab='r',ylab='$\\EE[Y|R=r]$',main='One-Sided')
    abline(v=0,lty=2)
}

dgms <- function(){
  lin <- function(x) 0.5*x
  as <- function(x) ifelse(abs(x)>0.5,3*x+sign(x)*(0.5-3)*0.5,lin(x))
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
 \\multicolumn{2}{r|}{Polynomial Degree}&1&2&3&4&1&2&3&4&',ifelse(full,'&1&2&3&4&1&2&3&4&n/a','n/a'),' \\\\
')
    for(rr in 1:nrow(tab)){
        if(rr==1) cat('\\hline\n\\hline\n\\multirow{',ifelse(full,4,2),'}{*}{',ifelse(full,'\\begin{sideways}Linear\\end{sideways}','Linear'),'}')
        if(rr==3) cat('\\hline\n\\hline\n\\multirow{',ifelse(full,4,2),'}{*}{',ifelse(full,'\\begin{sideways}Anti-Sym\\end{sideways}','Anti-Sym'),'}')
        if(rr==5) cat('\\hline\n\\hline\n\\multirow{',ifelse(full,4,2),'}{*}{',ifelse(full,'\\begin{sideways}One-Side\\end{sideways}','One-Side'),'}')
        cat('&',rownames(tab)[rr],'&')
        cat(paste(sprintf("%.2f", round(tab[rr,],1)),collapse='&'))
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
    cat('
        \\begin{table}[ht]
\\centering
\\begin{tabular}{cr|lllll|lllll|l',ifelse(full,'|llll|llll|l}','}'),'
  \\hline \n')
    if(full) cat('&&\\multicolumn{11}{c|}{$t_3$ Errors} &\\multicolumn{11}{c|}{$\\mathcal{N}(0,1)$ Errors} \\\\ \n')

 cat('&& \\multicolumn{5}{c|}{Limitless} &  \\multicolumn{5}{c|}{OLS} &\\makecell[c]{Local\\\\Linear}',
        ifelse(full,'\\multicolumn{5}{c|}{Limitless} &  \\multicolumn{5}{c|}{OLS} &\\makecell[c]{Local\\\\Linear}',''),'\\\\
 \\multicolumn{2}{r|}{\\makecell[r]{Polynomial\\\\Degree}}&1&2&3&4&5&1&2&3&4&5&',ifelse(full,'&1&2&3&4&1&2&3&4&5&n/a','n/a'),' \\\\
')
    for(rr in 1:nrow(tab)){
        if(rr==1) cat('\\hline\n\\hline\n\\multirow{',ifelse(full,4,2),'}{*}{',ifelse(full,'\\begin{sideways}Linear\\end{sideways}','Linear'),'}')
        if(rr==3) cat('\\hline\n\\hline\n\\multirow{',ifelse(full,4,2),'}{*}{',ifelse(full,'\\begin{sideways}Anti-Sym\\end{sideways}','Anti-Sym'),'}')
        if(rr==5) cat('\\hline\n\\hline\n\\multirow{',ifelse(full,4,2),'}{*}{',ifelse(full,'\\begin{sideways}One-Side\\end{sideways}','Sine'),'}')
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


##### functions to summarize outcome simulation
###


toMat <- function(run){
    if(is.matrix(run) | is.data.frame(run)) return(t(run))
    ncomp <- sapply(run,length)
    run <- run[ncomp==12]
    do.call('rbind',run)
}

#' @export
simlevels <- function(os){
    tab <- NULL
    for(n in c(50,250,2500))
        for(err in c('norm','t')){
            run <- toMat(os[[paste0(n,'_0_',err)]])
            #colnames(run)[-c(1:9)] <- gsub('sh','ik',colnames(run)[-c(1:9)])
            row <- NULL
            for(meth in c('cft','sh','ik')){
                col <- pmatch(paste0(meth,'.p'),colnames(run))
                row <- c(row,mean(run[,col]<0.05,na.rm=TRUE))
                names(row)[length(row)] <- meth
            }
            tab <- rbind(tab,row)
            rownames(tab)[nrow(tab)] <- paste(err,n)
        }

    tab
}

#' @export
simpower <- function(os){
    tab <- NULL
    for(n in c(50,250,2500))
        for(err in c('norm','t')){
            run <- toMat(os[[paste0(n,'_0.2_',err)]])
            #colnames(run)[-c(1:9)] <- gsub('sh','ik',colnames(run)[-c(1:9)])
            row <- NULL
            for(meth in c('cft','sh','ik')){
                col <- pmatch(paste0(meth,'.p'),colnames(run))
                row <- c(row,mean(run[,col]<0.05,na.rm=TRUE))
                    names(row)[length(row)] <- meth
                }
            tab <- rbind(tab,row)
            rownames(tab)[nrow(tab)] <- paste(err,n)
        }

    tab
}

#' @export
prntOutcomeSim <- function(levTab,powTab,caption,label){
cat('
\\begin{table}
\\footnotesize
\\begin{tabular}{cc|ccccccc}
\\hline

&&& \\multicolumn{ 2 }{c}{Permutation}&\\multicolumn{ 2 }{c}{\`\`Limitless\'\'}&\\multicolumn{ 2 }{c}{Local OLS}\\\\
$n$& Error &&', paste(rep('Level&Power',ncol(levTab)),collapse='&'),'\\\\
\\hline \n')
for(i in 1:nrow(levTab)){
    spec <- strsplit(rownames(levTab)[i],' ')[[1]]
    if(spec[1]=='norm'){
        cat('\\hline \n')
        cat('\\multirow{2}{*}{',round(as.numeric(spec[2])),'} & $\\mathcal{N}(0,1)$ &&')
    } else cat(' & $t_3$ &&')
    cat(paste(paste(round(levTab[i,]*100),round(powTab[i,]*100),sep='&'),collapse='&'),'\\\\ \n')
}
cat('\\hline
\\end{tabular}
\\caption{',caption,'}
\\label{',label,'}',sep='')
cat('\\end{table}\n')

}
