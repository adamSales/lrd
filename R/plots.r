polySim <- function(tp){

    ### the DGMs
    pLin <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))+
        stat_function(fun=function(x) 0.5*x)+xlim(-1,1)+ylim(-2,2)+
        labs(title='Linear',xlab='$R$',ylab='$Y$')+ geom_vline(xintercept=0,linetype = 2)
    pAS <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))+
        stat_function(fun=function(x) ifelse(abs(x)>0.5, 3*x+sign(x)*(0.5-3)*0.5,0.5*x))+xlim(-1,1)+ylim(-2,2)+
        labs(title='Anti-Symmetric',xlab='$R$',ylab='$Y$')+ geom_vline(xintercept=0,linetype = 2)
    pOS <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))+
        stat_function(fun=function(x) ifelse(x>0.5,3*x+(0.5-3)*0.5,0.5*x))+xlim(-1,1)+ylim(-2,2)+
        labs(title='One-Side',xlab='$R$',ylab='$Y$')+ geom_vline(xintercept=0,linetype = 2)
}

    dgms <- c('lin','antiSim','oneSide')
    errs <- c('t','norm')
    type <- c('AS','IK')
    deg <- 1:5


dgms <- function(tp){

    ### the DGMs
    curve(0.5*x,from=-1,to=1,ylim=c(-2,2),xlab='r',ylab='$\\EE[Y|R=r]$',main='Linear')
    abline(v=0,lty=2)
    curve(ifelse(abs(x)>0.5, 3*x+sign(x)*(0.5-3)*0.5,0.5*x),from=-1,to=1,ylim=c(-2,2),xlab='r',ylab='$\\EE[Y|R=r]$',main='Anti-Symmetric')
    abline(v=0,lty=2)
    curve(ifelse(x>0.5,3*x+(0.5-3)*0.5,0.5*x),from=-1,to=1,ylim=c(-2,2),xlab='r',ylab='$\\EE[Y|R=r]$',main='One-Sided')
    abline(v=0,lty=2)
}


resTab <- function(run){
    runPik <- run[seq(3,19,4),]
    runPsh <- run[seq(1,17,4),]
    runEstIK <- run[seq(4,20,4),]
    runEstSH <- run[seq(2,18,4),]

    ests <- rbind(runEstSH,runEstIK)
    tab <- rbind(
        level=apply(rbind(runPsh,runPik),1,function(x) mean(x<0.05,na.rm=TRUE)),
        bias=rowMeans(ests,na.rm=TRUE),
        sd=apply(ests,1,sd,na.rm=TRUE),
        rmse=apply(ests,1,function(x) sqrt(mean(x^2,na.rm=TRUE))))
    tab
}

msePlot <- function(run,ttl){
    tab <- resTab(run)

    plot(1:5,tab['rmse',6:10],xlab='Polynomial Degree',ylab='RMSE',col='blue',type='b',ylim=c(0,120))
    points(1:5,tab['rmse',1:5],col='red',type='b')
    if(!missing(ttl)) title(main=ttl)

    legend('topleft',legend=c('OLS','Limitless'),col=c('blue','red'),lty=1,pch=1)
}

totPlot <- function(totalPoly){
    par(mfrow=c(3,3))
    dgms()
    for(err in c('t','norm'))
        for(dgm in c('lin','antiSym','oneSide'))
            msePlot(totalPoly[[paste0(dgm,'_',err)]],paste(ifelse(err=='t','t(3)','Normal(0,1)'),'Errors'))
}

prntTab <- function(run,caption=''){
    tab <- resTab(run)
    addtorow <- list()
    addtorow$pos <- list(0, 0)
    addtorow$command <- c("& \\multicolumn{5}{c|}{Limitless} &  \\multicolumn{5}{c|}{OLS} \\\\\n",
                          paste(paste(c('degree',1:5,1:5),collapse='&'),'\\\\\n'))
    xtab <- xtable(tab,caption=caption)
    align(xtab) <- 'r|lllll|lllll|'
    print(xtab, add.to.row = addtorow, include.colnames = FALSE)
}

prntTab2 <- function(caption=''){
    tab <- NULL
    for(dgm in c('lin','antiSym','oneSide')){
        tab <- rbind(tab,cbind(resTab(totalPoly[[paste0(dgm,'_norm')]])[,-c(5,10)],
                     ikSum(ikp[[paste0(dgm,'_norm')]]),
                     resTab(totalPoly[[paste0(dgm,'_t')]])[,-c(5,10)],
                     ikSum(ikp[[paste0(dgm,'_t')]])))
    }

    cat('
        \\begin{table}[ht]
\\centering
\\begin{tabular}{cr|llll|llll|l|llll|llll|l|}
  \\hline
 &&\\multicolumn{9}{c|}{Normal(0,1) Errors} &\\multicolumn{9}{c|}{$t_3$ Errors} \\\\
 && \\multicolumn{4}{c|}{Limitless} &  \\multicolumn{4}{c|}{OLS} &\\makecell[c]{Local\\\\Linear}& \\multicolumn{4}{c|}{Limitless} &  \\multicolumn{4}{c|}{OLS} &\\makecell[c]{Local\\\\Linear}\\\\
 &degree&1&2&3&4&1&2&3&4&&1&2&3&4&1&2&3&4& \\\\
')
    for(rr in 1:nrow(tab)){
        if(rr==1) cat('\\hline\n\\hline\n\\multirow{4}{*}{\\begin{sideways}Linear\\end{sideways}}')
        if(rr==5) cat('\\hline\n\\hline\n\\multirow{4}{*}{\\begin{sideways}Anti-Sym\\end{sideways}}')
        if(rr==9) cat('\\hline\n\\hline\n\\multirow{4}{*}{\\begin{sideways}One-Side\\end{sideways}}')
        cat('&',rownames(tab)[rr],'&')
        cat(paste(sprintf("%.2f", round(tab[rr,],2)),collapse='&'))
        cat('\\\\ \n')
    }
    cat('
 \\hline
\\end{tabular}
\\caption{',caption,'}
\\end{table}\n')

}

ikSum <- function(run)
    c(level=mean(run[1,]<0.05,na.rm=TRUE),
      bias=mean(run[2,],na.rm=TRUE),
      sd=sd(run[2,],na.rm=TRUE),
      rmse=sqrt(mean(run[2,]^2,na.rm=TRUE)))

