dgms <- function(tp){

    ### the DGMs
    curve(0.5*x,from=-1,to=1,ylim=c(-2,2),xlab='r',ylab='$\\EE[Y|R=r]$',main='Linear')
    abline(v=0,lty=2)
    curve(ifelse(abs(x)>0.5, 3*x+sign(x)*(0.5-3)*0.5,0.5*x),from=-1,to=1,ylim=c(-2,2),xlab='r',ylab='$\\EE[Y|R=r]$',main='Anti-Symmetric')
    abline(v=0,lty=2)
    curve(ifelse(x>0.5,3*x+(0.5-3)*0.5,0.5*x),from=-1,to=1,ylim=c(-2,2),xlab='r',ylab='$\\EE[Y|R=r]$',main='One-Sided')
    abline(v=0,lty=2)
}


resTab <- function(run,full=FALSE){
    runPik <- run[seq(3,19,4),]
    runPsh <- run[seq(1,17,4),]
    runEstIK <- run[seq(4,20,4),]
    runEstSH <- run[seq(2,18,4),]

    ests <- rbind(runEstSH,runEstIK)
    tab <- rbind(
        level=apply(rbind(runPsh,runPik),1,function(x) mean(x<0.05,na.rm=TRUE)),
        RMSE=apply(ests,1,function(x) sqrt(mean(x^2,na.rm=TRUE))))
    if(full) tab <- rbind(tab,
                          bias=rowMeans(ests,na.rm=TRUE),
                          sd=apply(ests,1,sd,na.rm=TRUE))
    tab
}

#' @export
prntTab <- function(totalPoly,ikp,caption='',label='tab:poly',full=TRUE,md=FALSE){
    tab <- NULL
    for(dgm in c('lin','antiSym','oneSide')){
        tab <- rbind(tab,
                     cbind(resTab(totalPoly[[paste0(dgm,'_t')]],full=full)[,-c(5,10)],
                     ikSum(ikp[[paste0(dgm,'_t')]],full=full)))
        if(full)
            tab <- rbind(cbind(resTab(totalPoly[[paste0(dgm,'_norm')]],full=full)[,-c(5,10)],
                     ikSum(ikp[[paste0(dgm,'_norm')]],full=full)),tab)
    }
    if(md){
        colnames(tab) <- c(paste('Rob, deg=',1:4),paste('OLS, deg=',1:4),'Loc.Lin')
        rownames(tab) <- paste(rep(c('lin','antiSym','oneSide'),each=nrow(tab)/6,times=2),
                              rep(c('t err','norm err'),each=nrow(tab)/2),rownames(tab))
        return(tab)
    }
    cat('
        \\begin{table}[ht]
\\centering
\\begin{tabular}{cr|llll|llll|l|',ifelse(full,'llll|llll|l|}','}'),'
  \\hline \n')
    if(full) cat('&&\\multicolumn{9}{c|}{$t_3$ Errors} &\\multicolumn{9}{c|}{$\\mathcal{N}(0,1)$ Errors} \\\\ \n')

 cat('&& \\multicolumn{4}{c|}{Limitless} &  \\multicolumn{4}{c|}{OLS} &\\makecell[c]{Local\\\\Linear}',
        ifelse(full,'\\multicolumn{4}{c|}{Limitless} &  \\multicolumn{4}{c|}{OLS} &\\makecell[c]{Local\\\\Linear}',''),'\\\\
 &degree&1&2&3&4&1&2&3&4&',ifelse(full,'&1&2&3&4&1&2&3&4&',''),' \\\\
')
    for(rr in 1:nrow(tab)){
        if(rr==1) cat('\\hline\n\\hline\n\\multirow{',ifelse(full,4,2),'}{*}{',ifelse(full,'\\begin{sideways}Linear\\end{sideways}','Linear'),'}')
        if(rr==3) cat('\\hline\n\\hline\n\\multirow{',ifelse(full,4,2),'}{*}{',ifelse(full,'\\begin{sideways}Anti-Sym\\end{sideways}','Anti-Sym'),'}')
        if(rr==5) cat('\\hline\n\\hline\n\\multirow{',ifelse(full,4,2),'}{*}{',ifelse(full,'\\begin{sideways}One-Side\\end{sideways}','One-Side'),'}')
        cat('&',rownames(tab)[rr],'&')
        cat(paste(sprintf("%.2f", round(tab[rr,],2)),collapse='&'))
        cat('\\\\ \n')
    }
    cat('
 \\hline
\\end{tabular}
\\caption{',caption,'}
\\label{',label,'}
\\end{table}\n',sep='')

}


ikSum <- function(run,full=TRUE)
    if(full) return(c(level=mean(run[1,]<0.05,na.rm=TRUE),
                      RMSE=sqrt(mean(run[2,]^2,na.rm=TRUE)),
                      bias=mean(run[2,],na.rm=TRUE),
                      sd=sd(run[2,],na.rm=TRUE))) else
return(c(level=mean(run[1,]<0.05,na.rm=TRUE),
         RMSE=sqrt(mean(run[2,]^2,na.rm=TRUE))))

##### functions to summarize outcome simulation
###

toMat <- function(run){
    if(is.matrix(run) | is.data.frame(run)) return(run)
    ncomp <- sapply(run,length)
    if(all(ncomp==27)) return(do.call('rbind',run))
    runFull <- run[ncomp==27]
    examp <- runFull[[1]]
    names(examp)[-c(1:9)] <- gsub('sh','ik',names(examp)[-c(1:9)])
    for(aaa in run[ncomp<27]){
        new <- NULL
        names(aaa)[-c(1:9)] <- gsub('sh','ik',names(aaa)[-c(1:9)])
        for(i in 1:length(examp))
            new[i] <- aaa[names(examp)[i]]
        runFull[[length(runFull)+1]] <- new
    }
    do.call('rbind',runFull)
}


toMat <- function(run){
    if(is.matrix(run) | is.data.frame(run)) return(t(run))
    ncomp <- sapply(run,length)
    run <- run[ncomp==12]
    do.call('rbind',run)
}

#' @export
levels <- function(os){
    tab <- NULL
    for(n in c(50,250,2500))
        for(err in c('norm','t')){
            run <- toMat(os[[paste0(n,'_0_',err)]])
            #colnames(run)[-c(1:9)] <- gsub('sh','ik',colnames(run)[-c(1:9)])
            row <- NULL
            for(meth in c('cft','sh','ik'))
                for(bw in c('25','5')){
                    col <- pmatch(paste0(meth,bw,'.p'),colnames(run))
                    row <- c(row,mean(run[,col]<0.05,na.rm=TRUE))
                    names(row)[length(row)] <- paste(meth,bw)
                }
            tab <- rbind(tab,row)
            rownames(tab)[nrow(tab)] <- paste(err,n)
        }

    tab
}

#' @export
power <- function(os){
    tab <- NULL
    for(n in c(50,250,2500))
        for(err in c('norm','t')){
            run <- toMat(os[[paste0(n,'_0.2_',err)]])
            #colnames(run)[-c(1:9)] <- gsub('sh','ik',colnames(run)[-c(1:9)])
            row <- NULL
            for(meth in c('cft','sh','ik'))
                for(bw in c('25','5')){
                    col <- pmatch(paste0(meth,bw,'.p'),colnames(run))
                    row <- c(row,mean(run[,col]<0.05,na.rm=TRUE))
                    names(row)[length(row)] <- paste(meth,bw)
                }
            tab <- rbind(tab,row)
            rownames(tab)[nrow(tab)] <- paste(err,n)
        }

    tab
}

