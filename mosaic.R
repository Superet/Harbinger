#----------------------------------------------------------------
#
# begin color experiments:
#
#   mosaic plots require colors;
#   I sifted thru the 657 colors offered by "colors()" and
#   selected a few in two vectors:
#     pastell.colors    and    strong.colors
#   the former will be the default
#
# here is a tool for looking at a color vector, possibly a large one:
plot.colors <<- function(col.vec) {
  ncolors <- length(col.vec)
  windows(height=3, width=ncolors/5)
  par(mar=rep(0,4))
  plot(c(0,ncolors), c(-1,1), type="n")
  for(i in 1:ncolors) {
    rect(i-1,0,i,1,col=col.vec[i])
    text(i-0.5, -.2, i, srt=-90, adj=0)
  }
}

# gray: 253:153 (bright:dark)
#gray.colors <<- colors()[153:253]  # had to comment out; R complains
#plot.colors(gray.colors)

# strong colors:
strong.colors <<- colors()[c(70,76,91,97,119,134,254,461,653)]
#plot.colors(strong.colors)

# pastells: will be used as default below !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
pastell.colors <<- colors()[c(488,516,521,538,532,582,602,425,410,385)]
#plot.colors(pastell.colors)

# end color experiments
#
#----------------------------------------------------------------
#
# Begin mosaic plot functions:
#
#   You don't need to understand these two functions:
#     "recursive.divider" and "mosaic.plot".
#   Define them by sourcing this file in the R interpreter.
#   Make sure you have the vector "pastell.colors" defined from above,
#   which should be done if you source the whole file.
#
recursive.divider <<- function(arr, hor.vert, outer.rect, col.dim, col,
                               xmarg, ymarg, hor.depth=0, vert.depth=0, nolabel=F, cex=1)
{
  gap <- 0.01
  h.depth <- hor.depth;    v.depth <- vert.depth
  if(is.null(dim(arr))) {
    dims <- length(arr);  labels <- names(arr)
  } else {
    dims <- dim(arr);  labels <- dimnames(arr)[[1]]
  }
  ndims <- length(dims);  ngrps <- dims[1]
  if(is.null(ngrps)) { ngrps <- 1;  ndims <- 1 }
  mat <- matrix(arr, nrow=ngrps)  # no other way to handle variable numbers of dims...
  freq <- apply(mat, 1, sum);    freq.tot <- sum(freq)
  if(freq.tot==0) {
    points((outer.rect["left"] + outer.rect["right"])/2,
           (outer.rect["bottom"] + outer.rect["top"])/2,
           pch=1, cex=.5 )
    return()
  }
  division <- freq / freq.tot
  nrects.now <- length(division);    ngaps.now <- nrects.now + 1
  nrects <- 1;    ngaps <- 0
  if(hor.vert[1]=="h") {  h.depth <- h.depth + 1
    for(j in 1:ndims) if(hor.vert[j]=="h")  {
      mrects <- dims[j]
      ngaps  <- ngaps  + nrects * (mrects + 1)
      nrects <- nrects * mrects
    }
    division <- (outer.rect["right"] - outer.rect["left"] - ngaps*gap) *
      division + (ngaps-ngaps.now)/nrects.now*gap
    division <- outer.rect["left"] + cumsum(c(0,division) + gap)
  } else {  v.depth <- v.depth + 1
    for(j in 1:ndims) if(hor.vert[j]=="v")  {
      mrects <- dims[j]
      ngaps  <- ngaps  + nrects * (mrects + 1)
      nrects <- nrects * mrects
    }
    division <- (outer.rect["top"] - outer.rect["bottom"] - ngaps*gap) *
      division + (ngaps-ngaps.now)/nrects.now*gap
    division <- outer.rect["bottom"] + cumsum(c(0,division) + gap)
  }
  for(i in 1:ngrps) {
    lab.off <- 0.1
    if(hor.vert[1]=="h") {
      inner.rect <- c(left=division[i], outer.rect["bottom"],
                      right=division[i+1]-gap, outer.rect["top"])
    } else {
      inner.rect <- c(outer.rect["left"], bottom=division[i],
                      outer.rect["right"], top=division[i+1]-gap)
    }
    if(!is.null(xmarg) & hor.vert[1]=="h" & inner.rect["bottom"]<=gap*v.depth) {
      x <- (inner.rect["left"] + inner.rect["right"]) / 2
      y <- -xmarg * (sum(hor.vert=="h") - 1) - lab.off  # hor.vert keeps shrinking
      if(!nolabel) {
        text(x=x, y=y, label=labels[i], srt=-90, adj=0, cex=cex)
        lines(c(inner.rect["left"],x,inner.rect["right"]), y+lab.off*c(0.5,0.1,0.5))
      }
    }
    if(!is.null(ymarg) & hor.vert[1]=="v" & inner.rect["left"]<=gap*h.depth) {
      x <- -ymarg*(sum(hor.vert=="v") - 1) - lab.off    # hor.vert keeps shrinking
      y <- (inner.rect["bottom"] + inner.rect["top"]) / 2
      if(!nolabel) {
        text(x=x, y=y, label=labels[i], adj=1, cex=cex)
        lines(x+lab.off*c(0.5,0.1,0.5), c(inner.rect["bottom"],y,inner.rect["top"]))
      }
    }
    if(!is.null(col.dim)) if(col.dim==ndims)
      polygon(inner.rect[c(1,3,3,1)], inner.rect[c(2,2,4,4)], col=col[i], border=0)
    if(length(dim(arr)) <= 1)
      polygon(inner.rect[c(1,3,3,1)], inner.rect[c(2,2,4,4)])
    if(length(dim(arr)) > 1)
      recursive.divider(arr=array(mat[i,], dim=dim(arr)[-1],
                                           dimnames=as.list(dimnames(arr)[-1])),
                        hor.vert=hor.vert[-1], outer.rect=inner.rect,
                        col.dim=col.dim, col=col, xmarg, ymarg,
                        hor.depth=h.depth, vert.depth=v.depth, nolabel=nolabel, cex=cex)
  } # end for(i in 1:ngrps)...
} # end recursive.divider <- function(...
#
#----------------
#
mosaic.plot <<- function(arr,
                         dims=NULL,
                         hor.vert=c(rep("h",length(dims)-1),"v"),
                         col.dim=length(dims), col=pastell.colors,
                         xmarg=0.2, ymarg=0.2,
                         yn.newplot=T, nolabel=F, cex=1, border=F, border.width=1,
                         mar=rep(0,4), mgp=rep(0,3), print=T, round=3, main="", ...)
{
  if(!is.null(xmarg)) xmarg <- xmarg
  if(!is.null(ymarg)) ymarg <-ymarg
  if(is.data.frame(arr)) {
    local.arr <- table(arr)
  } else {
    local.arr <- as.array(arr)
  }
  if(is.null(dims)) dims <- if(!is.null(dim(local.arr))) 1:length(dim(local.arr)) else 1
  if(is.null(dimnames(local.arr))) {
    dimn <- list()
    for(i in 1:length(dim(local.arr)))
      dimn[[i]] <- paste("D",i,"/Grp",1:dim(local.arr)[i], sep="")
    names(dimn) <- paste("Dim",1:length(dim(local.arr)),sep="")
    dimnames(local.arr) <- dimn
  }
  if(length(hor.vert) != length(dims)) {
    cat("!!!!!Error in call to mosaic.plot:\n  length(hor.vert) < length(dims)\n")
    return(NULL) }
  if(is.character(dims)) {
    if(is.null(dimnames(local.arr))) {
      cat("!!!!!!Error in call to mosaic.plot:\n  array has no dimnames \n");
      return(NULL) }
    local.dims <- match(dims, names(dimnames(local.arr)))
  } else {
    local.dims <- dims
  }
  if(min(local.dims) < 1 | max(local.dims) > length(dim(local.arr))) {
    cat("!!!!!Error in call to mosaic.plot:\n  dims out of bound\n  dims=",dims,"\n");
    return(NULL); }
  if(any(is.na(local.dims))) {
    cat("!!!!!Error in call to mosaic.plot:\n  dimensions entered =",dims,
        "\n  actual dimensions  =",names(dimnames(local.arr)),"\n");  return(NULL) }
  if(!is.null(col.dim) & print)
    cat("Call to mosaic.plot:\n  dims entered =",dims,
        "\n  actual dims  =",names(dimnames(local.arr)),"\n")
  local.arr <- apply(local.arr, local.dims, sum)
  if(!is.null(col.dim)) col.dim <- length(dims) - col.dim + 1
  if(yn.newplot) {
    if(main != "" & mar[3]==0) mar[3] <- 3
    par(mar=mar, mgp=mgp)
    plot(0:1, 0:1,
         xlim=c(-ymarg*sum(hor.vert=="v"),1), c(-xmarg*sum(hor.vert=="h"),1),
         xaxt="n", yaxt="n", xlab="", ylab="", type="n", bty="n", main=main,...)
    if(border) lines(par()$usr[c(1,2,2,1,1)], par()$usr[c(3,3,4,4,3)], lwd=border.width)
  } else { lab.dist <- NULL }
  recursive.divider(local.arr, hor.vert, outer.rect=c(left=0,bottom=0,right=1,top=1),
                    col.dim=col.dim, col=col,
                    xmarg=xmarg, ymarg=ymarg,
                    nolabel=nolabel, cex=cex)
  seq.hor  <- seq(along=hor.vert)[hor.vert=="h"]
  seq.vert <- seq(along=hor.vert)[hor.vert=="v"]
  for(j in seq.vert) {
    perm <- seq(along=dim(local.arr));  perm[1] <- j;  perm[j] <- 1
    local.arr.perm <- aperm(local.arr, perm)
    local.mat <- matrix(local.arr.perm, nrow=dim(local.arr.perm)[1])
    local.mat <- local.mat[rev(1:nrow(local.mat)),]
    local.arr.perm <- array(local.mat, dim=dim(local.arr.perm), dimnames=dimnames(local.arr.perm))
    dimnames(local.arr.perm)[[1]] <- rev(dimnames(local.arr.perm)[[1]])
    local.arr <- aperm(local.arr.perm, perm)
  }
  if(print) {
    cat("----------------------------------------------------------------\n")
    print(ftable(addmargins(local.arr),
                 row.vars=seq.vert,
                 col.vars=seq.hor ) )
    cat("----------------------------------------------------------------\n")
    print(round(ftable(addmargins(prop.table(local.arr)),
                       row.vars=seq.vert,
                       col.vars=seq.hor ), round) )
    cat("----------------------------------------------------------------\n")
    print(round(ftable(addmargins(prop.table(local.arr, margin=seq.hor), margin=seq.vert),
                       row.vars=seq.vert,
                       col.vars=seq.hor ), round) )
    cat("----------------------------------------------------------------\n")
  }
}
#
#----------------
#
# This function scrambles the vertical variables, holding the horizontal variables fixed,
# and overplots the results of scrambling:
mosaic.plot.overallnull <<- function(dat, hor.vert=c(rep("h",dim(dat)[2]-1),"v"),
                                     nrand=0, ...)
{
  ## cat("Entering mosaic.plot.overallnull...\n")
  n <- dim(dat)[1]
  mosaic.plot(table(dat), ...)
  dat.null <- dat
  for(i in seq(length=nrand)) {
    dat.null[,hor.vert=="v"] <- dat[sample(1:n),hor.vert=="v"]
    mosaic.plot(table(dat.null), yn.newplot=F, nolabel=T, col.dim=NULL)
  }
  ## cat("Done.\n")
}
#
# Simplify life: This will be the master function.
mosaic <<- mosaic.plot.overallnull
#
#----------------
#
# Is the following Heike's code?  I believe so... (thank her).
#
mosaic.matrix <<- function(x,...) UseMethod("mosaic.matrix")

mosaic.matrix.data.frame <<- function (x, cex=2, cex.categ=cex*.75)
{
    p <- dim(x)[2]
    par(mfrow=c(p,p))
    for (i in 1:p)
      for(j in 1:p)
        if(i != j)
          {
            mosaic.plot(table(x[,j],x[,i]),nolabel=T, print=F)
          } else {
            plot(c(0,1), c(0,1), type="n", frame=F, axes=F, xlab="", ylab="")
            text(c(0.5), c(.6), labels=c(names(x)[i]), cex=cex)
            text(c(0.5), c(.4), labels=paste(names(table(x[,i])), collapse=", "), cex=cex.categ)
          }
}
mosaic.matrix.array <<- function (x, cex=2, cex.categ=cex*.75)
{
    p <- length(dim(x))
    par(mfrow=c(p,p))
    for (i in 1:p)
      for(j in 1:p)
        if(i != j)
          {
            mosaic.plot(margin.table(x, margin=c(i,j)), nolabel=T, print=F)
          } else {
            plot(c(0,1), c(0,1), type="n", frame=F, axes=F, xlab="", ylab="")
            text(c(0.5), c(.6), labels=names(dimnames(x))[i], cex=cex)
            text(c(0.5), c(.4), labels=paste(dimnames(x)[[i]], collapse=", "), cex=cex.categ)
          }
}
mosaic.matrix.table <<- mosaic.matrix.array

# Example:
#   mosaic.matrix(UCBAdmissions)
#
# end mosaic plot functions
#
#----------------------------------------------------------------
