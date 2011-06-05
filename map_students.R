#http://flowingdata.com/2011/05/11/how-to-map-connections-with-great-circles

students <- read.csv('/Users/aaron/projects/graduate_student_migrations/students_2005-2011.txt', header=TRUE, as.is=TRUE)
universities <- read.csv('/Users/aaron/projects/graduate_student_migrations/universities_2005-2011.txt', header=TRUE)

library(maps)
library(geosphere)

checkDateLine <- function(l){
  n<-0
  k<-length(l)
  k<-k-1
  for (j in 1:k){
    n[j] <- l[j+1] - l[j]
  }
  n <- abs(n)
  m<-max(n, rm.na=TRUE)
  ifelse(m > 30, TRUE, FALSE)
}
clean.Inter <- function(p1, p2, n, addStartEnd){
  inter <- gcIntermediate(p1, p2, n=n, addStartEnd=addStartEnd)
  if (checkDateLine(inter[,1])){
    m1 <- midPoint(p1, p2)
    m1[,1] <- (m1[,1]+180)%%360 - 180
    a1 <- antipode(m1)
    l1 <- gcIntermediate(p1, a1, n=n, addStartEnd=addStartEnd)
    l2 <- gcIntermediate(a1, p2, n=n, addStartEnd=addStartEnd)
    l3 <- rbind(l1, l2)
    l3
  }
  else{
    inter
  }
}

#map("world", col="#191919", fill=TRUE, bg="#736F6E", lwd=0.05)


add_lines <- function(){
  pal <- colorRampPalette(c("#00FF00", "#FF0000"))
  colors <- pal(100)

  fsub <- students[order(students$cnt),]
  maxcnt <- max(fsub$cnt)
  for (j in 1:length(fsub$undergrad)) {
    u1 <- universities[universities$uni_id == fsub[j,]$undergrad,]
    u2 <- universities[universities$uni_id == fsub[j,]$grad,]
    p1 <- c(u1[1,]$long, u1[1,]$lat)
    p2 <- c(u2[1,]$long, u2[1,]$lat)
    inter <- clean.Inter(p1,p2,n=100, addStartEnd=TRUE)
    colindex <- round( (fsub[j,]$cnt / maxcnt) *length(colors))
    lines(inter, col=colors[colindex], lwd=0.6)
  }
}

map_usa <- function(){
  xlim <- c(-171.738281, -56.601563)
  ylim <- c(12.039321, 71.856229)
  map("world", col="#191919", fill=TRUE, bg="#736F6E", lwd=0.05, xlim=xlim, ylim=ylim)
  add_lines()
}

map_world <- function(){
  map("world", col="#191919", fill=TRUE, bg="#736F6E", lwd=0.05)
  add_lines()
}
map_usa()