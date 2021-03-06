% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fishing.R
\name{fishing}
\alias{fishing}
\title{Go fish!}
\usage{
fishing(fish, quota, ages = 4:50, distance, fish_licenses, mpa)
}
\arguments{
\item{fish}{a matrix of fish numbers by site (rows) and by age (columns). Can easily be generated by \code{initpop}}

\item{quota}{fishable quota, can be estimated using \code{link{estimatequota}}}

\item{ages}{age of catchable fish}

\item{distance}{matrix of distance from each cell to each \code{fish_communities}}

\item{fish_licenses}{number of licenses per region in \code{fish_communities}}

\item{mpa}{Logical vector, TRUE if closed to fishing}
}
\description{
Creates a list of \code{fish} that were caught for each community
}
\examples{
# create a fish matrix
fish <- initpop(initial_abun=250*10^6,cells=length(BESTMPA_domain),maxage=50,rate=0.7)
# calculate distances from shore
distance <- gDistance(spTransform(fish_communities,proj),BESTMPA_domain,byid = T)
# number of licenses per region in fish_communities
fish_licenses <- c(866, 4714, 3002, 879, 963)
# define MPAs
mpa <- sample(c(TRUE,FALSE),length(BESTMPA_domain),replace=TRUE)
# quota
quota <- estimatequota(fish,maxage=20,y=2001,tot_time=2001:2071,FMSY=0.28,FMSY_buffer=0.667)
# go fishing!
fishcatch <- fishing(fish,quota,ages=4:50,distance,fish_licenses,mpa)
}

