% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrawl_build.R
\name{scrawl_build}
\alias{scrawl_build}
\title{Construct the data used for a scrawl plot}
\usage{
scrawl_build(seed = 1, n_paths = 1000, n_steps = 50, sz_step = 50, sz_slip = 5)
}
\arguments{
\item{seed}{Integer valued seed to pass to set.seed()}

\item{n_paths}{Number of paths to trace}

\item{n_steps}{Number of steps along each path}

\item{sz_step}{Size of each step in the x and y dimensions}

\item{sz_slip}{Size of each step in the z dimension}
}
\value{
A tibble
}
\description{
Construct the data used for a scrawl plot
}
