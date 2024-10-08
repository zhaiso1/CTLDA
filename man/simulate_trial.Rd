% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_trial.R
\name{simulate_trial}
\alias{simulate_trial}
\title{simulate_trial}
\usage{
simulate_trial(
  n_arm = 200,
  M = c(0, 3, 6, 9, 12),
  mean_pbo,
  mean_act,
  cov,
  sd,
  adjust = TRUE
)
}
\arguments{
\item{n_arm}{a numerical value of sample size per arm}

\item{M}{a numerical vector of visit points}

\item{mean_pbo}{a numerical vector of mean responses at visit points in placebo arm}

\item{mean_act}{a numerical vector of mean responses at visit points in active arm}

\item{cov}{a matrix of variance-covariance matrix}

\item{sd}{a numerical value of standard deviation}

\item{adjust}{an indicator of whether or not to adjust mean responses}
}
\value{
a data frame of simulated longitudinal data
}
\description{
Simulate trial
}
\references{
Zhai, S., Shen, J. and Mehrotra, D.V. (2016). CTLDA: A Combined Test for Longitudinal Data Analysis in Randomized Clinical Trials.
}
\author{
Song Zhai
}
