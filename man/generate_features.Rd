% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_features.R
\name{generate_features}
\alias{generate_features}
\title{generate_features}
\usage{
generate_features(df, sched = nfl_query("select * from nflschedule"),
  season = 2015, pos, def_start, num_start, byes)
}
\arguments{
\item{df}{dataframe containing names / teams of players to make
predictions for}

\item{sched}{dataframe containing nfl schedule data}

\item{season}{current season}

\item{pos}{position to make predictions for}

\item{def_start}{column containing first stat column for defense}

\item{num_start}{column containing first stat for player}

\item{byes}{teams on bye}
}
\value{
returns list of dataframes containing one, two, three week based
features
}
\description{
creates features needed to make predictions
}

