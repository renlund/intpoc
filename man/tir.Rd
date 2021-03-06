% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tir.r
\name{tir}
\alias{tir}
\title{Time In Range for 'time-series' like series... sort of.}
\usage{
tir(x, y, y.int, x.int, group, y.NA, incl.low, par.test = TRUE, mess = FALSE)
}
\arguments{
\item{x}{numerical; x-coordinates of points of interest}

\item{y}{numerical; y-coordinates of points of interest}

\item{y.int}{numerical; the INR intervals of interest}

\item{x.int}{numerical, the time intervals of interest}

\item{group}{logical; if \code{TRUE} 'Group.mean' (no!) will be called to
handle the situation of non-unique x-values
NOTE; if TRUE this may cause some missing y-coordinates
to "disappear". E.g. if a fixed x-value occurs multiple times
(in the x-vector), then \code{group = TRUE} will replace these values with
the single unique x-value and the corresponding mean (non-\code{NA}) y-value}

\item{y.NA}{character; "allow"="a", "warn"="w" or "stop"="s". This tells
the function how to deal with missing y-coordinates. If
"stop", then no missing values are allowed, so the function stops.
If "allow" or "warn" then \code{rep_na(x,y)} is used to replace
the missing values with the linearly interpolated value or
the "last observation carried forward" if the missing value is
is at the end of the vector (and analagously if the missing value
is at the beginning. If "warn" then the function will also warn
that this has been done. ("allow" if missing.)}

\item{incl.low}{logical; should lower bound be inclusive (only when \code{length(y.int)==2})}

\item{par.test}{logical; should we test for sane parameter values?}

\item{mess}{logical; do you want messages? (default \code{FALSE})}
}
\value{
A \code{data.frame} with
\code{R.l} = lower part of the interval for which TiR is calculated
\code{R.h} = upper part of the interval for which TiR is calculated
\code{T.l} = lower part of the time interval for which TiR is calculated
\code{T.h} = upper part of the time interval for which TiR is calculated
\code{Time} = the amount of "time" available in (T.l,T.h)
\code{TiR} = Time in Range with respect to (R.l,R.h] and (T.l,T.h)
\code{percent} = TiR/Time
}
\description{
this function does the same as TiR but now we can partition the
x- AND y-axis into parts; calculating the TiR for each part of the
interpolated curve
}
