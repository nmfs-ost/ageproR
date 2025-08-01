% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/options_reference_points.R
\name{reference_points}
\alias{reference_points}
\title{Values to output optional agepro model's reference point threshold report.}
\description{
Class Structure that defines biomass thresholds for spawaning biomass
\eqn{(B_{S,\ THRESHOLD})}, mean biomass \eqn{(\bar B_{S,\ THRESHOLD})},
and total stock biomass\eqn{(B_{T,\ THRESHOLD})}, including the threshold
rate for fishing mortality \eqn{(F_{THRESHOLD})}.

The logical flag \code{enable_reference_points} allows the user to
set values to this class fields. The flag will also notify \code{agepro_model}
if this keyword parameter is allowed to be written to input file.

If this class is initialized with default values, it is presumed that this
keyword parameter is not used in the agepro_model. Therefore,
\code{enable_reference_points} is flagged as FALSE. Valid non-default values will
set this flag to TRUE.
}
\details{
The reference_points class (\code{REFPOINTS}) is recognized as a keyword
parameter used in the AGEPRO input file format, but it is optional.
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{ssb_threshold}}{Threshold of spawning biomass}

\item{\code{stock_biomass_threshold}}{Threshold of total stock biomass}

\item{\code{mean_biomass_threshold}}{Threshold for mean biomass}

\item{\code{fishing_mortality_threshold}}{Threshold for the fishing mortality rate for annual total fishing
mortality calculated across all fleets.}

\item{\code{json_list_object}}{Returns JSON list object of containing REFPOINT values}

\item{\code{enable_reference_points}}{Logical field that flags if fields can be edited. This class will not
accept new values to its fields or allow it to be exported to input file
until this option flag is TRUE.}

\item{\code{keyword_name}}{AGEPRO keyword parameter name}

\item{\code{inp_keyword}}{Returns AGEPRO input-file formatted Parameter}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-reference_points-new}{\code{reference_points$new()}}
\item \href{#method-reference_points-print}{\code{reference_points$print()}}
\item \href{#method-reference_points-read_inp_lines}{\code{reference_points$read_inp_lines()}}
\item \href{#method-reference_points-get_inp_lines}{\code{reference_points$get_inp_lines()}}
\item \href{#method-reference_points-clone}{\code{reference_points$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-reference_points-new"></a>}}
\if{latex}{\out{\hypertarget{method-reference_points-new}{}}}
\subsection{Method \code{new()}}{
Initializes the class
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{reference_points$new(
  ssb_threshold = 0,
  stockbio_threshold = 0,
  meanbio_threshold = 0,
  fmort_threshold = 0,
  refpoint_flag = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ssb_threshold}}{Spawning Biomass threshold express in biomass output units (MT).}

\item{\code{stockbio_threshold}}{Stock biomass threshold expressed in biomass output units(MT).}

\item{\code{meanbio_threshold}}{Mean biomass threshold expressed in biomass output units (MT)}

\item{\code{fmort_threshold}}{Fishing mortality threshold}

\item{\code{refpoint_flag}}{R6class containing option flags to allow reference points to be used}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-reference_points-print"></a>}}
\if{latex}{\out{\hypertarget{method-reference_points-print}{}}}
\subsection{Method \code{print()}}{
Formatted to print out reference_points values
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{reference_points$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-reference_points-read_inp_lines"></a>}}
\if{latex}{\out{\hypertarget{method-reference_points-read_inp_lines}{}}}
\subsection{Method \code{read_inp_lines()}}{
Reads in the values from the keyword parameter REFPOINT from the
AGEPRO Input file

Note: enable_reference_points must be set to TRUE.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{reference_points$read_inp_lines(inp_con, nline)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{inp_con}}{Open file connection to AGEPRO Input File.}

\item{\code{nline}}{\cr
Location of the current line number being read when the file connection to
the AGEPRO input file is open.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-reference_points-get_inp_lines"></a>}}
\if{latex}{\out{\hypertarget{method-reference_points-get_inp_lines}{}}}
\subsection{Method \code{get_inp_lines()}}{
Returns values from the class to the REFOPINTS AGEPRO keyword parameter
formatted as AGEPRO input file lines.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{reference_points$get_inp_lines(delimiter = " ")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{delimiter}}{Character string delimiter separating values of AGEPRO input file line.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-reference_points-clone"></a>}}
\if{latex}{\out{\hypertarget{method-reference_points-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{reference_points$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
