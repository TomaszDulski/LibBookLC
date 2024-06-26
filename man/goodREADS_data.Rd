% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Data_format_for_GoodReads.R
\name{goodREADS_data}
\alias{goodREADS_data}
\title{goodREADS_data}
\usage{
goodREADS_data(GD_df, book_data)
}
\arguments{
\item{GD_df}{input generated from GR_df_matrix function}

\item{book_data}{input generated from pre_format_GR function}
}
\value{
ready Table for Good Reads Library
}
\description{
goodREADS_data
}
\examples{
ready_table <- goodREADS_data(GD_df = GR_df_empty, book_data = pre_format_book_df)
}
