\name{riceProdPhil}
\docType{data}
\alias{riceProdPhil}

\title{Rice Production in the Philippines}

\description{
   The \code{riceProdPhil} data frame contains annual data collected from
   43 smallholder rice producers in the Tarlac region of the Philippines
   between 1990 and 1997.
}

\usage{data( riceProdPhil )}

\format{
   This data frame contains the following vaiables (columns):
   \describe{
      \item{YEARDUM}{Time period (1= 1990, ..., 8 = 1997).}
      \item{FMERCODE}{Farmer code (1, ..., 43).}
      \item{PROD}{Output (tonnes of freshly threshed rice).}
      \item{AREA}{Area planted (hectares).}
      \item{LABOR}{Labour used (man-days of family and hired labour).}
      \item{NPK}{Fertiliser used (kg of active ingredients).}
      \item{OTHER}{Other inputs used (Laspeyres index = 100 for Firm 17 in 1991).}
      \item{PRICE}{Output price (pesos per kg).}
      \item{AREAP}{Rental price of land (pesos per hectare).}
      \item{LABORP}{Labour price (pesos per hired man-day.}
      \item{NPKP}{Fertiliser price (pesos per kg of active ingredient).}
      \item{OTHERP}{Price of other inputs (implicit price index).}
      \item{AGE}{Age of the household head (years).}
      \item{EDYRS}{Education of the household head (years).}
      \item{HHSIZE}{Household size.}
      \item{NADULT}{Number of adults in the household.}
      \item{BANRAT}{Percentage of area classified as bantog (upland) fields.}
   }
}

\details{
   This data set is published as supplement to Coelli et al. (2005).
   While most variables of this data set were supplied
   by the International Rice Research Institute (IRRI),
   some were calculated by Coelli et al. (2005, see p. 325--326).
   The survey is described in Pandey et al. (1999).
}

\source{
Supplementary files for Coelli et al. (2005),
\url{http://www.uq.edu.au/economics/cepa/software/CROB2005.zip}
}

\references{
Coelli, T. J., Rao, D. S. P., O'Donnell, C. J., and Battese, G. E. (2005)
\emph{An Introduction to Efficiency and Productivity Analysis},
Springer, New York.

Pandey, S., Masciat, P., Velasco, L, and Villano, R. (1999)
Risk analysis of a rainfed rice production system system in Tarlac,
Central Luzon, Philippines,
\emph{Experimental Agriculture}, \bold{35}, 225-237.
}

\keyword{datasets}
