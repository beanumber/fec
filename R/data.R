#' Congressional election results
#'
#' @description Official results from congressional elections
#' @docType data
#' @format For both, a data frame (\code{\link[dplyr]{tbl_df}}) with one row for each
#' candidate and 11 variables.
#' 
#' \describe{
#'   \item{fec_id}{FEC ID for the candidate}
#'   \item{state}{Two-character state abbreviation}
#'   \item{district}{District number within the state}
#'   \item{incumbent}{Logical indicating whether the candidate was an incumbent}
#'   \item{candidate_name}{The candidate's name}
#'   \item{party}{One-character abbreviation for the candidate's political party}
#'   \item{primary_votes}{Number of votes earned in the primary election}
#'   \item{runoff_votes}{Number of votes earned in a runoff election}
#'   \item{general_votes}{Number of votes earned in the general election}
#'   \item{ge_winner}{Did the candidate win the general election?}
#'   \item{election_cycle}{Which federal election cycle was it?}
#' }
#' @source \url{https://transition.fec.gov/pubrec/electionresults.shtml} 
"house_elections_2012"

#' FEC variables
#' @docType data
"fec_vars"