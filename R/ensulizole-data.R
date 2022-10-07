#' Biomarker Responses of the Blue Mussels to Organic UV Filters
#'
#' \code{sokolova2021} contains the biomarker responses of the blue mussel
#' \emph{Mytilus edulis} to organic UV filters ensulizole and octocrylene.
#' \code{ecorelevance} contains the ecological relevance of the biomarkers.
#'
#' @encoding UTF-8
#'
#' @format \code{sokolova2021} is a data frame with 30 rows and 31 variables:
#' \describe{
#' \item{Exposure}{5 experimental groups:
#' \describe{
#' \item{CTRL}{control group}
#' \item{EN10}{10 ug/L of ensulizole}
#' \item{EN100}{100 ug/L of ensulizole}
#' \item{OC10}{10 ug/L of octocrylene}
#' \item{OC100}{100 ug/L of octocrylene}
#' }
#' }
#' \item{NRR}{lysosomal membrane stability}
#' \item{ROS}{reactive oxygen species generation}
#' \item{TBARSd}{lipid peroxidation in digestive gland}
#' \item{TBARSg}{lipid peroxidation in gills}
#' \item{PCd}{protein carbonylation in digestive gland}
#' \item{PCg}{protein carbonylation in gills}
#' \item{CPRd}{NADPH–P450 reductase activity in digestive gland}
#' \item{CPRg}{NADPH–P450 reductase activity in gills}
#' \item{ERODd}{7-ethoxyresorufin-O-deethylase activity in digestive gland}
#' \item{ERODg}{7-ethoxyresorufin-O-deethylase activity in gills}
#' \item{CEd}{carboxylesterase activity in digestive gland}
#' \item{CEg}{carboxylesterase activity in gills}
#' \item{GSTd}{glutathione-S-transferase activity in digestive gland}
#' \item{GSTg}{glutathione-S-transferase activity in gills}
#' \item{GRd}{glutathione reductase activity in digestive gland}
#' \item{GRg}{glutathione reductase activity in gills}
#' \item{CTSDTd}{total cathepsin D activity in digestive gland}
#' \item{CTSDTg}{total cathepsin D activity in gills}
#' \item{CTSDFd}{free cathepsin D activity in digestive gland}
#' \item{CTSDFg}{free cathepsin D activity in gills}
#' \item{Cas2}{caspase 2}
#' \item{Cas3}{caspase 3}
#' \item{BAX}{Bcl-2-associated X protein}
#' \item{Bcl-2}{B-cell lymphoma 2}
#' \item{p53}{tumor protein 53}
#' \item{GADD45}{growth arrest and DNA-damage-inducible protein 45}
#' \item{NF-kB}{nuclear factor kB}
#' \item{IL-17}{interleukin 17}
#' \item{COX-2}{cyclooxygenase 2}
#' \item{ACC}{acetyl-CoA carboxylase}
#' }
#' @source Sokolova, I. M., Falfushynska, H., & Sokolov, E. P. (2021). Biomarker
#'   responses of the blue mussels to organic UV filters [Data set]. Zenodo.
#'   \doi{10.5281/zenodo.5176087}.
"sokolova2021"

#' @rdname sokolova2021
#'
#' @encoding UTF-8
#'
#' @format \code{ecorelevance} is a data frame with 30 rows and 2 variables:
#' \describe{
#' \item{Biomarker}{30 endpoints as documented in \code{sokolova2021}
#' }
#' \item{Eco}{ecological relevance}
#' }
"ecorelevance"
