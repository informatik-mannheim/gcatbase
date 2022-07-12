#' Amino acid properties.
#'
#' A dataset containing several biochemical properties of amino acids.
#'
#' @format A data frame with 20 rows and 6 variables:
#' \describe{
#'   \item{AA_1 }{Amino acid in one letter representation}
#'   \item{AA_3}{Amino acid in three letter representation}
#'   \item{Polar}{Woese et al.'s (1966) polar requirement is the slope of the line that results when $$\\log(1 - RF)/RF$$ for free amino acids is plotted against the log mole fraction of water in pyridine solvent.}
#'   \item{Hydropathy}{Kyte and Doolittle's (1982) hydropathy is based on water-vapor transfer free energies, the interior-exterior distribution of amino acid side-chains, and the subjective judgment of the authors}
#'   \item{Volume}{Grantham's (1974) molecular volume of side chains is the residue volume minus a constant peptide volume.}
#'   \item{Isoelectric}{The values of isoelectric points were taken from Alff-Steinberger (1969).}
#' }
"aa_prop"