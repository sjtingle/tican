.onAttach <- function(libname, pkgname) {
  msg <- paste(
    "Please cite tican as:",
    "Tingle SJ, Connelly C, Glover EK, et al. Contrast-Enhanced Ultrasound to Assess Kidney Quality During Ex Situ Normothermic Machine Perfusion. Transpl Int. 2025 Apr 2;38:14268.",
    "https://doi.org/10.3389/ti.2025.14268",
    sep = "\n"
  )
  packageStartupMessage(msg)
}
