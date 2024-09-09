# Process simulation yaml parameters

process_sim_yaml <- function(yaml.params){
  sim_args <- yaml.params
  rem_names <- c("author", "path_analysis", "save_output")
  sim_args <- sim_args[!(names(sim_args) %in% rem_names)]
  new_names <- sapply(names(sim_args), 
                      function(.v){
                        if ( .v != "class_label" ) {
                          if ( length ( grep(.v, pattern = "percent", value = FALSE) ) > 0 ) {
                            paste0("pct.", strsplit(.v, split = "_")[[1]][2])
                          } else if ( length ( grep(.v, pattern = "rand", value = FALSE ) ) > 0 ){
                            strsplit(.v, split = "_")[[1]][2]
                          } else {
                            paste0(strsplit(.v, split = "_")[[1]], collapse= ".")
                          }
                        } else {
                          "label"
                        }
                      }, USE.NAMES = FALSE)
  names(sim_args) <- new_names
  sim_args
}