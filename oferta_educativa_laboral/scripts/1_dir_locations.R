############
# SIAP
# Dirección de Administración
# Unidad de Personal
# Octubre 2024
# Directory structure and paths
# No input
# Output is rdata with the directories for the project
############


############
# Directory locations

###
print(getwd())

project_name <- 'oferta_educativa_laboral'
projects_dir <- here::here()

project_root <- here::here()
setwd(here::here())
print(getwd())

data_dir <- sprintf('%s/%s', project_root, 'data/')
devel_dir <- here::here()
code_dir <- sprintf('%s/%s',
                    devel_dir,
                    paste0(project_name, '/', project_name)
                    )
results_dir <- sprintf('%s/%s', project_root, 'results')

cat('CWD is:', '\n',
      getwd(), '\n', '\n',
      'project_root is:', '\n',
      project_root, '\n', '\n',
      'data_dir is:', '\n',
      data_dir, '\n', '\n',
      'devel_dir is:', '\n',
      devel_dir, '\n', '\n',
      'code_dir is:', '\n',
      code_dir, '\n', '\n',
      results_dir
      )
###

###
# Check directories:
all_locs <- c(getwd(),
              project_root,
              data_dir,
              devel_dir,
              code_dir,
              results_dir
              )

# Loop through each directory in `all_locs`
for (dir in all_locs) {
  # Check if the directory exists
  if (dir.exists(dir)) {
    # Check if the directory is empty
    files <- list.files(dir)
    if (length(files) == 0) {
      cat("Directory exists and is empty:", "\n", dir, "\n", "\n")
    } else {
      cat("Directory exists and is not empty:", "\n", dir, "\n", "\n")
    }
  } else {
    cat("Directory does not exist:", "\n", dir, "\n", "\n")
  }
}
############


############
# The end:
# Save objects, to eg .RData file:
print(data_dir)
dir(data_dir)

processed_data_dir <- sprintf('%s/data_UP/access_SIAP_18092024/processed/', data_dir)
script_n <- 'dir_locations'
suffix <- 'rdata.gzip'
outfile <- sprintf(fmt = '%s/%s.%s', processed_data_dir, script_n, suffix)
outfile

# Objects with directory information
objects_to_save <- c(
  'all_locs',
  'project_root',
  'data_dir',
  'devel_dir',
  'code_dir',
  'results_dir'
)

message('Skipping save to ', outfile)

# Clean up session
all_objects <- ls()
rm_list <- which(!all_objects %in% objects_to_save)
rm(list = all_objects[rm_list])
ls()

sessionInfo()
############
