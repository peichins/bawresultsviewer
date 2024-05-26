# Installation

```R
install_github('peichins/bawresultsviewer')
```

To use `install_github()`, make sure that either `remotes` or `devtools` are installed

# Use

1. Create a config list object (see configuration)
2. launchServer('my_data.rds', myconfig)


# Configuration

Pass configuration as a list. 
Default configuration can be found at ./R/config.R and can be used as a guide


# Data

Data must be formatted correctly. You can pass either a dataframe/tibble or 
a path to an rds file. The table must contain these columns:
- site
- label
- score
- timestamp
- offset
- arid


# Example




