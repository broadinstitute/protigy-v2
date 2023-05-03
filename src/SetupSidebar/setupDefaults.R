SETUP_LOG_TRANSFORM <- list(
  choices = c('log10', 'log2', 'None'),
  selected = 'None'
)

SETUP_DATA_NORMALIZATION <- list(
  choices = c('Median',
              'Median (non-zero)',
              'Quantile', 
              'VSN', 
              'Median-MAD',
              'Median-MAD (non-zero)',
              '2-component', 
              'Upper-quartile',
              'None'),
  selected = 'None'
)

SETUP_FILTER_DATA <- list(
  choices = c('Reproducibility', 'StdDev', 'None'),
  selected = 'None'
)

SETUP_SELECT_TEST <- list(
  choices = c('One-sample mod T', 'Two-sample mod T', 'mod F', 'None'),
  selected = 'None'
)