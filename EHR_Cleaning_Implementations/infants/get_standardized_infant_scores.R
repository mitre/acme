get_standardized_infant_scores <- function(param, agedays, sex, measurement,
                                           csd = F) {

  # avoid "no visible bindings" warning
  # src <- param <- sex <- age <- ret <- m <- NULL
  # csdneg <- csdpos <- s <- NULL

  # Pull in the needed files for the data.
  weianthro_path <- "C:/Users/molivier/Documents/Projects/CDC_CODI/Standardized_Scores/weianthro.txt"
  lenanthro_path <- "C:/Users/molivier/Documents/Projects/CDC_CODI/Standardized_Scores/lenanthro.txt"
  bmianthro_path <- "C:/Users/molivier/Documents/Projects/CDC_CODI/Standardized_Scores/bmianthro.txt"

  # Create a list with all the data tables.
  l <- list(
    with(
      read.table(weianthro_path, header = T),
      data.frame(
        src = 'WHO',
        param = 'WEIGHTKG',
        sex = sex - 1,
        age,
        l,
        m,
        s,
        csdpos = as.double(NA),
        csdneg = as.double(NA)
      )
    ),
    with(
      read.table(lenanthro_path, header = T),
      data.frame(
        src = 'WHO',
        param = 'HEIGHTCM',
        sex = sex - 1,
        age,
        l,
        m,
        s,
        csdpos = as.double(NA),
        csdneg = as.double(NA)
      )
    ),
    with(
      read.table(bmianthro_path, header = T),
      data.frame(
        src = 'WHO',
        param = 'BMI',
        sex = sex - 1,
        age,
        l,
        m,
        s,
        csdpos = as.double(NA),
        csdneg = as.double(NA)
      )
    )
  )

  # Bind everything into
  anthro <- rbindlist(l)
  setkey(anthro, src, param, sex, age)

  # Set the source.
  src <- "WHO"

  # keep column sequence the same fo efficient join
  dt <- data.table(src, param, sex, agedays, measurement)
  dt <- anthro[dt] # HOW DOES THIS WORK!!

  dt[, ret := as.double(NA)]

  if (csd) {
    dt[measurement < m, ret := (measurement - m) / csdneg]
    dt[measurement >= m, ret := (measurement - m) / csdpos]
  } else {
    dt[l == 0, ret := log(measurement / m) / s]
    dt[l != 0, ret := (((measurement / m) ^ l) - 1) / (l * s)]
  }
  return(dt$ret)
}
