
# GDD functions ----

calc_gdd <- function(tmin, tmax, base, upper) {
  if (base > upper) stop("Base cannot be greater than Upper")
  mapply(gdd_sine, tmin, tmax, base, upper)
}

gdd_sine <- function(tmin, tmax, base, upper) {
  if (is.na(tmin) || is.na(tmax)) return(NA)

  # swap min and max if in wrong order for some reason
  if (tmin > tmax) {
    t = tmin
    tmin = tmax
    tmax = t
  }

  # min and max > upper
  if (tmin >= upper) return(upper - base)

  # min and max < lower
  if (tmax <= base) return(0)

  average = (tmin + tmax) / 2

  # min and max between base and upper
  if (tmax <= upper && tmin >= base) return(average - base)

  alpha = (tmax - tmin) / 2

  # min < base, max between base and upper
  if (tmax <= upper && tmin < base) {
    base_radians = asin((base - average) / alpha)
    a = average - base
    b = pi / 2 - base_radians
    c = alpha * cos(base_radians)
    return((1 / pi) * (a * b + c))
  }

  # max > upper and min between base and upper
  if (tmax > upper && tmin >= base) {
    upper_radians = asin((upper - average) / alpha)
    a = average - base
    b = upper_radians + pi / 2
    c = upper - base
    d = pi / 2 - upper_radians
    e = alpha * cos(upper_radians)
    return((1 / pi) * (a * b + c * d - e))
  }

  # max > upper and min < base
  if (tmax > upper && tmin < base) {
    base_radians = asin((base - average) / alpha)
    upper_radians = asin((upper - average) / alpha)
    a = average - base
    b = upper_radians - base_radians
    c = alpha * (cos(base_radians) - cos(upper_radians))
    d = upper - base
    e = pi / 2 - upper_radians
    return((1 / pi) * ((a * b + c) + (d * e)))
  }
}
