data FloatWithError
  = FWE Float Float

add :: FloatWithError FloatWithError FloatWithError
add (FWE l1 h1) (FWE l2 h2) = FWE (l1 + l2) (h1 + h2)

multiply :: FloatWithError FloatWithError FloatWithError
multiply (FWE l1 h1) (FWE l2 h2)
  | l1 >= 0 && l2 >= 0                       = FWE (l1 * l2) (h1 * h2)
  | l1 <= 0 && h1 >= 0 && l2 >= 0            = FWE (l1 * h2) (h1 * h2) -- wrong?
  | l1 <= 0 && h1 >= 0 && l2 <= 0 && h2 >= 0 = FWE (min (l1 * h2) (h1 * l2)) (max (l1 * l2) (h1 * h2))
  | l1 >= 0 && l2 <= 0 && h2 >= 0            = FWE (h1 * l2) (h1 * h2) -- wrong?
  | l1 <= 0 && h1 >= 0 && h2 <= 0            = FWE (h1 * l2) (l1 * h2)
  | h1 <= 0 && l2 <= 0 && h2 >= 0            = FWE (l1 * h2) (h1 * l2)
  | l1 >= 0 && h2 <= 0                       = FWE (h1 * l2) (l1 * h2)
  | h1 <= 0 && l2 >= 0                       = FWE (l1 * h2) (h1 * l2)
  | h1 <= 0 && h2 <= 0                       = FWE (l1 * l2) (h1 * h2)
