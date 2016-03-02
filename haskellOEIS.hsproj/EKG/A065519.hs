module EKG.A065519 (a065519, a065519_sequence) where
  import EKG.A064413
  a065519 n = (a064413 n) - n
  a065519_sequence = map a064413 [1..]