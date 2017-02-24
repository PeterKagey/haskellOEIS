module SubprimeFib.A282812 (a282812) where
import Helpers.Table (n_k)
import Helpers.SubprimeFib (largestValue)

a282812 n = case n_k (n - 1) of (a, b) -> largestValue (a + 1) (b + 1)
