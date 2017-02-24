module SubprimeFib.A282813 (a282813) where
import Helpers.Table (n_k)
import Helpers.SubprimeFib (loopPosition)

a282813 n = case n_k (n - 1) of (a, b) -> loopPosition (a + 1) (b + 1)
