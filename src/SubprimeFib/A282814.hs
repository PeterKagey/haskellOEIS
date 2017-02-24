module SubprimeFib.A282814 (a282814) where
import Helpers.Table (n_k)
import Helpers.SubprimeFib (loopLength)

a282814 n = case n_k (n - 1) of (a, b) -> loopLength (a + 1) (b + 1)
