module HelperSequences.A066099 (a066099) where
import Miscellaneous.A228351 (a228351)

a066099 = (!!) a066099_list
a066099_list = concat a066099_tabf
a066099_tabf = map a066099_row [0..]

a066099_row 0 = [0]
a066099_row = reverse . a228351_row
