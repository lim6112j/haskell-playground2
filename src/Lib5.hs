module Lib5 where
import Data.Monoid
import TextShow
data Example = Example Int Int
instance TextShow Example where
  showb (Example i1 i2) = showb i1 <> showbSpace <> showb i2
