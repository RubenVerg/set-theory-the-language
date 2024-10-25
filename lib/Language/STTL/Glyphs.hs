{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Language.STTL.Glyphs where

comment = '\''
setOpen = '{'
setClose = '}'
elementSeparator = ','
groupLeft = '('
groupRight = ')'
highMinus = '¯'

syntax =
  [ comment
  , setOpen
  , setClose
  , elementSeparator
  , groupLeft
  , groupRight
  , highMinus
  ]

emptySet = '∅'

constants =
  [ emptySet
  ]

count = '#'
convert = '→'

monads =
  [ count
  , convert
  ]

union = '∪'
intersection = '∩'
difference = '∖'
subset = '⊆'
superset = '⊇'
element = '∈'
contains = '∋'
cartesianProduct = '×'
pair = ';'
plus = '+'
minus = '-'

dyads =
  [ union
  , intersection
  , difference
  , subset
  , superset
  , element
  , contains
  , cartesianProduct
  , pair
  , plus
  , minus
  ]

upperDouble = "𝔸𝔹ℂ𝔻𝔼𝔽𝔾ℍ𝕀𝕁𝕂𝕃𝕄ℕ𝕆ℙℚℝ𝕊𝕋𝕌𝕍𝕎𝕏𝕐ℤ"
lowerDouble = "𝕒𝕓𝕔𝕕𝕖𝕗𝕘𝕙𝕚𝕛𝕜𝕝𝕞𝕟𝕠𝕡𝕢𝕣𝕤𝕥𝕦𝕧𝕨𝕩𝕪𝕫"
digitsDouble = "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡"

double = upperDouble ++ lowerDouble ++ digitsDouble
