module Cipher where

  import Data.Char

  cryptF x y = x + y

  decryptF x y = x - y

  getCO x = mod (26 + x) 26

  cryptCharF f o c = chr $ firstCharMode + (getCO (f charO o))
    where
      firstCharMode = if (isUpper c) then (ord 'A') else (ord 'a')
      charO = ord c - firstCharMode

  ceaserCrypt o str = map crypt str
    where crypt = cryptCharF cryptF o

  ceaserDecrypt o str = map decrypt str
    where decrypt = cryptCharF decryptF o
