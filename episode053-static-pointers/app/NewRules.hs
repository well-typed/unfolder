module NewRules where

import GHC.StaticPtr

newtype Suspended = Suspended (StaticPtr (IO ()))

suspended1 :: Suspended
suspended1 = Suspended $
    static (putStrLn "Process was suspended")

-- Warning in 9.14.2, illegal in 9.16:
--
-- > From GHC 9.16, static forms cannot mention nested let-bound variables
-- >   Offending variables: msg
-- > Solution: move these free variables to top level
suspended2 :: Suspended
suspended2 = Suspended $
    let msg2 :: String
        msg2 = "Process was suspended"
    in static (putStrLn msg2)

-- But this is still fine.
--
-- So only variables defined at the top-level (or /local/ variables, of course)
suspended3 :: Suspended
suspended3 = Suspended $ static (putStrLn msg3)

msg3 :: String
msg3 = "Process was suspended"

-- And so is this
suspended4 :: Suspended
suspended4 = Suspended $
    static (
      let msg4 :: String
          msg4 = "Process was suspended"
      in putStrLn msg4
    )

main :: IO ()
main = putStrLn "Done"