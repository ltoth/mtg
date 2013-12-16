{-# LANGUAGE GADTs #-}
import Data.Word (Word8)

data Layout = Normal | Split | Flip | DoubleFaced | Token

type Name = String

type Names = [Name]

type ManaCost = [ManaSymbol]

data ManaSymbol = W | U | B | R | G | S | CL Word8 | X | Y | Z
                  | GW | WU | RW | WB | UB | GU | UR | BR | BG | RG
                  | W2 | U2 | B2 | R2 | G2 | WP | UP | BP | RP | GP | P

-- FIXME: This really should support decimals, because of Unhinged
type CMC = Word8

data Color = White | Blue | Black | Red | Green

data Colors a = Colors a | Colorless

main = return ()
