module DVI2HTML where

import Control.Monad.State
import DVIInstructions

data Font = Font {
  path        :: String,
  glyphWidths :: [Int]
  } deriving (Show)

data Glyph = Glyph {
  xpos       :: Int,
  ypos       :: Int,
  glyphFont  :: Font,
  index      :: Int
  } deriving (Show)

--We include the font in our stack, and just don't pop it on DVIInstr "pop".
data DVIStackState = DVIStackState {
  hs          :: [Int],
  vs          :: [Int],
  ws          :: [Int],
  xs          :: [Int],
  ys          :: [Int],
  zs          :: [Int],
  currentFont :: Font ,
  definedFonts:: [Font]
  } deriving (Show)

type Stack = StateT DVIStackState

--this needs to become execDVIInstr :: DVIInstr -> Stack Maybe Glyph
-- where Stack :: StateT DVIStackState
execDVIInstr :: DVIInstr -> Stack Maybe Glyph
execDVIInstr instr = do 
    stackstate <- get
    let h    = head (hs stackstate)
        ths  = tail (hs stackstate)
        v    = head (vs stackstate)
        tvs  = tail (vs stackstate)
        w    = head (ws stackstate)
        tws  = tail (ws stackstate)
        x    = head (xs stackstate)
        txs  = tail (xs stackstate)
        y    = head (ys stackstate)
        tys  = tail (ys stackstate)
        z    = head (zs stackstate)
        tzs  = tail (zs stackstate)
        font = currentFont stackstate
    case instr of
         SetChar charnum -> do
	   let charWidth = (glyphWidths font) !! charnum
	   put $ stackstate
	          { hs=h+charWidth : ths}
	   return $ Glyph h v font charnum
         SetRule height width -> do
           put $ stackstate
                  { hs=h+width : ths}
           lift Nothing
         Put charnum -> do
           return $ Glyph h v font charnum
         PutRule height width -> do
           lift Nothing
         Nop -> lift Nothing
         Bop c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 p -> do
           put $ stackstate
                  { hs = [],
                    vs = [],
                    ws = [],
                    xs = [],
                    ys = [],
                    zs = [],
                    currentFont = Font [] [] }
           lift Nothing
         Eop -> lift Nothing
         Push -> do
           put $ stackstate
                  { hs = h : hs stackstate,
                    vs = v : vs stackstate,
                    ws = w : ws stackstate,
                    xs = x : xs stackstate,
                    ys = y : ys stackstate,
                    zs = z : zs stackstate,
                    currentFont = font }
           lift Nothing
         Pop -> do
           put $ stackstate
                  { hs = ths,
                    vs = tvs,
                    ws = tws,
                    xs = txs,
                    ys = tys,
                    zs = tzs,
                    currentFont = font }
           lift Nothing
         RightI shift -> do
           put $ stackstate
                  { hs=h+shift : ths}
           lift Nothing
         W0 -> do
           put $ stackstate
                  { hs = h + w : ths}
           lift Nothing
         Wi shift -> do
           put $ stackstate
                  { hs = h + shift : ths,
                    ws = shift     : tws  }
           lift Nothing
         X0 -> do
           put $ stackstate
                  { hs = h + x : ths}
           lift Nothing
         Xi shift -> do
           put $ stackstate
                  { hs = h + shift : ths,
                    xs = shift     : txs  }
           lift Nothing
         DownI shift -> do
           put $ stackstate
                  { vs=v+shift : tvs}
           lift Nothing
         Y0 -> do
           put $ stackstate
                  { vs = v + y : tvs}
           lift Nothing
         Yi shift -> do
           put $ stackstate
                  { vs = v + shift : tvs,
                    ys = shift     : tys  }
           lift Nothing
         Z0 -> do
           put $ stackstate
                  { vs = v + z : tvs}
           lift Nothing
         Zi shift -> do
           put $ stackstate
                  { vs = v + shift : tvs,
                    zs = shift     : tzs  }
           lift Nothing
         FntNum fnum -> do
           put $ stackstate
                  { currentFont = (definedFonts stackstate) !! (fnum -1) }
           lift Nothing
         
           

--dvi2html :: Stack a -> [DVIInstr] -> (a, 

