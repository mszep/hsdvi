module DVIInstructions where


data DVIInstr = SetChar Int 
              | SetRule  Int Int
              | Put Int
              | PutRule  Int Int
              | Nop
              | Bop Int Int Int Int Int Int Int Int Int Int Int
              | Eop
              | Push
              | Pop
              | RightI Int
              | W0
              | Wi Int
              | X0
              | Xi Int
              | DownI Int
              | Y0
              | Yi Int
              | Z0
              | Zi Int
              | FntNum Int
              | Special Int String
              | FontDef Int Int Int Int Int Int String
              | Preamble Int Int Int Int Int String
              | Postamble Int Int Int Int Int Int Int Int
              | PostPost Int Int
                deriving (Eq)

instance Show DVIInstr where
  show (SetChar charnumber)       = "Setchar " ++ show charnumber
  show (Preamble _ num den _ _ descr) 
     = "247: preamble, num: " ++ show num ++ 
       "den: " ++ show den ++ 
       "message: " ++ descr
  show (SetRule h w) = "SetRule; height: " ++ show h ++ ", width: " ++ show w
  show (Put charnumber)  = "Put " ++ show charnumber
  show (PutRule h w) = "PutRule; height: " ++ show h ++ ", width: " ++ show w
  show Nop = "Nop"
  show (Bop _ _ _ _ _ _ _ _ _ _ _) = "Beginning of page"
  show Eop = "Eop"
  show Push = "Push"
  show Pop = "Pop"
  show (RightI dist) = "RightI " ++ show dist
  show W0 = "W0"
  show (Wi dist) = "Wi " ++ show dist
  show X0 = "X0"
  show (Xi dist) = "Xi " ++ show dist
  show (DownI dist) = "Down " ++ show dist
  show Y0 = "Y0"
  show (Yi dist) = "Yi " ++ show dist
  show Z0 = "Z0"
  show (Zi dist) = "Zi " ++ show dist
  show (FntNum num) = "FontNum " ++ show num
  show (Special descrLength descr) = "Special; " ++ show descrLength ++ descr
  show (FontDef _ _ _ _ _ _ fontpath) = "FontDef; path: " ++ fontpath
  show (Postamble _ _ _ _ _ _ _ _) = "Postamble"
  show (PostPost _ _) = "Post_Post"

writesGlyph :: DVIInstr -> Bool
writesGlyph (SetChar _) = True
writesGlyph (Put     _) = True
writesGlyph _           = False
