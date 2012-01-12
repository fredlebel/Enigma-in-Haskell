import Char
import List
import Maybe

-- ========== GENERIC FUNCTIONS ========== --
-- Simple piping function
a |> f = f a

-- Function to replace all occurences of a
-- certain item in a list with another item.
replace :: Eq a => a -> a -> [a] -> [a]
replace a b [] = []
replace a b (x:xs)
    | x == a    = b:(replace a b xs)
    | otherwise = x:(replace a b xs)

-- Convert a Char to Int where 'A' = 0
ordA0 :: Char -> Int
ordA0 ch = ord ch - ord 'A'

-- Convert an Int to Char where 'A' = 0
chrA0 :: Int -> Char
chrA0 i = chr (i + ord 'A')

-- Find the index of an element in a list without using Maybe.
idx :: Eq a => a -> [a] -> Int
idx a b = case (elemIndex a b) of (Just i) -> i

-- Rolls a char either left or right.
rollCh :: Int -> Char -> Char
rollCh n ch
    | n < 0     = rollCh (n+26) ch
    | otherwise = ch |> ordA0 |> (+n) |> flip mod 26 |> chrA0

-- Rolls a list of Char.
rollStr :: Int -> String -> String
rollStr 0 str = str
rollStr n (x:xs) = rollStr (n-1) (xs ++ [x])

-- ================================================================== --

-- All Enigma rotors.  Standard rotors are (rotor chars, notch, offset, display)
data Rotor = Rotor (String, Char, Int, Char) | Flat String
    deriving (Show)

type Reflector = Rotor
type Plugboard = Rotor

-- Enigma machine contains a plugboard, 
data Enigma = Enigma (Plugboard, [Rotor], Reflector)
--    deriving (Show)
instance Show Enigma where
    show (Enigma (plugboard, rotors, reflector)) = 
        "\nEnigma: \n"
        ++ "    Plugboard    : " ++ (show plugboard) ++ "\n"
        ++ "    Left Rotor   : " ++ (show (rotors !! 0)) ++ "\n"
        ++ "    Middle Rotor : " ++ (show (rotors !! 1)) ++ "\n"
        ++ "    Right Rotor  : " ++ (show (rotors !! 2)) ++ "\n"
        ++ "    Plugboard    : " ++ (show reflector) ++ "\n\n"


-- Rolls the rotor by n
rollRotor :: Int -> Rotor -> Rotor
rollRotor n (Rotor (str, notch, off, disp)) =  Rotor (newStr, notch, off, newDisp)
    where newStr = rollStr n str
          newDisp = rollCh n disp

-- Do a forward char mapping through a rotor.
mapCharFwd :: Rotor -> Char -> Char
mapCharFwd (Rotor (str, notch, off, disp)) ch = ch
    -- Get char at ch position on the rotor
    |> ordA0 |> (str !!)
    -- Subtract the display char and offset
    |> rollCh (26 - (ordA0 disp - (off-1)))
mapCharFwd (Flat str) ch = ordA0 ch |> (str !!)

-- Do a backward char mapping through a rotor.
mapCharBkw :: Rotor -> Char -> Char
mapCharBkw (Rotor (str, notch, off, disp)) ch = ch
    -- Add the display char
    |> rollCh (ordA0 disp - (off-1))
    -- Find index of that char on the rotor
    |> flip idx str |> chrA0
mapCharBkw (Flat str) ch = ch |> flip idx str |> chrA0

-- Steps a rotor and also returns of the step notch has been hit.
stepRotor :: Rotor -> (Rotor, Bool)
stepRotor rotor@(Rotor (_, notch, _, display))
    | display == notch = (rollRotor 1 rotor, True)
    | otherwise        = (rollRotor 1 rotor, False)

{- Odometer style rotor stepping.
stepRotors :: [Rotor] -> [Rotor]
stepRotors [rotor] = [stepRotor rotor |> fst]
stepRotors rotors = case stepRotor (last rotors) of
    (rotor, True)  -> (stepRotors (init rotors)) ++ [rotor]
    (rotor, False) -> (init rotors) ++ [rotor]
-}

--   .     .
-- BPV -> BQW -> CRX -> CRY
-- Special rotor stepping with hack for double stepping.
stepRotors :: [Rotor] -> [Rotor]
stepRotors [r0, r1, r2] = case stepRotor (r2) of
    (newR2, False) -> case stepRotor (r1) of
        (newR1, False) -> [r0] ++ [r1] ++ [newR2]
        (newR1, True)  -> case stepRotor (r0) of
            (newR0, _) -> [newR0] ++ [newR1] ++ [newR2]
    (newR2, True)  -> case stepRotor (r1) of
        (newR1, False) -> [r0] ++ [newR1] ++ [newR2]
        (newR1, True)  -> case stepRotor (r0) of
                (newR0, _) -> [newR0] ++ [newR1] ++ [newR2]

-- Steps the Enigma machine by one.
stepEnigma :: Enigma -> Enigma
stepEnigma (Enigma (pb, rotors, rf)) = Enigma (pb, stepRotors rotors, rf)

-- Steps the enigma machine by n.
stepEnigmaN :: Int -> Enigma -> Enigma
stepEnigmaN 0 enigma = enigma
stepEnigmaN n (Enigma (pb, rotors, rf)) = stepEnigmaN (n-1) (Enigma (pb, stepRotors rotors, rf))

-- Using an Enigma, process a single char.
cryptCh :: Enigma -> Char -> Char
cryptCh (Enigma (plugboard, rotors, reflector)) ch = 
    ch
        |> mapCharFwd plugboard
        |> mapCharFwd (rotors !! 2)
        |> mapCharFwd (rotors !! 1)
        |> mapCharFwd (rotors !! 0)
        |> mapCharFwd reflector
        |> mapCharBkw (rotors !! 0)
        |> mapCharBkw (rotors !! 1)
        |> mapCharBkw (rotors !! 2)
        |> mapCharBkw plugboard

-- Using an Enigma, process a string.
cryptStr :: Enigma -> String -> String
cryptStr enigma [] = []
cryptStr enigma (x:xs) = (cryptCh steppedEnigma x):(cryptStr steppedEnigma xs)
    where steppedEnigma = stepEnigma enigma

cryptStrMem :: Enigma -> String -> [(Enigma, Char)]
cryptStrMem enigma [] = []
cryptStrMem enigma (x:xs) = (steppedEnigma, cryptCh steppedEnigma x):(cryptStrMem steppedEnigma xs)
    where steppedEnigma = stepEnigma enigma

-- Initializes a rotor based on ring setting and message.
initRotor :: String -> Char -> Int -> Char -> Rotor
initRotor str notch off ch = Rotor (newStr, notch, off, ch)
    where newStr = rollStr (ordA0 ch + 27 - off) str

-- ROTORS             ABCDEFGHIJKLMNOPQRSTUVWXYZ
rotor1   = initRotor "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'Q'
rotor2   = initRotor "AJDKSIRUXBLHWTMCQGZNPYFVOE" 'E'
rotor3   = initRotor "BDFHJLCPRTXVZNYEIWGAKMUSQO" 'V'
rotor4   = initRotor "ESOVPZJAYQUIRHXLNFTGKDCMWB" 'J'
rotor5   = initRotor "VZBRGITYUPSDNHLXAWMJQOFECK" 'Z'
rotor6   = initRotor "JPGVOUMFYQBENHZRDKASXLICTW" 'Z'
rotor8   = initRotor "FKQHTLXOCBJSPDZRAMEWNIUYGV" 'Z'
rotor7   = initRotor "NZJHGRCXMYSWBOUFAIVLPEKQDT" 'Z'
-- REFLECTORS       ABCDEFGHIJKLMNOPQRSTUVWXYZ
reflectorA  = Flat "EJMZALYXVBWFCRQUONTSPIKHGD"
reflectorB  = Flat "YRUHQSLDPXNGOKMIEBFZCWVJAT"
reflectorC  = Flat "FVPJIAOYEDRZXWGCTKUQSBNMHL"
-- Plugboard: EZ RW MV IU BL PX JO
plugboard  = Flat "ALCDZFGHUOKBVNJXQWSTIMRPYE";
        

main = do
    Enigma (plugboard, [rotor2 1 'B', rotor1 23 'G', rotor3 4 'J'], reflectorB)
        |> show |> putStrLn
        
    putStrLn "--------------------------"

    --   .     .
    -- BPV -> BQW -> CRX -> CRY
    stepEnigmaN 0 (Enigma (plugboard, [rotor2 1 'B', rotor1 23 'P', rotor3 4 'V'], reflectorB))
        |> show |> putStrLn
    stepEnigmaN 1 (Enigma (plugboard, [rotor2 1 'B', rotor1 23 'P', rotor3 4 'V'], reflectorB))
        |> show |> putStrLn
    stepEnigmaN 2 (Enigma (plugboard, [rotor2 1 'B', rotor1 23 'P', rotor3 4 'V'], reflectorB))
        |> show |> putStrLn
    stepEnigmaN 3 (Enigma (plugboard, [rotor2 1 'B', rotor1 23 'P', rotor3 4 'V'], reflectorB))
        |> show |> putStrLn
    stepEnigmaN 4 (Enigma (plugboard, [rotor2 1 'B', rotor1 23 'P', rotor3 4 'V'], reflectorB))
        |> show |> putStrLn
    
    {-
    stepEnigmaN 1 (Enigma (plugboard, [rotor2 1 'B', rotor1 23 'G', rotor3 4 'J'], reflectorB))
        |> show |> putStrLn
    stepEnigmaN 10 (Enigma (plugboard, [rotor2 1 'B', rotor1 23 'G', rotor3 4 'J'], reflectorB))
        |> show |> putStrLn
    stepEnigmaN 11 (Enigma (plugboard, [rotor2 1 'B', rotor1 23 'G', rotor3 4 'J'], reflectorB))
        |> show |> putStrLn
    stepEnigmaN 12 (Enigma (plugboard, [rotor2 1 'B', rotor1 23 'G', rotor3 4 'J'], reflectorB))
        |> show |> putStrLn
    stepEnigmaN 13 (Enigma (plugboard, [rotor2 1 'B', rotor1 23 'G', rotor3 4 'J'], reflectorB))
        |> show |> putStrLn
    -}

    putStrLn "--------------------------"

    "QKRQWUQTZKFXZOMJFOYRHYZWVBXYSIWMMVWBLEBDMWUWBTVHMR\
        \FLKSDCCEXIYPAHRMPZIOVBBRVLNHZUPOSYEIPWJTUGYOSLAOXR\
        \HKVCHQOSVDTRBPDJEUKSBBXHTTGVHGFICACVGUVOQFAQWBKXZJ\
        \SQJFZPEVJROJTOESLBQHQTRAAHXVYAUHTNBGIBVCLBLXCYBDMQ\
        \RTVPYKFFZXNDDPCCJBHQFDKXEEYWPBYQWDXDRDHNIGDXEUJJPV\
        \MHUKPCFHLLFERAZHZOHXDGBKOQXKTLDVDCWKAEDHCPHJIWZMMT\
        \UAMQENNFCHUIAWCCHNCFYPWUARBBNIEPHGDDKMDQLMSNMTWOHM\
        \AUHRHGCUMQPKQRKDVSWVMTYVNFFDDSKIISXONXQHHLIYQSDFHE\
        \NCMCOMREZQDRPBMRVPQTVRSWZPGLPITRVIBPXXHPRFISZTPUEP\
        \LKOTTXNAZMHTJPCHAASFZLEFCEZUTPYBAOSKPZCJCYZOVAPZZV\
        \ELBLLZEVDCHRMIOYEPFVUGNDLENISXYCHKSJUWVXUSBITDEQTC\
        \NKRLSNXMXYZGCUPAWFULTZZSFAHMPXGLLNZRXYJNSKYNQAMZBU\
        \GFZJCURWGTQZCTLLOIEKAOISKHAAQFOPFUZIRTLWEVYWMDN"
        |> cryptStr (Enigma (plugboard, [rotor2 1 'B', rotor1 23 'G', rotor3 4 'I'], reflectorB))
        |> replace 'X' ' '
        |> show |> putStrLn

    "QKRQWUQTZKFXZOMJFOYRHYZWVBXYSIWMMVWBLEBDMWUWBTVHMR\
        \FLKSDCCEXIYPAHRMPZIOVBBRVLNHZUPOSYEIPWJTUGYOSLAOXR\
        \HKVCHQOSVDTRBPDJEUKSBBXHTTGVHGFICACVGUVOQFAQWBKXZJ\
        \SQJFZPEVJROJTOESLBQHQTRAAHXVYAUHTNBGIBVCLBLXCYBDMQ\
        \RTVPYKFFZXNDDPCCJBHQFDKXEEYWPBYQWDXDRDHNIGDXEUJJPV\
        \MHUKPCFHLLFERAZHZOHXDGBKOQXKTLDVDCWKAEDHCPHJIWZMMT\
        \UAMQENNFCHUIAWCCHNCFYPWUARBBNIEPHGDDKMDQLMSNMTWOHM\
        \AUHRHGCUMQPKQRKDVSWVMTYVNFFDDSKIISXONXQHHLIYQSDFHE\
        \NCMCOMREZQDRPBMRVPQTVRSWZPGLPITRVIBPXXHPRFISZTPUEP\
        \LKOTTXNAZMHTJPCHAASFZLEFCEZUTPYBAOSKPZCJCYZOVAPZZV\
        \ELBLLZEVDCHRMIOYEPFVUGNDLENISXYCHKSJUWVXUSBITDEQTC\
        \NKRLSNXMXYZGCUPAWFULTZZSFAHMPXGLLNZRXYJNSKYNQAMZBU\
        \GFZJCURWGTQZCTLLOIEKAOISKHAAQFOPFUZIRTLWEVYWMDN"
        |> cryptStrMem (Enigma (plugboard, [rotor2 1 'B', rotor1 23 'G', rotor3 4 'I'], reflectorB))
        |> show |> putStrLn

    "QKRQWUQTZKFXZOMJFOYRHYZWVBXYSIWMMVWBLEBDMWUWBTVHMR\
        \FLKSDCCEXIYPAHRMPZIOVBBRVLNHZUPOSYEIPWJTUGYOSLAOXR\
        \HKVCHQOSVDTRBPDJEUKSBBXHTTGVHGFICACVGUVOQFAQWBKXZJ\
        \SQJFZPEVJROJTOESLBQHQTRAAHXVYAUHTNBGIBVCLBLXCYBDMQ\
        \RTVPYKFFZXNDDPCCJBHQFDKXEEYWPBYQWDXDRDHNIGDXEUJJPV\
        \MHUKPCFHLLFERAZHZOHXDGBKOQXKTLDVDCWKAEDHCPHJIWZMMT\
        \UAMQENNFCHUIAWCCHNCFYPWUARBBNIEPHGDDKMDQLMSNMTWOHM\
        \AUHRHGCUMQPKQRKDVSWVMTYVNFFDDSKIISXONXQHHLIYQSDFHE\
        \NCMCOMREZQDRPBMRVPQTVRSWZPGLPITRVIBPXXHPRFISZTPUEP\
        \LKOTTXNAZMHTJPCHAASFZLEFCEZUTPYBAOSKPZCJCYZOVAPZZV\
        \ELBLLZEVDCHRMIOYEPFVUGNDLENISXYCHKSJUWVXUSBITDEQTC\
        \NKRLSNXMXYZGCUPAWFULTZZSFAHMPXGLLNZRXYJNSKYNQAMZBU\
        \GFZJCURWGTQZCTLLOIEKAOISKHAAQFOPFUZIRTLWEVYWMDN"
        |> cryptStrMem (Enigma (plugboard, [rotor2 1 'B', rotor1 23 'G', rotor3 4 'I'], reflectorB))
        |> foldr (\ (_, ch) str -> ch:str) []
        |> replace 'X' ' '
        |> show |> putStrLn

    {-
    -- QWUO OYCS STNM M->O OPUT TDGW WCGA
    -- KRWP PZJZ ZAAZ Z->T TUHG GQHX XEPI
    -- WEJB BLTJ JKLK K->N NOYX XHPF FNNF
    
    Step 1
                    |
    ABCDEFGHIJKLMNOPQRSTUVWXYZ
    CPRTXVZNYEIWGAKMUSQOBDFHJL (J:10 - off:4 = 6)
                  |
    ABCDEFGHIJKLMNOPQRSTUVWXYZ
    NTOWYHXUSPAIBRCJEKMFLGDQVZ (G:7 - off:23 = 10)
                      |
    ABCDEFGHIJKLMNOPQRSTUVWXYZ
    JDKSIRUXBLHWTMCQGZNPYFVOEA (B:2 - off:1 = 1)

    Step 2
              |
    ABCDEFGHIJKLMNOPQRSTUVWXYZ
    PRTXVZNYEIWGAKMUSQOBDFHJLC (K:11 - off:4 = 7)
                  |
    ABCDEFGHIJKLMNOPQRSTUVWXYZ
    NTOWYHXUSPAIBRCJEKMFLGDQVZ (G:7 - off:23 = 10)
                      |
    ABCDEFGHIJKLMNOPQRSTUVWXYZ
    JDKSIRUXBLHWTMCQGZNPYFVOEA (B:2 - off:1 = 1)
    -}
    
    {- 'Q'
        |> mapCharFwd plugboard
        |> mapCharFwd (rotor3 4 'J')
        |> mapCharFwd (rotor1 23 'G')
        |> mapCharFwd (rotor2 1 'B')
        |> mapCharFwd reflectorB
        |> mapCharBkw (rotor2 1 'B')
        |> mapCharBkw (rotor1 23 'G')
        |> mapCharBkw (rotor3 4 'J')
        |> show |> putStrLn -}
    --initRotor (1, 'B') rotor2 |> show |> putStrLn
    
{-    
rotor1      = Rotor ("EKMFLGDQVZNTOWYHXUSPAIBRCJ", 'Q')
rotor2      = Rotor ("AJDKSIRUXBLHWTMCQGZNPYFVOE", 'E')
rotor3      = Rotor ("BDFHJLCPRTXVZNYEIWGAKMUSQO", 'V')
rotor4      = Rotor ("ESOVPZJAYQUIRHXLNFTGKDCMWB", 'J')
rotor5      = Rotor ("VZBRGITYUPSDNHLXAWMJQOFECK", 'Z')
rotor6      = Rotor ("JPGVOUMFYQBENHZRDKASXLICTW", 'Z')
rotor7      = Rotor ("NZJHGRCXMYSWBOUFAIVLPEKQDT", 'Z')
rotor8      = Rotor ("FKQHTLXOCBJSPDZRAMEWNIUYGV", 'Z')
reflectorB  = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
reflectorC  = "FVPJIAOYEDRZXWGCTKUQSBNMHL"
-}

-- BPV -> BQW -> CRX -> CRY

--              BH                        BI                        BJ                        BK                        BL                        BM                        BN                        BO                        BP                        BQW (should step both again)          
--              .                         .                         .                         .                         .                         .                         .                         .                         .                         .                         .                         .                         .                         .                         .
-- AUFBEFEHLDESOBERSTENBEFEHLSHABERSSINDIMFALLE Z ZT UNWAHRSCHEINLICHEN FRANZOESISQENANGRIFFSDIEWESTBEFESTIGUNGENJEDERZAHLENMAESKIGENUEBERLEGENHEITZUMTROTZZUHALTEN FUEHRUNGUNDTRUPPEMUESSENVONDIESEREHRENPFLIQTDURQDRUNGENSEIN ABS DEMGEMAESSBEHALTEIQMIRDIEERMAEQTIGUNGZURPUFGABEDERBEFESTIGUNGENODERAUQVONTEILENAUSDRUECKLIQPERSOENLIQVOR ABS AENDERUNGDERANWEISUNG OKH GEN ST D H ERSTEABT NR DREIDREIZWOEINS DREIAQTG KDOS VOMJULIEINSNEUNDREIAQTBLEIBTVORBEHALTEN DEROBERBEFEHLSHABERDESHEERESKRKRFLOTTENCHEFANOKMMM  TORPEDOTREFFERACHTERAUS SCHIFFMANOEVRIERUNFAEHIG WIRKAEMPFENBISZURLETZTENGRANATE ESLEBEDERFUEHRER DERFUEHRERISTTOT DERKAMPFGEHTWEITER DOENITZ
-- AUFBEFEHLDESOBERSTENBEFEHLSHABERSSINDIMFALLE Z ZT UNWAHRSCHEINLICHEN FRANZOESISQENANGRIFFSDIEWESTBEFESTIGUNGENJEDERZAHLENMAESKIGENUEBERLEGENHEITZUMTROTZZUHALTEN FUEHRUNGUNDTRUPPEMUESSENVONDIESEREHRENPFLIQTDURQDRUNGENSEIN ABS DEMGEMAESSBEHALTEIQMIRDIEERMAEQTIGUNGZURPUFGABEDERBEFESTIGUNGENODERAUQVONTEILENAUSDRUECKLIQPERSOENLIQVOR ABS AENDERUNGDERANWEISUNG OKH GEN ST D H ERSTEABT NR DREIDREIZWOEINS DREIAQTG KDOS VOMJULIEINSNEUNDREIAQTBLEIBTVORBEHALTEN DEROBERBEFEHLSHABERDESHEERESKRKRFLOTTENCHEFANOKMMM  TORPEDOTREFFERACHTERAUS SCHIFFMANOEVRIERUNFAEHIG WIRKAEMPFENBISZURLETZTENGRANATE ESLEBEDERFUEHRER DERFUEHRERISTTOT DERKAMPFGEHTWEITER DOENITZ 
--              |
--                                                                                     |
{-
=============================================================
 QKRQW UQTZK FXZOM JFOYR HYZWV BXYSI WMMVW BLEBD MWUWB TVHMR
 FLKSD CCEXI YPAHR MPZIO VBBRV LNHZU POSYE IPWJT UGYOS LAOXR
 HKVCH QOSVD TRBPD JEUKS BBXHT TGVHG FICAC VGUVO QFAQW BKXZJ
 SQJFZ PEVJR OJTOE SLBQH QTRAA HXVYA UHTNB GIBVC LBLXC YBDMQ
 RTVPY KFFZX NDDPC CJBHQ FDKXE EYWPB YQWDX DRDHN IGDXE UJJPV
 MHUKP CFHLL FERAZ HZOHX DGBKO QXKTL DVDCW KAEDH CPHJI WZMMT
 UAMQE NNFCH UIAWC CHNCF YPWUA RBBNI EPHGD DKMDQ LMSNM TWOHM
 AUHRH GCUMQ PKQRK DVSWV MTYVN FFDDS KIISX ONXQH HLIYQ SDFHE
 NCMCO MREZQ DRPBM RVPQT VRSWZ PGLPI TRVIB PXXHP RFISZ TPUEP
 LKOTT XNAZM HTJPC HAASF ZLEFC EZUTP YBAOS KPZCJ CYZOV APZZV
 ELBLL ZEVDC HRMIO YEPFV UGNDL ENISX YCHKS JUWVX USBIT DEQTC
 NKRLS NXMXY ZGCUP AWFUL TZZSF AHMPX GLLNZ RXYJN SKYNQ AMZBU
 GFZJC URWGT QZCTL LOIEK AOISK HAAQF OPFUZ IRTLW EVYWM DN(FRQ)
=============================================================
 AUFBEFEHLDESOBERSTENBEFEHLSHABERSSINDIMFALLEXZXZTX
 UNWAHRSCHEINLICHENXFRANZOESISQENAN*GRIFFSDIEWESTBEF
 ESTIGUNGENJEDERZAHLENMAESKIGENUEBERLEGENHEITZUMTRO
 TZZUHALTENXFUEHRUNGUNDTRUPPEMUESSENVONDIESEREHRENP
 FLIQTDURQDRUNGENSEINXABSXDEMGEMAESSBEHALTEIQMIRDIE
 ERMAEQTIGUNGZURPUFGABEDERBEFESTIGUNGENODERAUQVONTE
 ILENAUSDRUECKLIQPERSOENLIQVORXABSXAENDERUNGDERANWE
 ISUNGXOKHXGENXSTXDXHXERSTEABTXNRXDREIDREIZWOEINSXD
 REIAQTGXKDOSXVOMJULIEINSNEUNDREIAQTBLEIBTVORBEHALT
 ENXDEROBERBEFEHLSHABERDESHEERESKRKRFLOTTENCHEFANOK
 MMMXXTORPEDOTREFFERACHTERAUSXSCHIFFMANOEVRIERUNFAE
 HIGXWIRKAEMPFENBISZURLETZTENGRANATEXESLEBEDERFUEHR
 ERXDERFUEHRERISTTOTXDERKAMPFGEHTWEITERXDOENITZX(XXX)
=============================================================
 Rotors:   II   I  III 
 Rings:     1  23   4
 Message:   2   7   9
 Plugboard: EZ RW MV IU BL PX JO    

 1  1  1
 2 11  6
 B  K  F

 Rings:    26  17  22
 Message:   1   1   1
=============================================================


 ABDC CCDD DDFF F->S SSSS SSEE EFCB
 BDHF FFII IIVV V->W WWNN NNTT TVLJ
 CFLI IIXX XXRR R->B BBWW WWMM MPHE
 DHPL LLHH HHQQ Q->E EEAA AAAA AEPL
 EJTO OOMM MMOO O->M MMCC CCPP PUWR
 FLVP PPCC CCMM M->O OOMM MMOO OUWQ
 GNNG GGRR RRUU U->C CCYY YYVV VCGZ

Rotor 1: ABCDEFGHIJKLMNOPQRSTUVWXYZ
         EKMFLGDQVZNTOWYHXUSPAIBRCJ

Rotor 2: ABCDEFGHIJKLMNOPQRSTUVWXYZ
         AJDKSIRUXBLHWTMCQGZNPYFVOE

Rotor 3: ABCDEFGHIJKLMNOPQRSTUVWXYZ
         BDFHJLCPRTXVZNYEIWGAKMUSQO

Reflector: ABCDEFGHIJKLMNOPQRSTUVWXYZ
           YRUHQSLDPXNGOKMIEBFZCWVJAT

Step 1
|
ABCDEFGHIJKLMNOPQRSTUVWXYZ
DFHJLCPRTXVZNYEIWGAKMUSQOB (1)
ABCDEFGHIJKLMNOPQRSTUVWXYZ
ZABCDEFGHIJKLMNOPQRSTUVWXY
  |
ABCDEFGHIJKLMNOPQRSTUVWXYZ
AJDKSIRUXBLHWTMCQGZNPYFVOE
   |
ABCDEFGHIJKLMNOPQRSTUVWXYZ
EKMFLGDQVZNTOWYHXUSPAIBRCJ

Step2
 |
ABCDEFGHIJKLMNOPQRSTUVWXYZ
FHJLCPRTXVZNYEIWGAKMUSQOBD (2)
ABCDEFGHIJKLMNOPQRSTUVWXYZ
YZABCDEFGHIJKLMNOPQRSTUVWX
     |
ABCDEFGHIJKLMNOPQRSTUVWXYZ
AJDKSIRUXBLHWTMCQGZNPYFVOE
        |
ABCDEFGHIJKLMNOPQRSTUVWXYZ
EKMFLGDQVZNTOWYHXUSPAIBRCJ
-}
