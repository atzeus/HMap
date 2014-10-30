

import Data.HMap 

-- type can be inferred.
example :: HKey x String -> HKey x1 Double -> HKey x2 Bool 
            -> String
example name salary female = 
   format a ++ "\n" ++ format b ++ "\n"
   where a = insert name "Edsger" $ 
             insert salary 4450.0 $ 
             insert female False empty
         b = insert name "Ada"    $ 
             insert salary 5000.0 $ 
             insert female True empty
         format x = x ! name ++ 
                    ": salary=" ++ show (x ! salary) ++ 
                    ", female="  ++ show (x ! female)

keyLocal :: String
keyLocal = withKey $ withKey $ withKey example

keyGlobal :: IO String
keyGlobal = 
  do name   <- createKey
     salary <- createKey
     female <- createKey
     return $ example name salary female
                     
main = do print "local"
          putStr keyLocal
          print "global"
          keyGlobal >>= putStr
  
