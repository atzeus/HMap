

import Data.HMap 

example name salary female = 
  do putStrLn $ format a
     putStrLn $ format b 
   where a = insert name "Edsger" $ 
             insert salary 4450.0 $ 
             insert female False empty
         b = insert name "Ada"    $ 
             insert salary 5000.0 $ 
             insert female True empty
         format x = x ! name ++ 
                    ": salary=" ++ show (x ! salary) ++ 
                    ", female="  ++ show (x ! female)

main = withKey $ withKey $ withKey example
  
