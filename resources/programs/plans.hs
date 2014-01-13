import Data.List
import Data.Maybe

be :: Eq a => a -> a -> [a] -> Bool
be a b l = ((elemIndex a l)) < ( ( elemIndex b l))

p = ["NE","NS","BE","BS","DSO"]

plans = [[a,b,c,d,e] | a <- p, b <- p, c <- p, d <- p, e <- p, a /= b, a /= c, a /= d, a /= e, b /= c, b /= d, b /= e, c /= d, c /= e, d /= e]

possible = [ p | p <- plans, be "NE" "BE" p, be "NS" "BS" p, be "BS" "DSO" p]

strings = [ "BEGIN \\rightarrow HEEN \\rightarrow "++a++" \\rightarrow "++b++" \\rightarrow "++c++" \\rightarrow "++d++" \\rightarrow "++e++"\\rightarrow LOB \\rightarrow TERUG \\rightarrow EINDE\\\\" | [a,b,c,d,e] <- possible]

main = do
    mapM_ putStrLn strings
