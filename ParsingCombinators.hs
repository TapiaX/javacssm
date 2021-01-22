module ParsingCombinators where

import Prelude hiding ( (<*>), (<$>), (*>), (<*), (<$) )

type Parser s r = [s] -> [(r, [s])]

symbol :: Eq s => s -> Parser s s
symbol _               []                               = []
symbol expectedSymbol (firstSymbolInput:remainingInput)
  | expectedSymbol == firstSymbolInput = [(firstSymbolInput,remainingInput)]
  | otherwise                          = []

succeed :: a -> Parser s a
succeed r xs = [(r,xs)]

(<|>) :: Parser s r -> Parser s r -> Parser s r
(p <|> q) s = p s ++ q s
infixr 4 <|>

(<$>) :: (a -> b) -> Parser s a -> Parser s b
(f <$> p) s =  map (\(r,resto) -> (f r,resto)) (p s)
infixl 7 <$>

(<$) :: a -> Parser s b -> Parser s a
(f <$ p) s =  map (\(r,resto) -> (f,resto)) (p s)
infixl 7 <$

(<*>) :: Parser s (b -> a) -> Parser s b -> Parser s a
(p <*> q) s = [ (pr qr,rqs) | (pr, rps) <- p s,  (qr, rqs) <- q rps ]
infixl 6 <*>

(*>) :: Parser s a -> Parser s b -> Parser s b
(p *> q) s = [ (qr,rqs) 
             | (pr, rps) <- p s
             , (qr, rqs) <- q rps ]
infixl 8 *>

(<*) :: Parser s a -> Parser s b -> Parser s a
(p <* q) s = [ (pr,rqs) | (pr, rps) <- p s,  (qr, rqs) <- q rps ]
infixl 9 <*

pMany :: Parser s a -> Parser s [a]
pMany p = list <$> p <*> pMany p 
       <|> succeed []

pMany1 :: Parser s a -> Parser s [a]
pMany1 p = list <$> p <*> pMany p

pFoldr :: (a -> b -> b) -> b -> Parser s a -> Parser s b
pFoldr f v p = f <$> p <*> (pFoldr f v p)
            <|> succeed v

pFoldr1 :: (a -> a -> a) ->Parser s a -> Parser s a
pFoldr1 f p = f <$> p <*> (pFoldr1 f p)
           <|> p

list :: a  -> [a] -> [a]
list x xs = x:xs

option :: Parser s a -> a -> Parser s a
option p d = p <|> succeed d

--Combinators for expressions
chainr :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainr pe po = h <$> pMany (j <$> pe <*> po) <*> pe
    where j x op = (x `op`)
          h fs x = foldr ($) x fs

choice :: [Parser s a] -> Parser s a
choice [p] = p
choice (p:ps) = p <|> choice ps

pListOf :: Parser s a -> Parser s b -> Parser s [a]
pListOf p s = list <$> p <*> pMany ( s *> p )

--pListOfFoldr :: (a -> b -> b) -> b -> Parser s a -> Parser s1 c -> Parser s b
pListOfFoldr f v p s =  f <$> p <*> pFoldr f v ( s *> p )
                    <|> succeed v