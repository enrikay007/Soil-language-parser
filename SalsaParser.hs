--
-- Skeleton for Salsa parser

-- To be used at the exam for Advanced Programming, B1-2013

--



module SalsaParser (
program,

defcoms,

defcom,
 
definition,

command,

vidents,

sidents,

--videntsopt,

--sidentsopt,

pos,

colour,

integer,

sident,

vident,

prim,

expr,

expr1,

parseString,

parseFile
)

where


import SalsaAst

import SimpleParse

import Data.Char(isUpper, isLower, isAlphaNum, isDigit, isSpace,isAlpha)




data Error=Error deriving Show

						

colour :: Parser Colour
colour = (do f <- many1 space

             ca <- symbol "blue"

             return Blue)
  
     <|>
         
            (do 
                cs <- symbol "plum"

                return Plum )

       <|>
  
             (do cd <- symbol "red"

                 return Red )

       <|>
             (do cf <- symbol "green"

                 return Green )
     
       <|>   (do cg <- symbol "orange"

                 return Orange )

			 
	


integer :: Parser Integer
integer = (do a <- many space
              str <- a_token num

              return (read str))



          
	

expr1 :: Parser Expr
expr1 =  expr2 
  where expr2 = (do  v <- prim
                     return v)



expr,t, f :: Parser Expr
expr = do tv <- t
          
          eopt tv 
	   


eopt :: Expr -> Parser Expr
eopt inval = (do symbol "+"

                 tv <- t
 
                 eopt(Plus inval tv))

             <|> return inval


t =    do fv <- f

          topt fv


topt :: Expr -> Parser Expr
topt inval =
   
        (do symbol "-"

            fv <- f

            topt (Minus inval fv))
 
    <|> return inval


f = prim -- <++ expr1

prim :: Parser Expr
prim = do i <- integer

          return (Const i)
    
   <|>
       (do 
		  
            c <- sident
            symbol "."
            symbol "x"

            return (Xproj c))

    <|>(do
 
	    c <- sident

	    symbol "."
            symbol "y"
            return (Yproj c))

    <|> (do

            schar '('

            f <- expr

            schar ')'

            return (f))


vident :: Parser Ident
vident = do a <- many space
	    c <- satisfy isUpper
            cs <- many1( digit <|> letter <|> underScore)

            return (c:cs)

sident :: Parser Ident
sident = do a <- many space
            c <- satisfy isLower
            cs <- many1( digit <|> letter <|> underScore)

            if elem (c:cs) keywords then reject
 
			   else return (c:cs)

                           where keywords = ["viewdef", "rectangle", "circle", "group", "view", "blue", "plum","red","green","orange"] 

num = many1 digit



digit:: Parser Char
digit =satisfy isDigit




a_token :: Parser a -> Parser a
a_token p = do {a <- p; spaces; return a}



letter :: Parser Char
letter = satisfy isAlphaNum



underScore :: Parser Char
underScore = do 
	
                c <- char '_'
	
                return c

	


pos :: Parser Pos 
pos = ( do  schar '('

            e1 <- expr

            schar ','

            e2 <- expr

            schar ')'

            return (Abs e1 e2))

	<|>

         (do symbol "+"
 
             schar '('

             e1 <- expr

             schar ','

             e2 <- expr

             schar ')'

             return (Rel e1 e2))





sidents :: Parser [Ident]
sidents = (do a <- many space
              b <- many1 sident
              return b)

	


vidents :: Parser [Ident]
vidents = (do a <- many space
              b <- many1 vident
              return b)
		
	

bop :: (a -> b -> a) -> Parser a -> Parser () -> Parser b -> Parser a
bop f l op r = do x <- l

                  bop' x

  where bop' x = (do op
 
                     y <- r

                     bop' $ f x y)

                 <|> return x

		 
com :: Parser Command
com =  (do v <- sidents

           symbol "->"

           uv <- pos
 
           return (Move v uv))



command,c2,c3:: Parser Command
command =   do tv <- c2

               e1 tv


e1 :: Command -> Parser Command
e1 inval = (do symbol "@"

               tv <- vident
 
               e1(At inval tv))

             <|> return inval



c2 =    do fv <- c3

           t1 fv


t1 :: Command -> Parser Command
t1 inval = (do symbol "||"

               fv <- command

               t1 (Par inval fv))
 
    <|> return inval


c3 = c0 -- <|> pr



c0 :: Parser Command
c0 = (do symbol "{"

         f <- command

         symbol "}"

         return (f))
 
 <++
 
      (do v <- sidents

          symbol "->"

          uv <- pos
 
          return (Move v uv))

	

definition :: Parser Definition
definition = view1 <|> rectangle <|> circle <|> view <|> group

  where view1      = (do b <-symbol "viewdef"
                         a <- vident
                         c <- expr
                         d <- expr
                         return (Viewdef a c d))

        rectangle  = (do g1 <- symbol "rectangle"
                         q1 <- sident
                         y1 <- expr
                         y2 <- expr
                         y3 <- expr
                         y4 <- expr
                         r1 <- colour
                         return (Rectangle q1 y1 y2 y3 y4 r1))

        circle = (do ro <- symbol "circle"
                     u1 <- sident
                     h1 <- expr
                     h2 <- expr
                     h3 <- expr
                     l1 <- colour
                     return (Circle u1 h1 h2 h3 l1))
             
        view = (do rp <- symbol "view"
                   v1 <- vident
                   return (View v1))


        group = (do gl <- symbol "group"
                    z1 <- vident
                    symbol "["
                    e<- vidents
                    symbol "]"
                    return (Group z1 e))		
  






defcom :: Parser DefCom
defcom = command' <|> define'

    where define' = do df <- definition

                       return (Def df)

          command' = do c <- command

                        return (Com c)



defcoms :: Parser [DefCom]
defcoms = do a <- defcom

             b <- defsopt

             return (a:b)



defsopt :: Parser [DefCom]
defsopt = do 
              a <- defcoms
 
              return a

          <|>
          
               return []





program :: Parser [DefCom]
program = defcoms




parseString :: String -> Either Error Program
parseString s =
	case parseEof program s of

	     []->Left Error
             [(p,_)]->Right p



parseFile :: FilePath -> IO ( Either Error Program)
parseFile filePath =
           do f <- readFile filePath

              let p = parseString f in return p
	 