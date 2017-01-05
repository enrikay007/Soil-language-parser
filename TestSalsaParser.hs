import Test.HUnit
import SalsaParser
import SalsaAst
import SimpleParse

-- integer
t_integer = parseEof integer
test_integer_01 = TestCase $ assertEqual "integer 1" ([(1,"")]) (t_integer "1")
test_integer_02 = TestCase $ assertEqual "integer 2" ([(1,"")]) (t_integer "01")
test_integer_03 = TestCase $ assertEqual "integer 3" ([(2,"")]) (t_integer "2")
test_integer_04 = TestCase $ assertEqual "integer 4" ([(2,"")]) (t_integer "02")
test_integer_05 = TestCase $ assertEqual "integer 5" ([(123,"")]) (t_integer "123")
test_integer_06 = TestCase $ assertEqual "integer 6" ([(1233,"")]) (t_integer "1233")
test_integer_07 = TestCase $ assertEqual "integer 7" ([(134,"")]) (t_integer "134")
test_integer_08 = TestCase $ assertEqual "integer 8" ([(15242,"")]) (t_integer "15242")
test_integer_09 = TestCase $ assertEqual "integer 9" ([(1,"")]) (t_integer "0001")
test_integer_10 = TestCase $ assertEqual "integer 10" ([(1000,"")]) (t_integer "1000")
test_integer_11 = TestCase $ assertEqual "integer 11" ([(11010,"")]) (t_integer "11010")
test_integer_12 = TestCase $ assertEqual "integer 12" ([(0,"")]) (t_integer "000000")

-- vident
t_vident = parseEof vident
test_vident_01 = TestCase $ assertEqual "vident 1" ([("Pa","")]) (t_vident "Pa")
test_vident_02 = TestCase $ assertEqual "vident 2" ([("PP","")]) (t_vident "PP")
test_vident_03 = TestCase $ assertEqual "vident 3" ([("Papa","")]) (t_vident "Papa")
test_vident_04 = TestCase $ assertEqual "vident 4" ([("Ka","")]) (t_vident "Ka")
test_vident_05 = TestCase $ assertEqual "vident 5" ([("Tak","")]) (t_vident "Tak")
test_vident_06 = TestCase $ assertEqual "vident 6" ([("Pa_1",""),("Pa_1","")]) (t_vident "Pa_1")
test_vident_07 = TestCase $ assertEqual "vident 7" ([("Pa_a","")]) (t_vident "Pa_a")
test_vident_08 = TestCase $ assertEqual "vident 8" ([("Pair","")]) (t_vident "Pair")
test_vident_09 = TestCase $ assertEqual "vident 9" ([("Pakl","")]) (t_vident "Pakl")
test_vident_10 = TestCase $ assertEqual "vident 10" ([("AAPa","")]) (t_vident "AAPa")
test_vident_11 = TestCase $ assertEqual "vident 11" ([("PAAA","")]) (t_vident "PAAA")
test_vident_12 = TestCase $ assertEqual "vident 12" ([("Ap","")]) (t_vident "Ap")
test_vident_13 = TestCase $ assertEqual "vident 13" ([("ApRocks","")]) (t_vident "ApRocks")

-- sident
t_sident = parseEof sident
test_sident_01 = TestCase $ assertEqual "sident 1" ([("aa","")]) (t_sident "aa")
test_sident_02 = TestCase $ assertEqual "sident 2" ([("aP","")]) (t_sident "aP")
test_sident_03 = TestCase $ assertEqual "sident 3" ([("aapa","")]) (t_sident "aapa")
test_sident_04 = TestCase $ assertEqual "sident 4" ([("aa","")]) (t_sident "aa")
test_sident_05 = TestCase $ assertEqual "sident 5" ([("tak","")]) (t_sident "tak")
test_sident_06 = TestCase $ assertEqual "sident 6" ([("aa_1",""),("aa_1","")]) (t_sident "aa_1")
test_sident_07 = TestCase $ assertEqual "sident 7" ([("pa_a","")]) (t_sident "pa_a")
test_sident_08 = TestCase $ assertEqual "sident 8" ([("pair","")]) (t_sident "pair")
test_sident_09 = TestCase $ assertEqual "sident 9" ([("pakl","")]) (t_sident "pakl")
test_sident_10 = TestCase $ assertEqual "sident 10" ([("papa","")]) (t_sident "papa")
test_sident_11 = TestCase $ assertEqual "sident 11" ([("paPa","")]) (t_sident "paPa")
test_sident_12 = TestCase $ assertEqual "sident 12" ([("parse","")]) (t_sident "parse")
test_sident_13 = TestCase $ assertEqual "sident 13" ([("apRocks","")]) (t_sident "apRocks")

--colour
t_colour = parseEof colour
test_colour_01 = TestCase $ assertEqual "colour 1" ([(Blue,"")]) (t_colour " blue")
test_colour_02 = TestCase $ assertEqual "colour 2" ([(Plum,"")]) (t_colour " plum")
test_colour_03 = TestCase $ assertEqual "colour 3" ([(Red,"")]) (t_colour " red")
test_colour_04 = TestCase $ assertEqual "colour 4" ([(Green,"")]) (t_colour " green")
test_colour_05 = TestCase $ assertEqual "colour 5" ([(Orange,"")]) (t_colour " orange")

--Prim
t_prim = parseEof prim
test_prim_01 = TestCase $ assertEqual "prim 1" ([(Const 1,"")]) (t_prim "1")
test_prim_02 = TestCase $ assertEqual "prim 2" ([(Const 12,"")]) (t_prim "12")
test_prim_03 = TestCase $ assertEqual "prim 3" ([(Const 101,"")]) (t_prim "101")
test_prim_04 = TestCase $ assertEqual "prim 4" ([(Const 1000,"")]) (t_prim "1000")
test_prim_05 = TestCase $ assertEqual "prim 5" ([(Xproj "pp","")]) (t_prim "pp.x")
test_prim_06 = TestCase $ assertEqual "prim 6" ([(Yproj "pp","")]) (t_prim "pp.y")
test_prim_07 = TestCase $ assertEqual "prim 7" ([(Const 1,"")]) (t_prim "(1)")
test_prim_08 = TestCase $ assertEqual "prim 8" ([(Plus (Const 1) (Const 1),"")]) (t_prim "(1+1)")
test_prim_09 = TestCase $ assertEqual "prim 9" ([(Minus (Const 1) (Const 1),"")]) (t_prim "(1-1)")
test_prim_10 = TestCase $ assertEqual "prim 10" ([(Const 1121,"")]) (t_prim "1121")

-- Expr
t_expr = parseEof expr
test_expr_01 = TestCase $ assertEqual "expr 1" ([(Const 1,"")]) (t_expr "1")
test_expr_02 = TestCase $ assertEqual "expr 2" ([(Const 12,"")]) (t_expr "12")
test_expr_03 = TestCase $ assertEqual "expr 3" ([(Const 101,"")]) (t_expr "101")
test_expr_04 = TestCase $ assertEqual "expr 4" ([(Const 1000,"")]) (t_expr "1000")
test_expr_05 = TestCase $ assertEqual "expr 5" ([(Xproj "pp","")]) (t_expr "pp.x")
test_expr_06 = TestCase $ assertEqual "expr 6" ([(Yproj "pp","")]) (t_expr "pp.y")
test_expr_07 = TestCase $ assertEqual "expr 7" ([(Const 1,"")]) (t_expr "(1)")
test_expr_08 = TestCase $ assertEqual "expr 8" ([(Plus (Const 1) (Const 1),"")]) (t_expr "(1+1)")
test_expr_09 = TestCase $ assertEqual "expr 9" ([(Minus (Const 1) (Const 1),"")]) (t_expr "(1-1)")
test_expr_10 = TestCase $ assertEqual "expr 10" ([(Const 1121,"")]) (t_expr "1121")
test_expr_11 = TestCase $ assertEqual "expr 11" ([(Minus (Minus (Const 1) (Const 1)) (Const 1),"")]) (t_expr "(1-1)-1")
test_expr_12 = TestCase $ assertEqual "expr 12" ([(Plus (Minus (Const 1) (Const 1)) (Const 1),"")]) (t_expr "(1-1)+1")
test_expr_13 = TestCase $ assertEqual "expr 13" ([(Minus (Plus (Const 1) (Const 1)) (Minus (Const 1) (Const 1)),"")]) (t_expr "(1+1)-(1-1)")

-- Pos
t_pos = parseEof pos
test_pos_01 = TestCase $ assertEqual "pos 1" ([(Abs (Const 1) (Const 1),"")]) (t_pos "(1,1)")
test_pos_02 = TestCase $ assertEqual "pos 2" ([(Abs (Minus (Const 1) (Const 1)) (Minus (Const 1) (Const 1)),"")]) (t_pos "(1-1,1-1)")
test_pos_03 = TestCase $ assertEqual "pos 3" ([(Abs (Plus (Minus (Const 1) (Const 1)) (Const 1)) (Minus (Minus (Const 1) (Const 1)) (Const 1)),"")]) (t_pos "(1-1+1,1-1-1)")
test_pos_04 = TestCase $ assertEqual "pos 4" ([(Rel (Const 1) (Const 1),"")]) (t_pos "+(1,1)")
test_pos_05 = TestCase $ assertEqual "pos 5" ([(Abs (Plus (Const 1) (Minus (Const 1) (Const 1))) (Const 1),"")]) (t_pos "(1+(1-1),1)")
test_pos_06 = TestCase $ assertEqual "pos 6" ([(Rel (Plus (Const 1) (Minus (Const 1) (Const 1))) (Const 1),"")]) (t_pos "+(1+(1-1),1)")
test_pos_07 = TestCase $ assertEqual "pos 7" ([(Rel (Plus (Const 1) (Minus (Const 1) (Const 1))) (Plus (Const 1) (Const 1)),"")]) (t_pos "+(1+(1-1),1+1)")

-- Sidents
t_sidents = parseEof sidents
test_sidents_01 = TestCase $ assertEqual "sidents 1"([(["aa"],"")]) (t_sidents "aa")
test_sidents_02 = TestCase $ assertEqual "sidents 2" ([(["aP"],"")]) (t_sidents "aP")
test_sidents_03 = TestCase $ assertEqual "sidents 3" ([(["aapa"],""),(["aa","pa"],"")]) (t_sidents "aapa")
test_sidents_04 = TestCase $ assertEqual "sidents 4" ([(["aaaa"],""),(["aa","aa"],"")]) (t_sidents "aaaa")
test_sidents_05 = TestCase $ assertEqual "sidents 5" ([(["aaa"],"")]) (t_sidents "aaa")
test_sidents_06 = TestCase $ assertEqual "sidents 6" ([(["aaa"],"")]) (t_sidents "aaa")
test_sidents_07 = TestCase $ assertEqual "sidents 7" ([(["aBa"],"")]) (t_sidents "aBa")
test_sidents_08 = TestCase $ assertEqual "sidents 8" ([(["aapa"],""),(["aa","pa"],"")]) (t_sidents "aapa")
test_sidents_09 = TestCase $ assertEqual "sidents 9" ([(["aaB"],"")]) (t_sidents "aaB")
test_sidents_10 = TestCase $ assertEqual "sidents 10" ([(["aaq"],"")]) (t_sidents "aaq")

-- vidents
t_vidents = parseEof vidents
test_vidents_01 = TestCase $ assertEqual "vidents 1"([(["Aa"],"")]) (t_vidents "Aa")
test_vidents_02 = TestCase $ assertEqual "vidents 2" ([(["AP"],"")]) (t_vidents "AP")
test_vidents_03 = TestCase $ assertEqual "vidents 3" ([(["AaPa"],""),(["Aa","Pa"],"")]) (t_vidents "AaPa")
test_vidents_04 = TestCase $ assertEqual "vidents 4" ([(["AaAa"],""),(["Aa","Aa"],"")]) (t_vidents "AaAa")
test_vidents_05 = TestCase $ assertEqual "vidents 5" ([(["Aaa"],"")]) (t_vidents "Aaa")
test_vidents_06 = TestCase $ assertEqual "vidents 6" ([(["AaB"],"")]) (t_vidents "AaB")
test_vidents_07 = TestCase $ assertEqual "vidents 7" ([(["ABaaaa"],"")]) (t_vidents "ABaaaa")
test_vidents_08 = TestCase $ assertEqual "vidents 8" ([(["AAPa"],""),(["AA","Pa"],"")]) (t_vidents "AAPa")
test_vidents_09 = TestCase $ assertEqual "vidents 9" ([(["AaB"],"")]) (t_vidents "AaB")
test_vidents_10 = TestCase $ assertEqual "vidents 10" ([(["Aaq"],"")]) (t_vidents "Aaq")

-- command
t_command = parseEof command
test_command_01 = TestCase $ assertEqual "command 1"([(Move ["aP"] (Abs (Const 1) (Const 2)),"")]) (t_command "aP->(1,2)")
test_command_02 = TestCase $ assertEqual "command 2"([(At (Move ["aP"] (Abs (Const 1) (Const 2))) "Ap","")]) (t_command "aP->(1,2)@Ap")
test_command_03 = TestCase $ assertEqual "command 3"([(Move ["aP"] (Abs (Const 13) (Const 42)),"")]) (t_command "aP->(13,42)")
test_command_04 = TestCase $ assertEqual "command 4"([(Par (Move ["aP"] (Abs (Const 1) (Const 2))) (Move ["aP"] (Abs (Const 1) (Const
 2))),"")]) (t_command "aP->(1,2)||aP->(1,2)")

-- Definition 
t_definition = parseEof definition
test_definition_01 = TestCase $ assertEqual "definition 1"([(Viewdef "Ap" (Const 1) (Const 1),"")]) (t_definition "viewdef Ap (1) (1)")
test_definition_02 = TestCase $ assertEqual "definition 2"([(Viewdef "Ap" (Yproj "ap") (Const 1),"")]) (t_definition "viewdef Ap (ap.y) 1 ")
test_definition_03 = TestCase $ assertEqual "definition 3"([(Rectangle "ap" (Const 1) (Const 1) (Const 1) (Const 1) Blue,"")]) (t_definition "rectangle ap (1) (1) (1) (1) blue")
test_definition_04 = TestCase $ assertEqual "definition 4"([(Circle "apple" (Const 1) (Const 1) (Const 1) Green,"")]) (t_definition "circle apple (1) (1) (1) green")
test_definition_05 = TestCase $ assertEqual "definition 5"([(Circle "roundabout" (Const 1) (Const 1) (Const 1) Plum,"")]) (t_definition "circle roundabout (1) (1) (1) plum")
test_definition_06 = TestCase $ assertEqual "definition 6"([(View "AboveScene","")]) (t_definition "view AboveScene")
test_definition_07 = TestCase $ assertEqual "definition 7"([(Group "AboveScene" ["Castle"],"")]) (t_definition "group AboveScene[Castle]")
test_definition_08 = TestCase $ assertEqual "definition 8"([(Group "AboveScene" ["CastleView"],""),(Group "AboveScene" ["Castle","View"],"")]) (t_definition "group AboveScene[CastleView]")
test_definition_09 = TestCase $ assertEqual "definition 9"([(Group "Scene" ["Castle"],"")]) (t_definition "group Scene[Castle]")


-- Defcom
t_defcom = parseEof defcom
test_defcom_01 = TestCase $ assertEqual "defcom 1"([(Com (Par (Move ["move","aP"] (Abs (Const 1) (Const 2))) (Move ["aP"] (Abs (Const 1) (Const 2)))),""),(Com (Par (Move ["mo","ve","aP"] (Abs (Const 1) (Const 2))) (Move ["aP"] (Abs (Const 1) (Const 2)))),"")]) (t_defcom "move aP->(1,2)||aP->(1,2)")
test_defcom_02 = TestCase $ assertEqual "defcom 2"([(Def (Rectangle "ap" (Const 1) (Const 1) (Const 1) (Const 1) Blue),""),(Def (Rectangle "ap" (Const 1) (Const 1) (Const 1) (Const 1) Blue),""),(Def (Rectangle "ap" (Const 1) (Const 1) (Const 1) (Const 1) Blue),""),(Def (Rectangle "ap" (Const 1) (Const 1) (Const 1) (Const 1) Blue),""),(Def (Rectangle "ap" (Const 1) (Const 1) (Const 1) (Const 1) Blue),""),(Def (Rectangle "ap" (Const 1) (Const 1) (Const 1) (Const 1) Blue),""),(Def (Rectangle "ap" (Const 1) (Const 1) (Const 1) (Const 1) Blue),""),(Def (Rectangle "ap" (Const 1) (Const 1) (Const 1) (Const 1) Blue),"")]) (t_defcom "rectangle ap 1 1 1 1 blue")
test_defcom_03 = TestCase $ assertEqual "defcom 3"([(Com (Par (Move ["move","ti"] (Abs (Const 1) (Const 2))) (Move ["aP"] (Abs (Const 1) (Const 2)))),""),(Com (Par (Move ["mo","ve","ti"] (Abs (Const 1) (Const 2))) (Move ["aP"] (Abs (Const 1) (Const 2)))),"")]) (t_defcom "move ti->(1,2)||aP->(1,2)")
test_defcom_04 = TestCase $ assertEqual "defcom 4"([(Com (Par (Move ["move","aP"] (Abs (Const 1) (Const 2))) (Move ["aP"] (Abs (Const 1) (Const 2)))),""),(Com (Par (Move ["mo","ve","aP"] (Abs (Const 1) (Const 2))) (Move ["aP"] (Abs (Const 1) (Const 2)))),"")]) (t_defcom "move aP->(1,2)||aP->(1,2)")

-- Defcoms
t_defcoms = parseEof defcoms
test_defcoms_01 = TestCase $ assertEqual "defcoms 1"([([Com (Par (Move ["move","aP"] (Abs (Const 1) (Const 2))) (Move ["aP"] (Abs (Const 1) (Const 2))))],""),([Com (Par (Move ["mo","ve","aP"] (Abs (Const 1) (Const 2))) (Move ["aP"] (Abs (Const 1) (Const 2))))],"")]) (t_defcoms "move aP->(1,2)||aP->(1,2)")
test_defcoms_02 = TestCase $ assertEqual "defcoms 2"([([Def (Rectangle "ap" (Const 1) (Const 1) (Const 1) (Const 1) Blue)],""),([Def(Rectangle "ap" (Const 1) (Const 1) (Const 1) (Const 1) Blue)],""),([Def (Rectangle "ap" (Const 1) (Const 1) (Const 1) (Const 1) Blue)],""),([Def (Rectangle "ap" (Const 1) (Const 1) (Const 1) (Const 1) Blue)],""),([Def (Rectangle "ap" (Const 1) (Const 1) (Const 1) (Const 1) Blue)],""),([Def (Rectangle "ap" (Const 1) (Const 1) (Const 1) (Const 1) Blue)],""),([Def (Rectangle "ap" (Const 1) (Const 1) (Const 1) (Const 1) Blue)],""),([Def (Rectangle "ap" (Const 1) (Const 1) (Const 1) (Const 1) Blue)],"")]) (t_defcoms "rectangle ap 1 1 1 1 blue")
test_defcoms_03 = TestCase $ assertEqual "defcoms 3"([([Com (Par (Move ["move","ti"] (Abs (Const 1) (Const 2))) (Move ["aP"] (Abs (Const 1) (Const 2))))],""),([Com (Par (Move ["mo","ve","ti"] (Abs (Const 1) (Const 2))) (Move ["aP"] (Abs (Const 1) (Const 2))))],"")]) (t_defcoms "move ti->(1,2)||aP->(1,2)")
test_defcoms_04 = TestCase $ assertEqual "defcoms 4"([([Com (Par (Move ["move","pi"] (Abs (Const 1) (Const 2))) (Move ["aP"] (Abs (Const 1) (Const 4))))],""),([Com (Par (Move ["mo","ve","pi"] (Abs (Const 1) (Const 2))) (Move ["aP"] (Abs (Const 1) (Const 4))))],"")]) (t_defcoms "move pi->(1,2)||aP->(1,4)")

--Program
t_program = parseEof program
test_program_01 = TestCase $ assertEqual "program 1"([([Com (Par (Move ["move","pi"] (Abs (Const 1) (Const 2))) (Move ["aP"] (Abs (Const 1) (Const 4))))],""),([Com (Par (Move ["mo","ve","pi"] (Abs (Const 1) (Const 2))) (Move ["aP"] (Abs (Const 1) (Const 4))))],"")]) (t_program "move pi->(1,2)||aP->(1,4)")

 
-- run tests
tests = TestList [TestLabel "" test_integer_01,
				  TestLabel "" test_integer_02,
				  TestLabel "" test_integer_03,
				  TestLabel "" test_integer_04,
				  TestLabel "" test_integer_05,
				  TestLabel "" test_integer_06,
				  TestLabel "" test_integer_07,
				  TestLabel "" test_integer_08,
				  TestLabel "" test_integer_09,
				  TestLabel "" test_integer_10,
				  TestLabel "" test_integer_11,
				  TestLabel "" test_integer_12,
				  TestLabel "" test_vident_01,
				  TestLabel "" test_vident_02,
				  TestLabel "" test_vident_03,
				  TestLabel "" test_vident_04,
				  TestLabel "" test_vident_05,
				  TestLabel "" test_vident_06,
                  TestLabel "" test_vident_07,
				  TestLabel "" test_vident_08,
				  TestLabel "" test_vident_09,
				  TestLabel "" test_vident_10,
				  TestLabel "" test_vident_11,
				  TestLabel "" test_vident_12,
				  TestLabel "" test_vident_13,
				  TestLabel "" test_sident_01,
				  TestLabel "" test_sident_02,
				  TestLabel "" test_sident_03,
				  TestLabel "" test_sident_04,
				  TestLabel "" test_sident_05,
				  TestLabel "" test_sident_06,
                  TestLabel "" test_sident_07,
				  TestLabel "" test_sident_08,
				  TestLabel "" test_sident_09,
				  TestLabel "" test_sident_10,
				  TestLabel "" test_sident_11,
				  TestLabel "" test_sident_12,
				  TestLabel "" test_sident_13,
				  TestLabel "" test_colour_01,
				  TestLabel "" test_colour_02,
				  TestLabel "" test_colour_03,
				  TestLabel "" test_colour_04,
				  TestLabel "" test_colour_05,
				  TestLabel "" test_prim_01,
				  TestLabel "" test_prim_02,
				  TestLabel "" test_prim_03,
				  TestLabel "" test_prim_04,
				  TestLabel "" test_prim_05,
				  TestLabel "" test_prim_06,
				  TestLabel "" test_prim_07,
				  TestLabel "" test_prim_08,
				  TestLabel "" test_prim_09,
				  TestLabel "" test_prim_10,
				  TestLabel "" test_expr_01,
				  TestLabel "" test_expr_02,
				  TestLabel "" test_expr_03,
				  TestLabel "" test_expr_04,
				  TestLabel "" test_expr_05,
				  TestLabel "" test_expr_06,
				  TestLabel "" test_expr_07,
				  TestLabel "" test_expr_08,
				  TestLabel "" test_expr_09,
				  TestLabel "" test_expr_10,
				  TestLabel "" test_expr_11,
				  TestLabel "" test_expr_12,
				  TestLabel "" test_expr_13,
				  TestLabel "" test_pos_01,
				  TestLabel "" test_pos_02,
				  TestLabel "" test_pos_03,
				  TestLabel "" test_pos_04,
				  TestLabel "" test_pos_05,
				  TestLabel "" test_pos_06,
				  TestLabel "" test_pos_07,
				  TestLabel "" test_sidents_01,
				  TestLabel "" test_sidents_02,
				  TestLabel "" test_sidents_03,
				  TestLabel "" test_sidents_04,
				  TestLabel "" test_sidents_05,
				  TestLabel "" test_sidents_06,
				  TestLabel "" test_sidents_07,
				  TestLabel "" test_sidents_08,
				  TestLabel "" test_sidents_09,
				  TestLabel "" test_sidents_10,
				  TestLabel "" test_vidents_01,
				  TestLabel "" test_vidents_02,
				  TestLabel "" test_vidents_03,
				  TestLabel "" test_vidents_04,
				  TestLabel "" test_vidents_05,
				  TestLabel "" test_vidents_06,
				  TestLabel "" test_vidents_07,
				  TestLabel "" test_vidents_08,
				  TestLabel "" test_vidents_09,
				  TestLabel "" test_vidents_10,
				  TestLabel "" test_command_01,
				  TestLabel "" test_command_02,
				  TestLabel "" test_command_03,
				  TestLabel "" test_command_04,
				  TestLabel "" test_definition_01,
				  TestLabel "" test_definition_02,
				  TestLabel "" test_definition_03,
				  TestLabel "" test_definition_04,
				  TestLabel "" test_definition_05,
				  TestLabel "" test_definition_06,
				  TestLabel "" test_definition_07,
				  TestLabel "" test_definition_08,
				  TestLabel "" test_definition_09,
				  TestLabel "" test_defcom_01,
				  TestLabel "" test_defcom_02,
				  TestLabel "" test_defcom_03,
				  TestLabel "" test_defcom_04,
				  TestLabel "" test_defcoms_01,
				  TestLabel "" test_defcoms_02,
				  TestLabel "" test_defcoms_03,
				  TestLabel "" test_defcoms_04,
				  TestLabel "" test_program_01
				  







				  ]

run = runTestTT tests