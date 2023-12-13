{-# OPTIONS_GHC -w #-}
module Parser where

import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t4 t5
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,209) ([32,9686,256,0,0,0,49152,45055,75,0,0,0,0,128,38744,1024,0,0,0,0,0,4,32776,2421,0,256,0,0,61441,60415,22,0,2048,49156,1210,65504,9695,0,0,2048,30080,16393,44032,75,24578,605,16,4843,128,38744,1024,47808,8196,54784,37,45057,302,32776,2421,64,19372,512,23904,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,54784,37,0,0,0,24832,0,0,512,23904,61442,60415,50,0,8,0,12416,0,0,0,0,63488,32255,16393,44032,75,0,32,16,4843,128,38744,0,0,0,0,0,0,3104,0,0,0,4096,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","Exp","Type","num","\"+\"","\"-\"","\"*\"","\"&&\"","\"||\"","\"<\"","\">\"","\"==\"","\"<=\"","\">=\"","\"!=\"","true","false","then","if","else","while","var","'\\\\'","\"->\"","'('","')'","'='","let","in","Bool","Num","':'","%eof"]
        bit_start = st * 35
        bit_end = (st + 1) * 35
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..34]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (6) = happyShift action_2
action_0 (18) = happyShift action_4
action_0 (19) = happyShift action_5
action_0 (21) = happyShift action_6
action_0 (23) = happyShift action_7
action_0 (24) = happyShift action_8
action_0 (25) = happyShift action_9
action_0 (27) = happyShift action_10
action_0 (30) = happyShift action_11
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (6) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (6) = happyShift action_2
action_3 (7) = happyShift action_18
action_3 (8) = happyShift action_19
action_3 (9) = happyShift action_20
action_3 (10) = happyShift action_21
action_3 (11) = happyShift action_22
action_3 (12) = happyShift action_23
action_3 (13) = happyShift action_24
action_3 (14) = happyShift action_25
action_3 (15) = happyShift action_26
action_3 (16) = happyShift action_27
action_3 (17) = happyShift action_28
action_3 (18) = happyShift action_4
action_3 (19) = happyShift action_5
action_3 (21) = happyShift action_6
action_3 (23) = happyShift action_7
action_3 (24) = happyShift action_8
action_3 (25) = happyShift action_9
action_3 (27) = happyShift action_10
action_3 (30) = happyShift action_11
action_3 (35) = happyAccept
action_3 (4) = happyGoto action_17
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_2

action_5 _ = happyReduce_3

action_6 (6) = happyShift action_2
action_6 (18) = happyShift action_4
action_6 (19) = happyShift action_5
action_6 (21) = happyShift action_6
action_6 (23) = happyShift action_7
action_6 (24) = happyShift action_8
action_6 (25) = happyShift action_9
action_6 (27) = happyShift action_10
action_6 (30) = happyShift action_11
action_6 (4) = happyGoto action_16
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (6) = happyShift action_15
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_17

action_9 (24) = happyShift action_14
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (6) = happyShift action_2
action_10 (18) = happyShift action_4
action_10 (19) = happyShift action_5
action_10 (21) = happyShift action_6
action_10 (23) = happyShift action_7
action_10 (24) = happyShift action_8
action_10 (25) = happyShift action_9
action_10 (27) = happyShift action_10
action_10 (30) = happyShift action_11
action_10 (4) = happyGoto action_13
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (24) = happyShift action_12
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (29) = happyShift action_44
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (6) = happyShift action_2
action_13 (7) = happyShift action_18
action_13 (8) = happyShift action_19
action_13 (9) = happyShift action_20
action_13 (10) = happyShift action_21
action_13 (11) = happyShift action_22
action_13 (12) = happyShift action_23
action_13 (13) = happyShift action_24
action_13 (14) = happyShift action_25
action_13 (15) = happyShift action_26
action_13 (16) = happyShift action_27
action_13 (17) = happyShift action_28
action_13 (18) = happyShift action_4
action_13 (19) = happyShift action_5
action_13 (21) = happyShift action_6
action_13 (23) = happyShift action_7
action_13 (24) = happyShift action_8
action_13 (25) = happyShift action_9
action_13 (27) = happyShift action_10
action_13 (28) = happyShift action_43
action_13 (30) = happyShift action_11
action_13 (4) = happyGoto action_17
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (34) = happyShift action_42
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (6) = happyShift action_2
action_15 (18) = happyShift action_4
action_15 (19) = happyShift action_5
action_15 (21) = happyShift action_6
action_15 (23) = happyShift action_7
action_15 (24) = happyShift action_8
action_15 (25) = happyShift action_9
action_15 (27) = happyShift action_10
action_15 (30) = happyShift action_11
action_15 (4) = happyGoto action_41
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (6) = happyShift action_2
action_16 (7) = happyShift action_18
action_16 (8) = happyShift action_19
action_16 (9) = happyShift action_20
action_16 (10) = happyShift action_21
action_16 (11) = happyShift action_22
action_16 (12) = happyShift action_23
action_16 (13) = happyShift action_24
action_16 (14) = happyShift action_25
action_16 (15) = happyShift action_26
action_16 (16) = happyShift action_27
action_16 (17) = happyShift action_28
action_16 (18) = happyShift action_4
action_16 (19) = happyShift action_5
action_16 (20) = happyShift action_40
action_16 (21) = happyShift action_6
action_16 (23) = happyShift action_7
action_16 (24) = happyShift action_8
action_16 (25) = happyShift action_9
action_16 (27) = happyShift action_10
action_16 (30) = happyShift action_11
action_16 (4) = happyGoto action_17
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (6) = happyShift action_2
action_17 (7) = happyShift action_18
action_17 (8) = happyShift action_19
action_17 (9) = happyShift action_20
action_17 (10) = happyShift action_21
action_17 (11) = happyShift action_22
action_17 (12) = happyShift action_23
action_17 (13) = happyShift action_24
action_17 (14) = happyShift action_25
action_17 (15) = happyShift action_26
action_17 (16) = happyShift action_27
action_17 (17) = happyShift action_28
action_17 (18) = happyShift action_4
action_17 (19) = happyShift action_5
action_17 (21) = happyShift action_6
action_17 (23) = happyShift action_7
action_17 (24) = happyShift action_8
action_17 (25) = happyShift action_9
action_17 (27) = happyShift action_10
action_17 (30) = happyShift action_11
action_17 (4) = happyGoto action_17
action_17 _ = happyReduce_19

action_18 (6) = happyShift action_2
action_18 (18) = happyShift action_4
action_18 (19) = happyShift action_5
action_18 (21) = happyShift action_6
action_18 (23) = happyShift action_7
action_18 (24) = happyShift action_8
action_18 (25) = happyShift action_9
action_18 (27) = happyShift action_10
action_18 (30) = happyShift action_11
action_18 (4) = happyGoto action_39
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (6) = happyShift action_2
action_19 (18) = happyShift action_4
action_19 (19) = happyShift action_5
action_19 (21) = happyShift action_6
action_19 (23) = happyShift action_7
action_19 (24) = happyShift action_8
action_19 (25) = happyShift action_9
action_19 (27) = happyShift action_10
action_19 (30) = happyShift action_11
action_19 (4) = happyGoto action_38
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (6) = happyShift action_2
action_20 (18) = happyShift action_4
action_20 (19) = happyShift action_5
action_20 (21) = happyShift action_6
action_20 (23) = happyShift action_7
action_20 (24) = happyShift action_8
action_20 (25) = happyShift action_9
action_20 (27) = happyShift action_10
action_20 (30) = happyShift action_11
action_20 (4) = happyGoto action_37
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (6) = happyShift action_2
action_21 (18) = happyShift action_4
action_21 (19) = happyShift action_5
action_21 (21) = happyShift action_6
action_21 (23) = happyShift action_7
action_21 (24) = happyShift action_8
action_21 (25) = happyShift action_9
action_21 (27) = happyShift action_10
action_21 (30) = happyShift action_11
action_21 (4) = happyGoto action_36
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (6) = happyShift action_2
action_22 (18) = happyShift action_4
action_22 (19) = happyShift action_5
action_22 (21) = happyShift action_6
action_22 (23) = happyShift action_7
action_22 (24) = happyShift action_8
action_22 (25) = happyShift action_9
action_22 (27) = happyShift action_10
action_22 (30) = happyShift action_11
action_22 (4) = happyGoto action_35
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (6) = happyShift action_2
action_23 (18) = happyShift action_4
action_23 (19) = happyShift action_5
action_23 (21) = happyShift action_6
action_23 (23) = happyShift action_7
action_23 (24) = happyShift action_8
action_23 (25) = happyShift action_9
action_23 (27) = happyShift action_10
action_23 (30) = happyShift action_11
action_23 (4) = happyGoto action_34
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (6) = happyShift action_2
action_24 (18) = happyShift action_4
action_24 (19) = happyShift action_5
action_24 (21) = happyShift action_6
action_24 (23) = happyShift action_7
action_24 (24) = happyShift action_8
action_24 (25) = happyShift action_9
action_24 (27) = happyShift action_10
action_24 (30) = happyShift action_11
action_24 (4) = happyGoto action_33
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (6) = happyShift action_2
action_25 (18) = happyShift action_4
action_25 (19) = happyShift action_5
action_25 (21) = happyShift action_6
action_25 (23) = happyShift action_7
action_25 (24) = happyShift action_8
action_25 (25) = happyShift action_9
action_25 (27) = happyShift action_10
action_25 (30) = happyShift action_11
action_25 (4) = happyGoto action_32
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (6) = happyShift action_2
action_26 (18) = happyShift action_4
action_26 (19) = happyShift action_5
action_26 (21) = happyShift action_6
action_26 (23) = happyShift action_7
action_26 (24) = happyShift action_8
action_26 (25) = happyShift action_9
action_26 (27) = happyShift action_10
action_26 (30) = happyShift action_11
action_26 (4) = happyGoto action_31
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (6) = happyShift action_2
action_27 (18) = happyShift action_4
action_27 (19) = happyShift action_5
action_27 (21) = happyShift action_6
action_27 (23) = happyShift action_7
action_27 (24) = happyShift action_8
action_27 (25) = happyShift action_9
action_27 (27) = happyShift action_10
action_27 (30) = happyShift action_11
action_27 (4) = happyGoto action_30
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (6) = happyShift action_2
action_28 (18) = happyShift action_4
action_28 (19) = happyShift action_5
action_28 (21) = happyShift action_6
action_28 (23) = happyShift action_7
action_28 (24) = happyShift action_8
action_28 (25) = happyShift action_9
action_28 (27) = happyShift action_10
action_28 (30) = happyShift action_11
action_28 (4) = happyGoto action_29
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (6) = happyShift action_2
action_29 (7) = happyShift action_18
action_29 (8) = happyShift action_19
action_29 (9) = happyShift action_20
action_29 (10) = happyShift action_21
action_29 (11) = happyShift action_22
action_29 (12) = happyShift action_23
action_29 (13) = happyShift action_24
action_29 (14) = happyShift action_25
action_29 (15) = happyShift action_26
action_29 (16) = happyShift action_27
action_29 (17) = happyShift action_28
action_29 (18) = happyShift action_4
action_29 (19) = happyShift action_5
action_29 (21) = happyShift action_6
action_29 (23) = happyShift action_7
action_29 (24) = happyShift action_8
action_29 (25) = happyShift action_9
action_29 (27) = happyShift action_10
action_29 (30) = happyShift action_11
action_29 (4) = happyGoto action_17
action_29 _ = happyReduce_14

action_30 (6) = happyShift action_2
action_30 (7) = happyShift action_18
action_30 (8) = happyShift action_19
action_30 (9) = happyShift action_20
action_30 (10) = happyShift action_21
action_30 (11) = happyShift action_22
action_30 (12) = happyShift action_23
action_30 (13) = happyShift action_24
action_30 (14) = happyShift action_25
action_30 (15) = happyShift action_26
action_30 (16) = happyShift action_27
action_30 (17) = happyShift action_28
action_30 (18) = happyShift action_4
action_30 (19) = happyShift action_5
action_30 (21) = happyShift action_6
action_30 (23) = happyShift action_7
action_30 (24) = happyShift action_8
action_30 (25) = happyShift action_9
action_30 (27) = happyShift action_10
action_30 (30) = happyShift action_11
action_30 (4) = happyGoto action_17
action_30 _ = happyReduce_13

action_31 (6) = happyShift action_2
action_31 (7) = happyShift action_18
action_31 (8) = happyShift action_19
action_31 (9) = happyShift action_20
action_31 (10) = happyShift action_21
action_31 (11) = happyShift action_22
action_31 (12) = happyShift action_23
action_31 (13) = happyShift action_24
action_31 (14) = happyShift action_25
action_31 (15) = happyShift action_26
action_31 (16) = happyShift action_27
action_31 (17) = happyShift action_28
action_31 (18) = happyShift action_4
action_31 (19) = happyShift action_5
action_31 (21) = happyShift action_6
action_31 (23) = happyShift action_7
action_31 (24) = happyShift action_8
action_31 (25) = happyShift action_9
action_31 (27) = happyShift action_10
action_31 (30) = happyShift action_11
action_31 (4) = happyGoto action_17
action_31 _ = happyReduce_12

action_32 (6) = happyShift action_2
action_32 (7) = happyShift action_18
action_32 (8) = happyShift action_19
action_32 (9) = happyShift action_20
action_32 (10) = happyShift action_21
action_32 (11) = happyShift action_22
action_32 (12) = happyShift action_23
action_32 (13) = happyShift action_24
action_32 (14) = happyShift action_25
action_32 (15) = happyShift action_26
action_32 (16) = happyShift action_27
action_32 (17) = happyShift action_28
action_32 (18) = happyShift action_4
action_32 (19) = happyShift action_5
action_32 (21) = happyShift action_6
action_32 (23) = happyShift action_7
action_32 (24) = happyShift action_8
action_32 (25) = happyShift action_9
action_32 (27) = happyShift action_10
action_32 (30) = happyShift action_11
action_32 (4) = happyGoto action_17
action_32 _ = happyReduce_11

action_33 (6) = happyShift action_2
action_33 (7) = happyShift action_18
action_33 (8) = happyShift action_19
action_33 (9) = happyShift action_20
action_33 (10) = happyShift action_21
action_33 (11) = happyShift action_22
action_33 (12) = happyShift action_23
action_33 (13) = happyShift action_24
action_33 (14) = happyShift action_25
action_33 (15) = happyShift action_26
action_33 (16) = happyShift action_27
action_33 (17) = happyShift action_28
action_33 (18) = happyShift action_4
action_33 (19) = happyShift action_5
action_33 (21) = happyShift action_6
action_33 (23) = happyShift action_7
action_33 (24) = happyShift action_8
action_33 (25) = happyShift action_9
action_33 (27) = happyShift action_10
action_33 (30) = happyShift action_11
action_33 (4) = happyGoto action_17
action_33 _ = happyReduce_10

action_34 (6) = happyShift action_2
action_34 (7) = happyShift action_18
action_34 (8) = happyShift action_19
action_34 (9) = happyShift action_20
action_34 (10) = happyShift action_21
action_34 (11) = happyShift action_22
action_34 (12) = happyShift action_23
action_34 (13) = happyShift action_24
action_34 (14) = happyShift action_25
action_34 (15) = happyShift action_26
action_34 (16) = happyShift action_27
action_34 (17) = happyShift action_28
action_34 (18) = happyShift action_4
action_34 (19) = happyShift action_5
action_34 (21) = happyShift action_6
action_34 (23) = happyShift action_7
action_34 (24) = happyShift action_8
action_34 (25) = happyShift action_9
action_34 (27) = happyShift action_10
action_34 (30) = happyShift action_11
action_34 (4) = happyGoto action_17
action_34 _ = happyReduce_9

action_35 (6) = happyShift action_2
action_35 (7) = happyShift action_18
action_35 (8) = happyShift action_19
action_35 (9) = happyShift action_20
action_35 (10) = happyShift action_21
action_35 (11) = happyShift action_22
action_35 (12) = happyShift action_23
action_35 (13) = happyShift action_24
action_35 (14) = happyShift action_25
action_35 (15) = happyShift action_26
action_35 (16) = happyShift action_27
action_35 (17) = happyShift action_28
action_35 (18) = happyShift action_4
action_35 (19) = happyShift action_5
action_35 (21) = happyShift action_6
action_35 (23) = happyShift action_7
action_35 (24) = happyShift action_8
action_35 (25) = happyShift action_9
action_35 (27) = happyShift action_10
action_35 (30) = happyShift action_11
action_35 (4) = happyGoto action_17
action_35 _ = happyReduce_8

action_36 (6) = happyShift action_2
action_36 (7) = happyShift action_18
action_36 (8) = happyShift action_19
action_36 (9) = happyShift action_20
action_36 (10) = happyShift action_21
action_36 (11) = happyShift action_22
action_36 (12) = happyShift action_23
action_36 (13) = happyShift action_24
action_36 (14) = happyShift action_25
action_36 (15) = happyShift action_26
action_36 (16) = happyShift action_27
action_36 (17) = happyShift action_28
action_36 (18) = happyShift action_4
action_36 (19) = happyShift action_5
action_36 (21) = happyShift action_6
action_36 (23) = happyShift action_7
action_36 (24) = happyShift action_8
action_36 (25) = happyShift action_9
action_36 (27) = happyShift action_10
action_36 (30) = happyShift action_11
action_36 (4) = happyGoto action_17
action_36 _ = happyReduce_7

action_37 (6) = happyShift action_2
action_37 (7) = happyShift action_18
action_37 (8) = happyShift action_19
action_37 (9) = happyShift action_20
action_37 (10) = happyShift action_21
action_37 (11) = happyShift action_22
action_37 (12) = happyShift action_23
action_37 (13) = happyShift action_24
action_37 (14) = happyShift action_25
action_37 (15) = happyShift action_26
action_37 (16) = happyShift action_27
action_37 (17) = happyShift action_28
action_37 (18) = happyShift action_4
action_37 (19) = happyShift action_5
action_37 (21) = happyShift action_6
action_37 (23) = happyShift action_7
action_37 (24) = happyShift action_8
action_37 (25) = happyShift action_9
action_37 (27) = happyShift action_10
action_37 (30) = happyShift action_11
action_37 (4) = happyGoto action_17
action_37 _ = happyReduce_6

action_38 (6) = happyShift action_2
action_38 (7) = happyShift action_18
action_38 (8) = happyShift action_19
action_38 (9) = happyShift action_20
action_38 (10) = happyShift action_21
action_38 (11) = happyShift action_22
action_38 (12) = happyShift action_23
action_38 (13) = happyShift action_24
action_38 (14) = happyShift action_25
action_38 (15) = happyShift action_26
action_38 (16) = happyShift action_27
action_38 (17) = happyShift action_28
action_38 (18) = happyShift action_4
action_38 (19) = happyShift action_5
action_38 (21) = happyShift action_6
action_38 (23) = happyShift action_7
action_38 (24) = happyShift action_8
action_38 (25) = happyShift action_9
action_38 (27) = happyShift action_10
action_38 (30) = happyShift action_11
action_38 (4) = happyGoto action_17
action_38 _ = happyReduce_5

action_39 (6) = happyShift action_2
action_39 (7) = happyShift action_18
action_39 (8) = happyShift action_19
action_39 (9) = happyShift action_20
action_39 (10) = happyShift action_21
action_39 (11) = happyShift action_22
action_39 (12) = happyShift action_23
action_39 (13) = happyShift action_24
action_39 (14) = happyShift action_25
action_39 (15) = happyShift action_26
action_39 (16) = happyShift action_27
action_39 (17) = happyShift action_28
action_39 (18) = happyShift action_4
action_39 (19) = happyShift action_5
action_39 (21) = happyShift action_6
action_39 (23) = happyShift action_7
action_39 (24) = happyShift action_8
action_39 (25) = happyShift action_9
action_39 (27) = happyShift action_10
action_39 (30) = happyShift action_11
action_39 (4) = happyGoto action_17
action_39 _ = happyReduce_4

action_40 (6) = happyShift action_2
action_40 (18) = happyShift action_4
action_40 (19) = happyShift action_5
action_40 (21) = happyShift action_6
action_40 (23) = happyShift action_7
action_40 (24) = happyShift action_8
action_40 (25) = happyShift action_9
action_40 (27) = happyShift action_10
action_40 (30) = happyShift action_11
action_40 (4) = happyGoto action_50
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (6) = happyShift action_2
action_41 (7) = happyShift action_18
action_41 (8) = happyShift action_19
action_41 (9) = happyShift action_20
action_41 (10) = happyShift action_21
action_41 (11) = happyShift action_22
action_41 (12) = happyShift action_23
action_41 (13) = happyShift action_24
action_41 (14) = happyShift action_25
action_41 (15) = happyShift action_26
action_41 (16) = happyShift action_27
action_41 (17) = happyShift action_28
action_41 (18) = happyShift action_4
action_41 (19) = happyShift action_5
action_41 (21) = happyShift action_6
action_41 (23) = happyShift action_7
action_41 (24) = happyShift action_8
action_41 (25) = happyShift action_9
action_41 (27) = happyShift action_10
action_41 (30) = happyShift action_11
action_41 (4) = happyGoto action_17
action_41 _ = happyReduce_16

action_42 (27) = happyShift action_47
action_42 (32) = happyShift action_48
action_42 (33) = happyShift action_49
action_42 (5) = happyGoto action_46
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_20

action_44 (6) = happyShift action_2
action_44 (18) = happyShift action_4
action_44 (19) = happyShift action_5
action_44 (21) = happyShift action_6
action_44 (23) = happyShift action_7
action_44 (24) = happyShift action_8
action_44 (25) = happyShift action_9
action_44 (27) = happyShift action_10
action_44 (30) = happyShift action_11
action_44 (4) = happyGoto action_45
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (6) = happyShift action_2
action_45 (7) = happyShift action_18
action_45 (8) = happyShift action_19
action_45 (9) = happyShift action_20
action_45 (10) = happyShift action_21
action_45 (11) = happyShift action_22
action_45 (12) = happyShift action_23
action_45 (13) = happyShift action_24
action_45 (14) = happyShift action_25
action_45 (15) = happyShift action_26
action_45 (16) = happyShift action_27
action_45 (17) = happyShift action_28
action_45 (18) = happyShift action_4
action_45 (19) = happyShift action_5
action_45 (21) = happyShift action_6
action_45 (23) = happyShift action_7
action_45 (24) = happyShift action_8
action_45 (25) = happyShift action_9
action_45 (27) = happyShift action_10
action_45 (30) = happyShift action_11
action_45 (31) = happyShift action_54
action_45 (4) = happyGoto action_17
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (26) = happyShift action_53
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (27) = happyShift action_47
action_47 (32) = happyShift action_48
action_47 (33) = happyShift action_49
action_47 (5) = happyGoto action_52
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_22

action_49 _ = happyReduce_23

action_50 (6) = happyShift action_2
action_50 (7) = happyShift action_18
action_50 (8) = happyShift action_19
action_50 (9) = happyShift action_20
action_50 (10) = happyShift action_21
action_50 (11) = happyShift action_22
action_50 (12) = happyShift action_23
action_50 (13) = happyShift action_24
action_50 (14) = happyShift action_25
action_50 (15) = happyShift action_26
action_50 (16) = happyShift action_27
action_50 (17) = happyShift action_28
action_50 (18) = happyShift action_4
action_50 (19) = happyShift action_5
action_50 (21) = happyShift action_6
action_50 (22) = happyShift action_51
action_50 (23) = happyShift action_7
action_50 (24) = happyShift action_8
action_50 (25) = happyShift action_9
action_50 (27) = happyShift action_10
action_50 (30) = happyShift action_11
action_50 (4) = happyGoto action_17
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (6) = happyShift action_2
action_51 (18) = happyShift action_4
action_51 (19) = happyShift action_5
action_51 (21) = happyShift action_6
action_51 (23) = happyShift action_7
action_51 (24) = happyShift action_8
action_51 (25) = happyShift action_9
action_51 (27) = happyShift action_10
action_51 (30) = happyShift action_11
action_51 (4) = happyGoto action_58
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (26) = happyShift action_57
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (6) = happyShift action_2
action_53 (18) = happyShift action_4
action_53 (19) = happyShift action_5
action_53 (21) = happyShift action_6
action_53 (23) = happyShift action_7
action_53 (24) = happyShift action_8
action_53 (25) = happyShift action_9
action_53 (27) = happyShift action_10
action_53 (30) = happyShift action_11
action_53 (4) = happyGoto action_56
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (6) = happyShift action_2
action_54 (18) = happyShift action_4
action_54 (19) = happyShift action_5
action_54 (21) = happyShift action_6
action_54 (23) = happyShift action_7
action_54 (24) = happyShift action_8
action_54 (25) = happyShift action_9
action_54 (27) = happyShift action_10
action_54 (30) = happyShift action_11
action_54 (4) = happyGoto action_55
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (6) = happyShift action_2
action_55 (7) = happyShift action_18
action_55 (8) = happyShift action_19
action_55 (9) = happyShift action_20
action_55 (10) = happyShift action_21
action_55 (11) = happyShift action_22
action_55 (12) = happyShift action_23
action_55 (13) = happyShift action_24
action_55 (14) = happyShift action_25
action_55 (15) = happyShift action_26
action_55 (16) = happyShift action_27
action_55 (17) = happyShift action_28
action_55 (18) = happyShift action_4
action_55 (19) = happyShift action_5
action_55 (21) = happyShift action_6
action_55 (23) = happyShift action_7
action_55 (24) = happyShift action_8
action_55 (25) = happyShift action_9
action_55 (27) = happyShift action_10
action_55 (30) = happyShift action_11
action_55 (4) = happyGoto action_17
action_55 _ = happyReduce_21

action_56 (6) = happyShift action_2
action_56 (7) = happyShift action_18
action_56 (8) = happyShift action_19
action_56 (9) = happyShift action_20
action_56 (10) = happyShift action_21
action_56 (11) = happyShift action_22
action_56 (12) = happyShift action_23
action_56 (13) = happyShift action_24
action_56 (14) = happyShift action_25
action_56 (15) = happyShift action_26
action_56 (16) = happyShift action_27
action_56 (17) = happyShift action_28
action_56 (18) = happyShift action_4
action_56 (19) = happyShift action_5
action_56 (21) = happyShift action_6
action_56 (23) = happyShift action_7
action_56 (24) = happyShift action_8
action_56 (25) = happyShift action_9
action_56 (27) = happyShift action_10
action_56 (30) = happyShift action_11
action_56 (4) = happyGoto action_17
action_56 _ = happyReduce_18

action_57 (27) = happyShift action_47
action_57 (32) = happyShift action_48
action_57 (33) = happyShift action_49
action_57 (5) = happyGoto action_59
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (6) = happyShift action_2
action_58 (7) = happyShift action_18
action_58 (8) = happyShift action_19
action_58 (9) = happyShift action_20
action_58 (10) = happyShift action_21
action_58 (11) = happyShift action_22
action_58 (12) = happyShift action_23
action_58 (13) = happyShift action_24
action_58 (14) = happyShift action_25
action_58 (15) = happyShift action_26
action_58 (16) = happyShift action_27
action_58 (17) = happyShift action_28
action_58 (18) = happyShift action_4
action_58 (19) = happyShift action_5
action_58 (21) = happyShift action_6
action_58 (23) = happyShift action_7
action_58 (24) = happyShift action_8
action_58 (25) = happyShift action_9
action_58 (27) = happyShift action_10
action_58 (30) = happyShift action_11
action_58 (4) = happyGoto action_17
action_58 _ = happyReduce_15

action_59 (28) = happyShift action_60
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_24

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn4
		 (Num happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 _
	 =  HappyAbsSyn4
		 (BTrue
	)

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 _
	 =  HappyAbsSyn4
		 (BFalse
	)

happyReduce_4 = happySpecReduce_3  4 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Add happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  4 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  4 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Mult happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  4 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (And happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  4 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Or happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  4 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Menor happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  4 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Maior happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  4 happyReduction_11
happyReduction_11 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Iguais happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  4 happyReduction_12
happyReduction_12 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (MenorIgual happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  4 happyReduction_13
happyReduction_13 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (MaiorIgual happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  4 happyReduction_14
happyReduction_14 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Diferente happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 6 4 happyReduction_15
happyReduction_15 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_3  4 happyReduction_16
happyReduction_16 (HappyAbsSyn4  happy_var_3)
	(HappyTerminal (TokenNum happy_var_2))
	_
	 =  HappyAbsSyn4
		 (While happy_var_2 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  4 happyReduction_17
happyReduction_17 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn4
		 (Var happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happyReduce 6 4 happyReduction_18
happyReduction_18 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Lam happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_2  4 happyReduction_19
happyReduction_19 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (App happy_var_1 happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  4 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Paren happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 6 4 happyReduction_21
happyReduction_21 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Let happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_1  5 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn5
		 (TBool
	)

happyReduce_23 = happySpecReduce_1  5 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn5
		 (TNum
	)

happyReduce_24 = happyReduce 5 5 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (TFun happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 35 35 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenNum happy_dollar_dollar -> cont 6;
	TokenAdd -> cont 7;
	TokenSub -> cont 8;
	TokenMult -> cont 9;
	TokenAnd -> cont 10;
	TokenOr -> cont 11;
	TokenMenor -> cont 12;
	TokenMaior -> cont 13;
	TokenIguais -> cont 14;
	TokenMenorIgual -> cont 15;
	TokenMaiorIgual -> cont 16;
	TokenDiferente -> cont 17;
	TokenTrue -> cont 18;
	TokenFalse -> cont 19;
	TokenThen -> cont 20;
	TokenIf -> cont 21;
	TokenElse -> cont 22;
	TokenWhile -> cont 23;
	TokenVar happy_dollar_dollar -> cont 24;
	TokenLam -> cont 25;
	TokenArrow -> cont 26;
	TokenLParen -> cont 27;
	TokenRParen -> cont 28;
	TokenEq -> cont 29;
	TokenLet -> cont 30;
	TokenIn -> cont 31;
	TokenBoolean -> cont 32;
	TokenNumber -> cont 33;
	TokenColon -> cont 34;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 35 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parserError tokens)
parser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parserError :: [Token] -> a
parserError _ = error "Syntaz error!"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
