{-# OPTIONS_GHC -w #-}
module Parser where

import Data.Char
import Common
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29

action_0 (30) = happyShift action_15
action_0 (31) = happyShift action_16
action_0 (32) = happyShift action_17
action_0 (34) = happyShift action_18
action_0 (36) = happyShift action_19
action_0 (37) = happyShift action_20
action_0 (38) = happyShift action_21
action_0 (39) = happyShift action_22
action_0 (40) = happyShift action_23
action_0 (41) = happyShift action_24
action_0 (42) = happyShift action_25
action_0 (52) = happyShift action_26
action_0 (53) = happyShift action_27
action_0 (54) = happyShift action_28
action_0 (55) = happyShift action_29
action_0 (58) = happyShift action_30
action_0 (60) = happyShift action_48
action_0 (8) = happyGoto action_47
action_0 (9) = happyGoto action_6
action_0 (10) = happyGoto action_7
action_0 (11) = happyGoto action_8
action_0 (12) = happyGoto action_9
action_0 (13) = happyGoto action_10
action_0 (14) = happyGoto action_11
action_0 (15) = happyGoto action_12
action_0 (18) = happyGoto action_13
action_0 (19) = happyGoto action_14
action_0 _ = happyFail

action_1 (30) = happyShift action_15
action_1 (31) = happyShift action_16
action_1 (32) = happyShift action_17
action_1 (34) = happyShift action_18
action_1 (36) = happyShift action_19
action_1 (37) = happyShift action_20
action_1 (38) = happyShift action_21
action_1 (39) = happyShift action_22
action_1 (40) = happyShift action_23
action_1 (41) = happyShift action_24
action_1 (42) = happyShift action_25
action_1 (52) = happyShift action_26
action_1 (53) = happyShift action_27
action_1 (54) = happyShift action_28
action_1 (55) = happyShift action_29
action_1 (58) = happyShift action_30
action_1 (60) = happyShift action_31
action_1 (9) = happyGoto action_46
action_1 (10) = happyGoto action_7
action_1 (11) = happyGoto action_8
action_1 (12) = happyGoto action_9
action_1 (13) = happyGoto action_10
action_1 (14) = happyGoto action_11
action_1 (15) = happyGoto action_12
action_1 (18) = happyGoto action_13
action_1 (19) = happyGoto action_14
action_1 _ = happyFail

action_2 (34) = happyShift action_45
action_2 (23) = happyGoto action_44
action_2 _ = happyFail

action_3 (34) = happyShift action_38
action_3 (53) = happyShift action_39
action_3 (63) = happyShift action_40
action_3 (64) = happyShift action_41
action_3 (65) = happyShift action_42
action_3 (66) = happyShift action_43
action_3 (26) = happyGoto action_34
action_3 (27) = happyGoto action_35
action_3 (28) = happyGoto action_36
action_3 (29) = happyGoto action_37
action_3 _ = happyFail

action_4 (34) = happyShift action_33
action_4 (20) = happyGoto action_32
action_4 _ = happyFail

action_5 (30) = happyShift action_15
action_5 (31) = happyShift action_16
action_5 (32) = happyShift action_17
action_5 (34) = happyShift action_18
action_5 (36) = happyShift action_19
action_5 (37) = happyShift action_20
action_5 (38) = happyShift action_21
action_5 (39) = happyShift action_22
action_5 (40) = happyShift action_23
action_5 (41) = happyShift action_24
action_5 (42) = happyShift action_25
action_5 (52) = happyShift action_26
action_5 (53) = happyShift action_27
action_5 (54) = happyShift action_28
action_5 (55) = happyShift action_29
action_5 (58) = happyShift action_30
action_5 (60) = happyShift action_31
action_5 (9) = happyGoto action_6
action_5 (10) = happyGoto action_7
action_5 (11) = happyGoto action_8
action_5 (12) = happyGoto action_9
action_5 (13) = happyGoto action_10
action_5 (14) = happyGoto action_11
action_5 (15) = happyGoto action_12
action_5 (18) = happyGoto action_13
action_5 (19) = happyGoto action_14
action_5 _ = happyFail

action_6 _ = happyReduce_5

action_7 _ = happyReduce_8

action_8 _ = happyReduce_10

action_9 (43) = happyShift action_74
action_9 (44) = happyShift action_75
action_9 (45) = happyShift action_76
action_9 (46) = happyShift action_77
action_9 (47) = happyShift action_78
action_9 (48) = happyShift action_79
action_9 (49) = happyShift action_80
action_9 (50) = happyShift action_81
action_9 (51) = happyShift action_82
action_9 (16) = happyGoto action_72
action_9 (17) = happyGoto action_73
action_9 _ = happyFail

action_10 _ = happyReduce_12

action_11 (30) = happyShift action_15
action_11 (31) = happyShift action_16
action_11 (32) = happyShift action_17
action_11 (34) = happyShift action_18
action_11 (52) = happyShift action_26
action_11 (53) = happyShift action_27
action_11 (54) = happyShift action_28
action_11 (15) = happyGoto action_71
action_11 (19) = happyGoto action_14
action_11 _ = happyReduce_20

action_12 (43) = happyReduce_14
action_12 (44) = happyReduce_14
action_12 (45) = happyReduce_14
action_12 (46) = happyReduce_14
action_12 (47) = happyReduce_14
action_12 (48) = happyReduce_14
action_12 (49) = happyReduce_14
action_12 (50) = happyReduce_14
action_12 (51) = happyReduce_14
action_12 _ = happyReduce_22

action_13 (30) = happyShift action_15
action_13 (31) = happyShift action_16
action_13 (32) = happyShift action_17
action_13 (34) = happyShift action_18
action_13 (36) = happyShift action_19
action_13 (37) = happyShift action_20
action_13 (38) = happyShift action_21
action_13 (39) = happyShift action_22
action_13 (40) = happyShift action_23
action_13 (41) = happyShift action_24
action_13 (42) = happyShift action_25
action_13 (52) = happyShift action_26
action_13 (53) = happyShift action_27
action_13 (54) = happyShift action_28
action_13 (55) = happyShift action_29
action_13 (58) = happyShift action_30
action_13 (60) = happyShift action_31
action_13 (9) = happyGoto action_70
action_13 (10) = happyGoto action_7
action_13 (11) = happyGoto action_8
action_13 (12) = happyGoto action_9
action_13 (13) = happyGoto action_10
action_13 (14) = happyGoto action_11
action_13 (15) = happyGoto action_12
action_13 (18) = happyGoto action_13
action_13 (19) = happyGoto action_14
action_13 _ = happyFail

action_14 _ = happyReduce_23

action_15 _ = happyReduce_43

action_16 _ = happyReduce_44

action_17 (33) = happyShift action_69
action_17 _ = happyFail

action_18 (30) = happyShift action_15
action_18 (31) = happyShift action_16
action_18 (32) = happyShift action_17
action_18 (34) = happyShift action_18
action_18 (35) = happyShift action_68
action_18 (36) = happyShift action_19
action_18 (37) = happyShift action_20
action_18 (38) = happyShift action_21
action_18 (39) = happyShift action_22
action_18 (40) = happyShift action_23
action_18 (41) = happyShift action_24
action_18 (42) = happyShift action_25
action_18 (52) = happyShift action_26
action_18 (53) = happyShift action_27
action_18 (54) = happyShift action_28
action_18 (55) = happyShift action_29
action_18 (58) = happyShift action_30
action_18 (60) = happyShift action_31
action_18 (9) = happyGoto action_67
action_18 (10) = happyGoto action_7
action_18 (11) = happyGoto action_8
action_18 (12) = happyGoto action_9
action_18 (13) = happyGoto action_10
action_18 (14) = happyGoto action_11
action_18 (15) = happyGoto action_12
action_18 (18) = happyGoto action_13
action_18 (19) = happyGoto action_14
action_18 _ = happyFail

action_19 _ = happyReduce_35

action_20 _ = happyReduce_36

action_21 _ = happyReduce_37

action_22 _ = happyReduce_38

action_23 _ = happyReduce_39

action_24 _ = happyReduce_40

action_25 _ = happyReduce_41

action_26 _ = happyReduce_24

action_27 _ = happyReduce_42

action_28 _ = happyReduce_45

action_29 (30) = happyShift action_15
action_29 (31) = happyShift action_16
action_29 (32) = happyShift action_17
action_29 (34) = happyShift action_18
action_29 (36) = happyShift action_19
action_29 (37) = happyShift action_20
action_29 (38) = happyShift action_21
action_29 (39) = happyShift action_22
action_29 (40) = happyShift action_23
action_29 (41) = happyShift action_24
action_29 (42) = happyShift action_25
action_29 (52) = happyShift action_26
action_29 (53) = happyShift action_27
action_29 (54) = happyShift action_28
action_29 (55) = happyShift action_29
action_29 (58) = happyShift action_30
action_29 (60) = happyShift action_31
action_29 (9) = happyGoto action_66
action_29 (10) = happyGoto action_7
action_29 (11) = happyGoto action_8
action_29 (12) = happyGoto action_9
action_29 (13) = happyGoto action_10
action_29 (14) = happyGoto action_11
action_29 (15) = happyGoto action_12
action_29 (18) = happyGoto action_13
action_29 (19) = happyGoto action_14
action_29 _ = happyFail

action_30 (52) = happyShift action_65
action_30 _ = happyFail

action_31 (52) = happyShift action_63
action_31 (62) = happyShift action_64
action_31 _ = happyFail

action_32 (69) = happyAccept
action_32 _ = happyFail

action_33 (34) = happyShift action_38
action_33 (35) = happyShift action_62
action_33 (53) = happyShift action_56
action_33 (63) = happyShift action_40
action_33 (64) = happyShift action_41
action_33 (65) = happyShift action_42
action_33 (66) = happyShift action_43
action_33 (22) = happyGoto action_60
action_33 (27) = happyGoto action_61
action_33 (28) = happyGoto action_36
action_33 (29) = happyGoto action_37
action_33 _ = happyFail

action_34 (69) = happyAccept
action_34 _ = happyFail

action_35 _ = happyReduce_59

action_36 (45) = happyShift action_57
action_36 (59) = happyShift action_58
action_36 (67) = happyShift action_59
action_36 _ = happyReduce_62

action_37 _ = happyReduce_64

action_38 (34) = happyShift action_38
action_38 (53) = happyShift action_56
action_38 (63) = happyShift action_40
action_38 (64) = happyShift action_41
action_38 (65) = happyShift action_42
action_38 (66) = happyShift action_43
action_38 (27) = happyGoto action_55
action_38 (28) = happyGoto action_36
action_38 (29) = happyGoto action_37
action_38 _ = happyFail

action_39 (68) = happyShift action_54
action_39 _ = happyReduce_69

action_40 _ = happyReduce_65

action_41 _ = happyReduce_66

action_42 _ = happyReduce_67

action_43 _ = happyReduce_68

action_44 (69) = happyAccept
action_44 _ = happyFail

action_45 (35) = happyShift action_52
action_45 (52) = happyShift action_53
action_45 (25) = happyGoto action_51
action_45 _ = happyFail

action_46 (69) = happyAccept
action_46 _ = happyFail

action_47 (69) = happyAccept
action_47 _ = happyFail

action_48 (52) = happyShift action_49
action_48 (62) = happyShift action_50
action_48 _ = happyFail

action_49 (50) = happyShift action_102
action_49 _ = happyFail

action_50 (52) = happyShift action_101
action_50 _ = happyFail

action_51 (48) = happyShift action_100
action_51 (24) = happyGoto action_99
action_51 _ = happyReduce_56

action_52 _ = happyReduce_53

action_53 (59) = happyShift action_98
action_53 _ = happyFail

action_54 (34) = happyShift action_38
action_54 (53) = happyShift action_39
action_54 (63) = happyShift action_40
action_54 (64) = happyShift action_41
action_54 (65) = happyShift action_42
action_54 (66) = happyShift action_43
action_54 (26) = happyGoto action_97
action_54 (27) = happyGoto action_35
action_54 (28) = happyGoto action_36
action_54 (29) = happyGoto action_37
action_54 _ = happyFail

action_55 (35) = happyShift action_96
action_55 _ = happyFail

action_56 _ = happyReduce_69

action_57 (34) = happyShift action_38
action_57 (53) = happyShift action_56
action_57 (63) = happyShift action_40
action_57 (64) = happyShift action_41
action_57 (65) = happyShift action_42
action_57 (66) = happyShift action_43
action_57 (27) = happyGoto action_95
action_57 (28) = happyGoto action_36
action_57 (29) = happyGoto action_37
action_57 _ = happyFail

action_58 (34) = happyShift action_38
action_58 (53) = happyShift action_56
action_58 (63) = happyShift action_40
action_58 (64) = happyShift action_41
action_58 (65) = happyShift action_42
action_58 (66) = happyShift action_43
action_58 (27) = happyGoto action_94
action_58 (28) = happyGoto action_36
action_58 (29) = happyGoto action_37
action_58 _ = happyFail

action_59 _ = happyReduce_63

action_60 (48) = happyShift action_93
action_60 (21) = happyGoto action_92
action_60 _ = happyReduce_51

action_61 (50) = happyShift action_91
action_61 _ = happyFail

action_62 _ = happyReduce_48

action_63 (50) = happyShift action_90
action_63 _ = happyFail

action_64 (52) = happyShift action_89
action_64 _ = happyFail

action_65 (59) = happyShift action_88
action_65 _ = happyFail

action_66 (56) = happyShift action_87
action_66 _ = happyFail

action_67 (35) = happyShift action_86
action_67 _ = happyFail

action_68 _ = happyReduce_47

action_69 _ = happyReduce_46

action_70 _ = happyReduce_19

action_71 _ = happyReduce_21

action_72 (30) = happyShift action_15
action_72 (31) = happyShift action_16
action_72 (32) = happyShift action_17
action_72 (34) = happyShift action_18
action_72 (36) = happyShift action_19
action_72 (37) = happyShift action_20
action_72 (38) = happyShift action_21
action_72 (39) = happyShift action_22
action_72 (40) = happyShift action_23
action_72 (41) = happyShift action_24
action_72 (42) = happyShift action_25
action_72 (52) = happyShift action_26
action_72 (53) = happyShift action_27
action_72 (54) = happyShift action_28
action_72 (55) = happyShift action_29
action_72 (58) = happyShift action_30
action_72 (60) = happyShift action_31
action_72 (10) = happyGoto action_85
action_72 (11) = happyGoto action_8
action_72 (12) = happyGoto action_9
action_72 (13) = happyGoto action_10
action_72 (14) = happyGoto action_11
action_72 (15) = happyGoto action_12
action_72 (18) = happyGoto action_13
action_72 (19) = happyGoto action_14
action_72 _ = happyFail

action_73 (30) = happyShift action_15
action_73 (31) = happyShift action_16
action_73 (32) = happyShift action_17
action_73 (34) = happyShift action_18
action_73 (36) = happyShift action_19
action_73 (37) = happyShift action_20
action_73 (38) = happyShift action_21
action_73 (39) = happyShift action_22
action_73 (40) = happyShift action_23
action_73 (41) = happyShift action_24
action_73 (42) = happyShift action_25
action_73 (52) = happyShift action_26
action_73 (53) = happyShift action_27
action_73 (54) = happyShift action_28
action_73 (55) = happyShift action_29
action_73 (58) = happyShift action_30
action_73 (60) = happyShift action_31
action_73 (13) = happyGoto action_83
action_73 (14) = happyGoto action_11
action_73 (15) = happyGoto action_84
action_73 (18) = happyGoto action_13
action_73 (19) = happyGoto action_14
action_73 _ = happyFail

action_74 _ = happyReduce_27

action_75 _ = happyReduce_28

action_76 _ = happyReduce_29

action_77 _ = happyReduce_30

action_78 _ = happyReduce_31

action_79 _ = happyReduce_32

action_80 _ = happyReduce_26

action_81 _ = happyReduce_33

action_82 _ = happyReduce_34

action_83 _ = happyReduce_11

action_84 (43) = happyReduce_13
action_84 (44) = happyReduce_13
action_84 (45) = happyReduce_13
action_84 (46) = happyReduce_13
action_84 (47) = happyReduce_13
action_84 (48) = happyReduce_13
action_84 (49) = happyReduce_13
action_84 (50) = happyReduce_13
action_84 (51) = happyReduce_13
action_84 _ = happyReduce_22

action_85 _ = happyReduce_9

action_86 _ = happyReduce_25

action_87 (30) = happyShift action_15
action_87 (31) = happyShift action_16
action_87 (32) = happyShift action_17
action_87 (34) = happyShift action_18
action_87 (36) = happyShift action_19
action_87 (37) = happyShift action_20
action_87 (38) = happyShift action_21
action_87 (39) = happyShift action_22
action_87 (40) = happyShift action_23
action_87 (41) = happyShift action_24
action_87 (42) = happyShift action_25
action_87 (52) = happyShift action_26
action_87 (53) = happyShift action_27
action_87 (54) = happyShift action_28
action_87 (55) = happyShift action_29
action_87 (58) = happyShift action_30
action_87 (60) = happyShift action_31
action_87 (9) = happyGoto action_114
action_87 (10) = happyGoto action_7
action_87 (11) = happyGoto action_8
action_87 (12) = happyGoto action_9
action_87 (13) = happyGoto action_10
action_87 (14) = happyGoto action_11
action_87 (15) = happyGoto action_12
action_87 (18) = happyGoto action_13
action_87 (19) = happyGoto action_14
action_87 _ = happyFail

action_88 (30) = happyShift action_15
action_88 (31) = happyShift action_16
action_88 (32) = happyShift action_17
action_88 (34) = happyShift action_18
action_88 (36) = happyShift action_19
action_88 (37) = happyShift action_20
action_88 (38) = happyShift action_21
action_88 (39) = happyShift action_22
action_88 (40) = happyShift action_23
action_88 (41) = happyShift action_24
action_88 (42) = happyShift action_25
action_88 (52) = happyShift action_26
action_88 (53) = happyShift action_27
action_88 (54) = happyShift action_28
action_88 (55) = happyShift action_29
action_88 (58) = happyShift action_30
action_88 (60) = happyShift action_31
action_88 (9) = happyGoto action_113
action_88 (10) = happyGoto action_7
action_88 (11) = happyGoto action_8
action_88 (12) = happyGoto action_9
action_88 (13) = happyGoto action_10
action_88 (14) = happyGoto action_11
action_88 (15) = happyGoto action_12
action_88 (18) = happyGoto action_13
action_88 (19) = happyGoto action_14
action_88 _ = happyFail

action_89 (52) = happyShift action_112
action_89 _ = happyFail

action_90 (30) = happyShift action_15
action_90 (31) = happyShift action_16
action_90 (32) = happyShift action_17
action_90 (34) = happyShift action_18
action_90 (36) = happyShift action_19
action_90 (37) = happyShift action_20
action_90 (38) = happyShift action_21
action_90 (39) = happyShift action_22
action_90 (40) = happyShift action_23
action_90 (41) = happyShift action_24
action_90 (42) = happyShift action_25
action_90 (52) = happyShift action_26
action_90 (53) = happyShift action_27
action_90 (54) = happyShift action_28
action_90 (55) = happyShift action_29
action_90 (58) = happyShift action_30
action_90 (60) = happyShift action_31
action_90 (9) = happyGoto action_111
action_90 (10) = happyGoto action_7
action_90 (11) = happyGoto action_8
action_90 (12) = happyGoto action_9
action_90 (13) = happyGoto action_10
action_90 (14) = happyGoto action_11
action_90 (15) = happyGoto action_12
action_90 (18) = happyGoto action_13
action_90 (19) = happyGoto action_14
action_90 _ = happyFail

action_91 (34) = happyShift action_38
action_91 (53) = happyShift action_56
action_91 (63) = happyShift action_40
action_91 (64) = happyShift action_41
action_91 (65) = happyShift action_42
action_91 (66) = happyShift action_43
action_91 (27) = happyGoto action_110
action_91 (28) = happyGoto action_36
action_91 (29) = happyGoto action_37
action_91 _ = happyFail

action_92 (35) = happyShift action_109
action_92 _ = happyFail

action_93 (34) = happyShift action_38
action_93 (53) = happyShift action_56
action_93 (63) = happyShift action_40
action_93 (64) = happyShift action_41
action_93 (65) = happyShift action_42
action_93 (66) = happyShift action_43
action_93 (22) = happyGoto action_108
action_93 (27) = happyGoto action_61
action_93 (28) = happyGoto action_36
action_93 (29) = happyGoto action_37
action_93 _ = happyFail

action_94 _ = happyReduce_61

action_95 _ = happyReduce_60

action_96 _ = happyReduce_70

action_97 _ = happyReduce_58

action_98 (34) = happyShift action_38
action_98 (53) = happyShift action_39
action_98 (63) = happyShift action_40
action_98 (64) = happyShift action_41
action_98 (65) = happyShift action_42
action_98 (66) = happyShift action_43
action_98 (26) = happyGoto action_107
action_98 (27) = happyGoto action_35
action_98 (28) = happyGoto action_36
action_98 (29) = happyGoto action_37
action_98 _ = happyFail

action_99 (35) = happyShift action_106
action_99 _ = happyFail

action_100 (52) = happyShift action_53
action_100 (25) = happyGoto action_105
action_100 _ = happyFail

action_101 (52) = happyShift action_104
action_101 _ = happyFail

action_102 (30) = happyShift action_15
action_102 (31) = happyShift action_16
action_102 (32) = happyShift action_17
action_102 (34) = happyShift action_18
action_102 (36) = happyShift action_19
action_102 (37) = happyShift action_20
action_102 (38) = happyShift action_21
action_102 (39) = happyShift action_22
action_102 (40) = happyShift action_23
action_102 (41) = happyShift action_24
action_102 (42) = happyShift action_25
action_102 (52) = happyShift action_26
action_102 (53) = happyShift action_27
action_102 (54) = happyShift action_28
action_102 (55) = happyShift action_29
action_102 (58) = happyShift action_30
action_102 (60) = happyShift action_31
action_102 (9) = happyGoto action_103
action_102 (10) = happyGoto action_7
action_102 (11) = happyGoto action_8
action_102 (12) = happyGoto action_9
action_102 (13) = happyGoto action_10
action_102 (14) = happyGoto action_11
action_102 (15) = happyGoto action_12
action_102 (18) = happyGoto action_13
action_102 (19) = happyGoto action_14
action_102 _ = happyFail

action_103 (61) = happyShift action_117
action_103 _ = happyReduce_6

action_104 (50) = happyShift action_120
action_104 _ = happyFail

action_105 (48) = happyShift action_100
action_105 (24) = happyGoto action_119
action_105 _ = happyReduce_56

action_106 _ = happyReduce_54

action_107 _ = happyReduce_57

action_108 (48) = happyShift action_93
action_108 (21) = happyGoto action_118
action_108 _ = happyReduce_51

action_109 _ = happyReduce_49

action_110 _ = happyReduce_52

action_111 (61) = happyShift action_117
action_111 _ = happyFail

action_112 (50) = happyShift action_116
action_112 _ = happyFail

action_113 _ = happyReduce_15

action_114 (57) = happyShift action_115
action_114 _ = happyFail

action_115 (30) = happyShift action_15
action_115 (31) = happyShift action_16
action_115 (32) = happyShift action_17
action_115 (34) = happyShift action_18
action_115 (36) = happyShift action_19
action_115 (37) = happyShift action_20
action_115 (38) = happyShift action_21
action_115 (39) = happyShift action_22
action_115 (40) = happyShift action_23
action_115 (41) = happyShift action_24
action_115 (42) = happyShift action_25
action_115 (52) = happyShift action_26
action_115 (53) = happyShift action_27
action_115 (54) = happyShift action_28
action_115 (55) = happyShift action_29
action_115 (58) = happyShift action_30
action_115 (60) = happyShift action_31
action_115 (9) = happyGoto action_124
action_115 (10) = happyGoto action_7
action_115 (11) = happyGoto action_8
action_115 (12) = happyGoto action_9
action_115 (13) = happyGoto action_10
action_115 (14) = happyGoto action_11
action_115 (15) = happyGoto action_12
action_115 (18) = happyGoto action_13
action_115 (19) = happyGoto action_14
action_115 _ = happyFail

action_116 (30) = happyShift action_15
action_116 (31) = happyShift action_16
action_116 (32) = happyShift action_17
action_116 (34) = happyShift action_18
action_116 (36) = happyShift action_19
action_116 (37) = happyShift action_20
action_116 (38) = happyShift action_21
action_116 (39) = happyShift action_22
action_116 (40) = happyShift action_23
action_116 (41) = happyShift action_24
action_116 (42) = happyShift action_25
action_116 (52) = happyShift action_26
action_116 (53) = happyShift action_27
action_116 (54) = happyShift action_28
action_116 (55) = happyShift action_29
action_116 (58) = happyShift action_30
action_116 (60) = happyShift action_31
action_116 (9) = happyGoto action_123
action_116 (10) = happyGoto action_7
action_116 (11) = happyGoto action_8
action_116 (12) = happyGoto action_9
action_116 (13) = happyGoto action_10
action_116 (14) = happyGoto action_11
action_116 (15) = happyGoto action_12
action_116 (18) = happyGoto action_13
action_116 (19) = happyGoto action_14
action_116 _ = happyFail

action_117 (30) = happyShift action_15
action_117 (31) = happyShift action_16
action_117 (32) = happyShift action_17
action_117 (34) = happyShift action_18
action_117 (36) = happyShift action_19
action_117 (37) = happyShift action_20
action_117 (38) = happyShift action_21
action_117 (39) = happyShift action_22
action_117 (40) = happyShift action_23
action_117 (41) = happyShift action_24
action_117 (42) = happyShift action_25
action_117 (52) = happyShift action_26
action_117 (53) = happyShift action_27
action_117 (54) = happyShift action_28
action_117 (55) = happyShift action_29
action_117 (58) = happyShift action_30
action_117 (60) = happyShift action_31
action_117 (9) = happyGoto action_122
action_117 (10) = happyGoto action_7
action_117 (11) = happyGoto action_8
action_117 (12) = happyGoto action_9
action_117 (13) = happyGoto action_10
action_117 (14) = happyGoto action_11
action_117 (15) = happyGoto action_12
action_117 (18) = happyGoto action_13
action_117 (19) = happyGoto action_14
action_117 _ = happyFail

action_118 _ = happyReduce_50

action_119 _ = happyReduce_55

action_120 (30) = happyShift action_15
action_120 (31) = happyShift action_16
action_120 (32) = happyShift action_17
action_120 (34) = happyShift action_18
action_120 (36) = happyShift action_19
action_120 (37) = happyShift action_20
action_120 (38) = happyShift action_21
action_120 (39) = happyShift action_22
action_120 (40) = happyShift action_23
action_120 (41) = happyShift action_24
action_120 (42) = happyShift action_25
action_120 (52) = happyShift action_26
action_120 (53) = happyShift action_27
action_120 (54) = happyShift action_28
action_120 (55) = happyShift action_29
action_120 (58) = happyShift action_30
action_120 (60) = happyShift action_31
action_120 (9) = happyGoto action_121
action_120 (10) = happyGoto action_7
action_120 (11) = happyGoto action_8
action_120 (12) = happyGoto action_9
action_120 (13) = happyGoto action_10
action_120 (14) = happyGoto action_11
action_120 (15) = happyGoto action_12
action_120 (18) = happyGoto action_13
action_120 (19) = happyGoto action_14
action_120 _ = happyFail

action_121 (61) = happyShift action_125
action_121 _ = happyReduce_7

action_122 _ = happyReduce_17

action_123 (61) = happyShift action_125
action_123 _ = happyFail

action_124 _ = happyReduce_16

action_125 (30) = happyShift action_15
action_125 (31) = happyShift action_16
action_125 (32) = happyShift action_17
action_125 (34) = happyShift action_18
action_125 (36) = happyShift action_19
action_125 (37) = happyShift action_20
action_125 (38) = happyShift action_21
action_125 (39) = happyShift action_22
action_125 (40) = happyShift action_23
action_125 (41) = happyShift action_24
action_125 (42) = happyShift action_25
action_125 (52) = happyShift action_26
action_125 (53) = happyShift action_27
action_125 (54) = happyShift action_28
action_125 (55) = happyShift action_29
action_125 (58) = happyShift action_30
action_125 (60) = happyShift action_31
action_125 (9) = happyGoto action_126
action_125 (10) = happyGoto action_7
action_125 (11) = happyGoto action_8
action_125 (12) = happyGoto action_9
action_125 (13) = happyGoto action_10
action_125 (14) = happyGoto action_11
action_125 (15) = happyGoto action_12
action_125 (18) = happyGoto action_13
action_125 (19) = happyGoto action_14
action_125 _ = happyFail

action_126 _ = happyReduce_18

happyReduce_5 = happySpecReduce_1  8 happyReduction_5
happyReduction_5 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (AnonDec happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happyReduce 4 8 happyReduction_6
happyReduction_6 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (ID happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (LetDec happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 6 8 happyReduction_7
happyReduction_7 ((HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (ID happy_var_4)) `HappyStk`
	(HappyTerminal (ID happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (LetRec happy_var_3 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_1  9 happyReduction_8
happyReduction_8 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  10 happyReduction_9
happyReduction_9 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn10
		 (BinOpExp happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  10 happyReduction_10
happyReduction_10 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  11 happyReduction_11
happyReduction_11 (HappyAbsSyn13  happy_var_3)
	(HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (BinOpExp happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  11 happyReduction_12
happyReduction_12 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  12 happyReduction_13
happyReduction_13 (HappyAbsSyn15  happy_var_3)
	(HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (BinOpExp happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  12 happyReduction_14
happyReduction_14 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happyReduce 4 13 happyReduction_15
happyReduction_15 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (ID happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (FunExp happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 6 13 happyReduction_16
happyReduction_16 ((HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (IfExp happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 6 13 happyReduction_17
happyReduction_17 ((HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (ID happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (LetExp happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 8 13 happyReduction_18
happyReduction_18 ((HappyAbsSyn9  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (ID happy_var_4)) `HappyStk`
	(HappyTerminal (ID happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (LetRecExp happy_var_3 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_2  13 happyReduction_19
happyReduction_19 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn13
		 (MonOpExp happy_var_1 happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  13 happyReduction_20
happyReduction_20 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  14 happyReduction_21
happyReduction_21 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (AppExp happy_var_1 happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  14 happyReduction_22
happyReduction_22 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  15 happyReduction_23
happyReduction_23 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn15
		 (ConstExp happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  15 happyReduction_24
happyReduction_24 (HappyTerminal (ID happy_var_1))
	 =  HappyAbsSyn15
		 (VarExp happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  15 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  16 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn16
		 (ConsOp
	)

happyReduce_27 = happySpecReduce_1  17 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn17
		 (IntPlusOp
	)

happyReduce_28 = happySpecReduce_1  17 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn17
		 (IntMinusOp
	)

happyReduce_29 = happySpecReduce_1  17 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn17
		 (IntTimesOp
	)

happyReduce_30 = happySpecReduce_1  17 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn17
		 (IntDivOp
	)

happyReduce_31 = happySpecReduce_1  17 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn17
		 (ConcatOp
	)

happyReduce_32 = happySpecReduce_1  17 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn17
		 (CommaOp
	)

happyReduce_33 = happySpecReduce_1  17 happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn17
		 (EqOp
	)

happyReduce_34 = happySpecReduce_1  17 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn17
		 (GreaterOp
	)

happyReduce_35 = happySpecReduce_1  18 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn18
		 (HdOp
	)

happyReduce_36 = happySpecReduce_1  18 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn18
		 (TlOp
	)

happyReduce_37 = happySpecReduce_1  18 happyReduction_37
happyReduction_37 _
	 =  HappyAbsSyn18
		 (PrintOp
	)

happyReduce_38 = happySpecReduce_1  18 happyReduction_38
happyReduction_38 _
	 =  HappyAbsSyn18
		 (IntNegOp
	)

happyReduce_39 = happySpecReduce_1  18 happyReduction_39
happyReduction_39 _
	 =  HappyAbsSyn18
		 (FstOp
	)

happyReduce_40 = happySpecReduce_1  18 happyReduction_40
happyReduction_40 _
	 =  HappyAbsSyn18
		 (SndOp
	)

happyReduce_41 = happySpecReduce_1  18 happyReduction_41
happyReduction_41 _
	 =  HappyAbsSyn18
		 (NotOp
	)

happyReduce_42 = happySpecReduce_1  19 happyReduction_42
happyReduction_42 (HappyTerminal (INT happy_var_1))
	 =  HappyAbsSyn19
		 (IntConst happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  19 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn19
		 (BoolConst True
	)

happyReduce_44 = happySpecReduce_1  19 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn19
		 (BoolConst False
	)

happyReduce_45 = happySpecReduce_1  19 happyReduction_45
happyReduction_45 (HappyTerminal (STRING happy_var_1))
	 =  HappyAbsSyn19
		 (StringConst happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  19 happyReduction_46
happyReduction_46 _
	_
	 =  HappyAbsSyn19
		 (NilConst
	)

happyReduce_47 = happySpecReduce_2  19 happyReduction_47
happyReduction_47 _
	_
	 =  HappyAbsSyn19
		 (UnitConst
	)

happyReduce_48 = happySpecReduce_2  20 happyReduction_48
happyReduction_48 _
	_
	 =  HappyAbsSyn20
		 ([]
	)

happyReduce_49 = happyReduce 4 20 happyReduction_49
happyReduction_49 (_ `HappyStk`
	(HappyAbsSyn21  happy_var_3) `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (happy_var_2 : happy_var_3
	) `HappyStk` happyRest

happyReduce_50 = happySpecReduce_3  21 happyReduction_50
happyReduction_50 (HappyAbsSyn21  happy_var_3)
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (happy_var_2 : happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_0  21 happyReduction_51
happyReduction_51  =  HappyAbsSyn21
		 ([]
	)

happyReduce_52 = happySpecReduce_3  22 happyReduction_52
happyReduction_52 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn22
		 ((happy_var_1, happy_var_3)
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_2  23 happyReduction_53
happyReduction_53 _
	_
	 =  HappyAbsSyn23
		 ([]
	)

happyReduce_54 = happyReduce 4 23 happyReduction_54
happyReduction_54 (_ `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (happy_var_2 : happy_var_3
	) `HappyStk` happyRest

happyReduce_55 = happySpecReduce_3  24 happyReduction_55
happyReduction_55 (HappyAbsSyn24  happy_var_3)
	(HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (happy_var_2 : happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_0  24 happyReduction_56
happyReduction_56  =  HappyAbsSyn24
		 ([]
	)

happyReduce_57 = happySpecReduce_3  25 happyReduction_57
happyReduction_57 (HappyAbsSyn26  happy_var_3)
	_
	(HappyTerminal (ID happy_var_1))
	 =  HappyAbsSyn25
		 ((happy_var_1, happy_var_3)
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  26 happyReduction_58
happyReduction_58 (HappyAbsSyn26  happy_var_3)
	_
	(HappyTerminal (INT happy_var_1))
	 =  HappyAbsSyn26
		 (let (l, tau) = happy_var_3 in (happy_var_1:l, tau)
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  26 happyReduction_59
happyReduction_59 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn26
		 (([], happy_var_1)
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  27 happyReduction_60
happyReduction_60 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn27
		 (pairTy happy_var_1 happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  27 happyReduction_61
happyReduction_61 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn27
		 (funTy happy_var_1 happy_var_3
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  27 happyReduction_62
happyReduction_62 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_2  28 happyReduction_63
happyReduction_63 _
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn28
		 (listTy happy_var_1
	)
happyReduction_63 _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  28 happyReduction_64
happyReduction_64 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn28
		 (happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  29 happyReduction_65
happyReduction_65 _
	 =  HappyAbsSyn29
		 (intTy
	)

happyReduce_66 = happySpecReduce_1  29 happyReduction_66
happyReduction_66 _
	 =  HappyAbsSyn29
		 (boolTy
	)

happyReduce_67 = happySpecReduce_1  29 happyReduction_67
happyReduction_67 _
	 =  HappyAbsSyn29
		 (stringTy
	)

happyReduce_68 = happySpecReduce_1  29 happyReduction_68
happyReduction_68 _
	 =  HappyAbsSyn29
		 (unitTy
	)

happyReduce_69 = happySpecReduce_1  29 happyReduction_69
happyReduction_69 (HappyTerminal (INT happy_var_1))
	 =  HappyAbsSyn29
		 (TyVar happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_3  29 happyReduction_70
happyReduction_70 _
	(HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn29
		 (happy_var_2
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 69 69 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TRUE -> cont 30;
	FALSE -> cont 31;
	LBRAC -> cont 32;
	RBRAC -> cont 33;
	LPAREN -> cont 34;
	RPAREN -> cont 35;
	HD -> cont 36;
	TL -> cont 37;
	PRINT -> cont 38;
	NEG -> cont 39;
	FST -> cont 40;
	SND -> cont 41;
	NOT -> cont 42;
	PLUS -> cont 43;
	MINUS -> cont 44;
	TIMES -> cont 45;
	DIV -> cont 46;
	CARAT -> cont 47;
	COMMA -> cont 48;
	DCOLON -> cont 49;
	EQUALS -> cont 50;
	GE -> cont 51;
	ID happy_dollar_dollar -> cont 52;
	INT happy_dollar_dollar -> cont 53;
	STRING happy_dollar_dollar -> cont 54;
	IF -> cont 55;
	THEN -> cont 56;
	ELSE -> cont 57;
	FUN -> cont 58;
	ARROW -> cont 59;
	LET -> cont 60;
	IN -> cont 61;
	REC -> cont 62;
	T_INT -> cont 63;
	T_BOOL -> cont 64;
	T_STRING -> cont 65;
	T_UNIT -> cont 66;
	T_LIST -> cont 67;
	DOT -> cont 68;
	_ -> happyError' (tk:tks)
	}

happyError_ 69 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn8 z -> happyReturn z; _other -> notHappyAtAll })

parseExp tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn9 z -> happyReturn z; _other -> notHappyAtAll })

parseEnv tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn23 z -> happyReturn z; _other -> notHappyAtAll })

parseType tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn26 z -> happyReturn z; _other -> notHappyAtAll })

parseEqList tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn20 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token =
  TRUE | FALSE | LBRAC | RBRAC | LPAREN | RPAREN
  | HD | TL | PRINT | NEG | FST | SND | NOT
  | PLUS | MINUS | TIMES | DIV | CARAT | COMMA | DCOLON
  | EQUALS | GE | ID String | INT Int | STRING String
  | IF | THEN | ELSE | FUN | ARROW | LET | IN | REC
  | T_INT | T_BOOL | T_STRING | T_UNIT | T_LIST | DOT

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | isSpace c = lexer cs
  | isAlpha c = lexWord (c:cs)
  | isDigit c = lexInt (c:cs)
lexer ('"':cs) = lexStrConst cs ""
lexer ('[':cs) = LBRAC : lexer cs
lexer (']':cs) = RBRAC : lexer cs
lexer ('(':cs) = LPAREN : lexer cs
lexer (')':cs) = RPAREN : lexer cs
lexer ('~':cs) = NEG : lexer cs
lexer ('+':cs) = PLUS : lexer cs
lexer ('-':'>':cs) = ARROW : lexer cs
lexer ('-':cs) = MINUS : lexer cs
lexer ('*':cs) = TIMES : lexer cs
lexer ('/':cs) = DIV : lexer cs
lexer ('^':cs) = CARAT : lexer cs
lexer (',':cs) = COMMA : lexer cs
lexer (':':':':cs) = DCOLON : lexer cs
lexer ('=':cs) = EQUALS : lexer cs
lexer ('>':cs) = GE : lexer cs
lexer ('.':cs) = DOT : lexer cs
lexer (_:cs) = lexer cs

lexInt cs =
  let (num, cs') = span isDigit cs
    in INT (read num) : lexer cs'

lexWord cs =
  case span isAlpha cs of
    ("true", cs) -> TRUE : lexer cs
    ("false", cs) -> FALSE : lexer cs
    ("hd", cs) -> HD : lexer cs
    ("tl", cs) -> TL : lexer cs
    ("print", cs) -> PRINT : lexer cs
    ("fst", cs) -> FST : lexer cs
    ("snd", cs) -> SND : lexer cs
    ("not", cs) -> NOT : lexer cs
    ("if", cs) -> IF : lexer cs
    ("then", cs) -> THEN : lexer cs
    ("else", cs) -> ELSE : lexer cs
    ("fun", cs) -> FUN : lexer cs
    ("let", cs) -> LET : lexer cs
    ("in", cs) -> IN : lexer cs
    ("rec", cs) -> REC : lexer cs
    ("int", cs) -> T_INT : lexer cs
    ("bool", cs) -> T_BOOL : lexer cs
    ("string", cs) -> T_STRING : lexer cs
    ("unit", cs) -> T_UNIT : lexer cs
    ("list", cs) -> T_LIST : lexer cs
    (id, cs) -> ID id : lexer cs

lexStrConst [] s = [STRING s]
lexStrConst ('"':cs) s = STRING s : lexer cs
lexStrConst (c:cs) s = lexStrConst cs (s ++ [c])
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4










































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

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
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

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

{-# LINE 322 "templates/GenericTemplate.hs" #-}
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
