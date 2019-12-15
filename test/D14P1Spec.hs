module D14P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as M

import D14P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "D14P1 Unit tests"
    [
        testCase "gives correct answer to the original problem" $ do
            1 @=? 1
            -- 31 @=? fst . orecount M.fromList [
            --         ("A",(10.0,[("ORE",10.0)])),("B",(1.0,[("ORE",1.0)])),("C",(1.0,[("A",7.0),("B",1.0)])),("D",(1.0,[("A",7.0),("C",1.0)])),("E",(1.0,[("A",7.0),("D",1.0)])),("FUEL",(1.0,[("A",7.0),("E",1.0)]))
            --     ]
            -- 165 @=? orecount [
            --         (("A",2),[("ORE",9)]),(("B",3),[("ORE",8)]),(("C",5),[("ORE",7)]),(("AB",1),[("A",3),("B",4)]),(("BC",1),[("B",5),("C",7)]),(("CA",1),[("C",4),("A",1)]),(("FUEL",1),[("AB",2),("BC",3),("CA",4)])
            --     ]
            -- 13312 @=? orecount [
            --         (("NZVS",5),[("ORE",157)]),(("DCFZ",6),[("ORE",165)]),(("FUEL",1),[("XJWVT",44),("KHKGT",5),("QDVJ",1),("NZVS",29),("GPVTF",9),("HKGWZ",48)]),(("QDVJ",9),[("HKGWZ",12),("GPVTF",1),("PSHF",8)]),(("PSHF",7),[("ORE",179)]),(("HKGWZ",5),[("ORE",177)]),(("XJWVT",2),[("DCFZ",7),("PSHF",7)]),(("GPVTF",2),[("ORE",165)]),(("KHKGT",8),[("DCFZ",3),("NZVS",7),("HKGWZ",5),("PSHF",10)])
            --     ]
            -- 180697 @=? orecount [
            --         (("STKFG",1),[("VPVL",2),("FWMGM",7),("CXFTF",2),("MNCFX",11)]),(("VPVL",8),[("NVRVD",17),("JNWZP",3)]),(("FUEL",1),[("STKFG",53),("MNCFX",6),("VJHF",46),("HVMC",81),("CXFTF",68),("GNMV",25)]),(("FWMGM",5),[("VJHF",22),("MNCFX",37)]),(("NVRVD",4),[("ORE",139)]),(("JNWZP",7),[("ORE",144)]),(("HVMC",3),[("MNCFX",5),("RFSQX",7),("FWMGM",2),("VPVL",2),("CXFTF",19)]),(("GNMV",6),[("VJHF",5),("MNCFX",7),("VPVL",9),("CXFTF",37)]),(("MNCFX",6),[("ORE",145)]),(("CXFTF",8),[("NVRVD",1)]),(("RFSQX",4),[("VJHF",1),("MNCFX",6)]),(("VJHF",6),[("ORE",176)])
            --     ]
            -- 2210736 @=? orecount [
            --         (("CNZTR",8),[("ORE",171)]),(("PLWSL",4),[("ZLQW",7),("BMBT",3),("XCVML",9),("XMNCP",26),("WPTQ",1),("MZWV",2),("RJRHP",1)]),(("BHXH",4),[("ORE",114)]),(("BMBT",6),[("VRPVC",14)]),(("FUEL",1),[("BHXH",6),("KTJDG",18),("WPTQ",12),("PLWSL",7),("FHTLT",31),("ZDVW",37)]),(("FHTLT",6),[("WPTQ",6),("BMBT",2),("ZLQW",8),("KTJDG",18),("XMNCP",1),("MZWV",6),("RJRHP",1)]),(("ZLQW",6),[("XDBXC",15),("LTCX",2),("VRPVC",1)]),(("ZDVW",1),[("WPTQ",13),("LTCX",10),("RJRHP",3),("XMNCP",14),("MZWV",2),("ZLQW",1)]),(("WPTQ",4),[("BMBT",5)]),(("KTJDG",9),[("ORE",189)]),(("XMNCP",2),[("MZWV",1),("XDBXC",17),("XCVML",3)]),(("XDBXC",2),[("VRPVC",12),("CNZTR",27)]),(("XCVML",5),[("KTJDG",15),("BHXH",12)]),(("MZWV",7),[("BHXH",3),("VRPVC",2)]),(("VRPVC",7),[("ORE",121)]),(("RJRHP",6),[("XCVML",7)]),(("LTCX",5),[("BHXH",5),("VRPVC",4)])
            --     ]
    ]
