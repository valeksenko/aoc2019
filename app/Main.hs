module Main where

import D03P2

main :: IO ()
main = do
    print $ stepstointersection [
            R 991,U 77,L 916,D 26,R 424,D 739,L 558,D 439,R 636,U 616,L 364,D 653,R 546,U 909,L 66,D 472,R 341,U 906,L 37,D 360,L 369,D 451,L 649,D 521,R 2,U 491,R 409,U 801,R 23,U 323,L 209,U 171,L 849,D 891,L 854,U 224,R 476,D 519,L 937,U 345,R 722,D 785,L 312,D 949,R 124,U 20,R 677,D 236,R 820,D 320,L 549,D 631,R 42,U 621,R 760,U 958,L 925,U 84,R 914,U 656,R 598,D 610,R 397,D 753,L 109,U 988,R 435,U 828,R 219,U 583,L 317,D 520,L 940,D 850,R 594,D 801,L 422,U 292,R 883,U 204,L 76,U 860,L 753,U 483,L 183,U 179,R 441,U 163,L 859,U 437,L 485,D 239,R 454,D 940,R 689,D 704,R 110,D 12,R 370,D 413,L 192,D 979,R 990,D 651,L 308,U 177,R 787,D 717,R 245,U 689,R 11,D 509,L 680,U 228,L 347,D 179,R 508,D 40,L 502,U 689,L 643,U 45,R 884,D 653,L 23,D 918,L 825,D 312,L 691,U 292,L 285,D 183,R 997,U 427,L 89,U 252,R 475,U 217,R 16,U 749,L 578,D 931,L 273,U 509,L 741,U 97,R 407,U 275,L 605,U 136,L 558,U 318,R 478,U 505,R 446,U 295,R 562,D 646,R 988,D 254,L 68,U 645,L 953,U 916,L 442,D 713,R 978,U 540,R 447,U 594,L 804,U 215,R 95,D 995,R 818,D 237,R 212,U 664,R 455,D 684,L 338,U 308,R 463,D 985,L 988,D 281,R 758,U 510,L 232,U 509,R 289,D 90,R 65,D 46,R 886,D 741,L 327,U 755,R 236,U 870,L 764,U 60,R 391,U 91,R 367,U 587,L 651,D 434,L 47,U 954,R 707,D 336,L 242,D 387,L 410,D 19,R 203,D 703,L 228,U 292,L 19,U 916,R 411,U 421,L 726,U 543,L 240,U 755,R 157,U 836,L 397,U 71,L 125,D 934,L 723,D 145,L 317,D 229,R 863,U 941,L 926,D 55,L 2,D 452,R 895,D 670,L 216,U 504,R 66,U 696,L 581,U 75,L 235,U 88,L 609,U 415,L 850,U 21,L 109,U 416,R 408,D 367,R 823,D 199,L 718,U 136,L 860,U 780,L 308,D 312,R 230,D 671,R 477,D 672,L 94,U 307,R 301,D 143,L 300,D 792,L 593,D 399,R 840,D 225,R 680,D 484,L 646,D 917,R 132,D 213,L 779,D 143,L 176,U 673,L 772,D 93,L 10,D 624,L 244,D 993,R 346
        ] [
            L 997,U 989,L 596,U 821,L 419,U 118,R 258,D 239,R 902,D 810,R 553,D 271,R 213,D 787,R 723,D 57,L 874,D 556,R 53,U 317,L 196,D 813,R 500,U 151,R 180,D 293,L 415,U 493,L 99,U 482,R 517,U 649,R 102,U 860,R 905,D 499,R 133,D 741,R 394,U 737,L 903,U 800,R 755,D 376,L 11,U 751,R 539,U 33,R 539,U 30,L 534,D 631,L 714,U 190,L 446,U 409,R 977,D 731,R 282,U 244,R 29,D 212,L 523,D 570,L 89,D 327,R 178,U 970,R 435,U 250,R 213,D 604,R 64,D 348,R 315,D 994,L 508,D 261,R 62,D 50,L 347,U 183,R 410,D 627,L 128,U 855,L 803,D 695,L 879,U 857,L 629,D 145,L 341,D 733,L 566,D 626,L 302,U 236,L 55,U 428,R 183,U 254,R 226,D 228,R 616,U 137,L 593,U 204,R 620,U 624,R 605,D 705,L 263,D 568,R 931,D 464,R 989,U 621,L 277,U 274,L 137,U 768,L 261,D 360,L 45,D 110,R 35,U 212,L 271,D 318,L 444,D 427,R 225,D 380,L 907,D 193,L 118,U 741,L 101,D 298,R 604,D 598,L 98,U 458,L 733,U 511,L 82,D 173,L 644,U 803,R 926,D 610,R 24,D 170,L 198,U 766,R 656,D 474,L 393,D 934,L 789,U 92,L 889,U 460,L 232,U 193,L 877,D 380,L 455,D 526,R 899,D 696,R 452,U 95,L 828,D 720,R 370,U 664,L 792,D 204,R 84,D 749,R 808,U 132,L 152,D 375,R 19,U 164,L 615,D 121,R 644,D 289,R 381,U 126,L 304,U 508,L 112,D 268,L 572,D 838,L 998,U 127,R 500,D 344,R 694,U 451,L 846,D 565,R 158,U 47,L 430,U 214,R 571,D 983,R 690,D 227,L 107,U 109,L 286,D 66,L 544,U 205,L 453,U 716,L 36,U 672,L 517,U 878,L 487,U 936,L 628,U 253,R 424,D 409,R 422,U 636,R 412,U 553,R 59,D 332,R 7,U 495,L 305,D 939,L 428,D 821,R 749,D 195,R 531,D 898,R 337,D 303,L 398,D 625,R 57,D 503,L 699,D 553,L 478,U 716,R 897,D 3,R 420,U 903,R 994,U 864,L 745,U 205,R 229,U 126,L 227,D 454,R 670,U 605,L 356,U 499,R 510,U 238,L 542,D 440,R 156,D 512,L 237,D 341,L 439,U 642,R 873,D 650,R 871,D 616,R 322,U 696,R 248,D 746,R 990,U 829,R 812,U 294,L 462,U 740,R 780
        ]
