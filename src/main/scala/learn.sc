import me.shadaj.genalgo.sequences.{AminoAcid, Protein}
import org.denigma.bio.lessons.peptides.{LinearSpectrum, ProteinWeights, CycloSpectrum, CycloSequencer}

import scala.collection.immutable.IndexedSeq
import scala.io.Source

//val input = "0 71 71 87 87 97 97 99 101 101 101 101 103 113 113 113 113 113 115 115 115 115 128 128 129 129 131 131 137 147 156 156 156 163 163 163 163 172 184 186 186 199 204 210 212 212 214 214 214 216 218 226 227 228 230 231 232 243 244 244 257 259 260 260 262 273 276 278 278 281 285 287 292 293 293 301 302 303 312 314 319 325 326 327 327 327 327 328 331 332 341 358 360 364 373 374 374 375 377 389 390 391 393 394 398 402 403 407 407 409 415 416 418 425 428 429 440 440 441 441 444 445 449 456 464 465 472 475 488 490 492 494 497 499 502 503 503 504 504 505 505 512 516 517 520 521 521 531 540 541 544 550 554 556 559 560 569 570 572 573 577 578 587 592 592 601 603 605 612 613 617 618 618 619 621 621 622 625 628 634 634 653 655 657 659 661 669 671 672 675 678 680 684 688 691 696 701 704 705 707 714 715 716 716 718 721 722 723 724 732 734 735 743 748 749 749 756 768 769 775 776 781 781 785 791 792 799 800 804 806 808 809 816 819 822 822 825 829 829 830 835 836 836 843 846 847 850 852 852 870 872 877 879 887 890 895 900 904 905 905 906 912 913 919 919 921 923 928 931 932 935 937 938 942 942 944 947 948 948 953 959 961 965 976 985 985 985 992 999 1005 1008 1013 1015 1016 1020 1024 1024 1028 1032 1035 1036 1036 1036 1043 1046 1049 1050 1051 1061 1061 1062 1062 1066 1068 1069 1073 1075 1076 1089 1090 1098 1100 1114 1117 1122 1123 1131 1132 1133 1133 1133 1137 1137 1137 1139 1141 1148 1149 1152 1155 1160 1162 1162 1163 1174 1174 1176 1179 1183 1191 1198 1199 1201 1201 1204 1219 1220 1224 1225 1229 1232 1232 1234 1244 1245 1248 1250 1250 1252 1252 1253 1261 1261 1262 1263 1264 1265 1273 1280 1288 1296 1302 1302 1305 1312 1313 1316 1316 1318 1325 1332 1335 1335 1339 1345 1347 1349 1350 1351 1354 1354 1360 1362 1363 1363 1367 1369 1376 1377 1387 1388 1391 1392 1392 1393 1397 1403 1410 1415 1417 1425 1425 1428 1433 1434 1440 1444 1451 1460 1463 1464 1464 1464 1464 1464 1465 1469 1475 1477 1479 1488 1491 1492 1497 1501 1502 1505 1510 1520 1525 1525 1526 1526 1530 1531 1534 1540 1540 1543 1547 1548 1556 1561 1564 1564 1566 1577 1578 1579 1588 1590 1590 1592 1593 1597 1603 1606 1619 1623 1626 1627 1627 1627 1628 1634 1635 1637 1637 1638 1639 1641 1648 1648 1653 1662 1665 1671 1674 1676 1677 1681 1689 1693 1693 1695 1703 1706 1708 1719 1724 1724 1727 1729 1734 1735 1738 1740 1742 1742 1750 1752 1752 1753 1754 1754 1756 1760 1763 1765 1766 1778 1784 1785 1790 1792 1794 1804 1804 1806 1808 1821 1821 1823 1832 1837 1837 1837 1851 1853 1853 1853 1855 1857 1857 1862 1865 1866 1866 1867 1871 1871 1875 1879 1879 1883 1885 1889 1890 1891 1897 1905 1908 1916 1919 1923 1928 1933 1936 1941 1941 1949 1952 1964 1966 1966 1966 1966 1967 1968 1968 1977 1980 1984 1984 1985 1988 1992 2000 2004 2004 2009 2010 2012 2020 2020 2022 2028 2031 2034 2034 2036 2037 2041 2046 2051 2052 2053 2056 2065 2079 2079 2081 2081 2087 2091 2093 2097 2097 2105 2111 2113 2115 2116 2122 2123 2124 2129 2131 2133 2135 2138 2139 2140 2147 2148 2152 2153 2159 2160 2165 2168 2168 2169 2178 2180 2184 2187 2192 2197 2198 2206 2206 2209 2215 2224 2226 2237 2237 2240 2244 2244 2244 2247 2250 2253 2255 2260 2263 2264 2266 2268 2269 2277 2280 2286 2287 2293 2293 2294 2296 2297 2300 2301 2307 2309 2310 2311 2315 2315 2324 2334 2337 2339 2341 2352 2354 2356 2359 2369 2378 2378 2382 2383 2384 2386 2392 2393 2396 2397 2399 2400 2400 2406 2407 2413 2416 2424 2425 2427 2429 2430 2433 2438 2440 2443 2446 2449 2449 2449 2453 2456 2456 2467 2469 2478 2484 2487 2487 2495 2496 2501 2506 2509 2513 2515 2524 2525 2525 2528 2533 2534 2540 2541 2545 2546 2553 2554 2555 2558 2560 2562 2564 2569 2570 2571 2577 2578 2580 2582 2588 2596 2596 2600 2602 2606 2612 2612 2614 2614 2628 2637 2640 2641 2642 2647 2652 2656 2657 2659 2659 2662 2665 2671 2673 2673 2681 2683 2684 2689 2689 2693 2701 2705 2708 2709 2709 2713 2716 2725 2725 2726 2727 2727 2727 2727 2729 2741 2744 2752 2752 2757 2760 2765 2770 2774 2777 2785 2788 2796 2802 2803 2804 2808 2810 2814 2814 2818 2822 2822 2826 2827 2827 2828 2831 2836 2836 2838 2840 2840 2840 2842 2856 2856 2856 2861 2870 2872 2872 2885 2887 2889 2889 2899 2901 2903 2908 2909 2915 2927 2928 2930 2933 2937 2939 2939 2940 2941 2941 2943 2951 2951 2953 2955 2958 2959 2964 2966 2969 2969 2974 2985 2987 2990 2998 3000 3000 3004 3012 3016 3017 3019 3022 3028 3031 3040 3045 3045 3052 3054 3055 3056 3056 3058 3059 3065 3066 3066 3066 3067 3070 3074 3087 3090 3096 3100 3101 3103 3103 3105 3114 3115 3116 3127 3129 3129 3132 3137 3145 3146 3150 3153 3153 3159 3162 3163 3167 3167 3168 3168 3173 3183 3188 3191 3192 3196 3201 3202 3205 3214 3216 3218 3224 3228 3229 3229 3229 3229 3229 3230 3233 3242 3249 3253 3259 3260 3265 3268 3268 3276 3278 3283 3290 3296 3300 3301 3301 3302 3305 3306 3316 3317 3324 3326 3330 3330 3331 3333 3339 3339 3342 3343 3344 3346 3348 3354 3358 3358 3361 3368 3375 3377 3377 3380 3381 3388 3391 3391 3397 3405 3413 3420 3428 3429 3430 3431 3432 3432 3440 3441 3441 3443 3443 3445 3448 3449 3459 3461 3461 3464 3468 3469 3473 3474 3489 3492 3492 3494 3495 3502 3510 3514 3517 3519 3519 3530 3531 3531 3533 3538 3541 3544 3545 3552 3554 3556 3556 3556 3560 3560 3560 3561 3562 3570 3571 3576 3579 3593 3595 3603 3604 3617 3618 3620 3624 3625 3627 3631 3631 3632 3632 3642 3643 3644 3647 3650 3657 3657 3657 3658 3661 3665 3669 3669 3673 3677 3678 3680 3685 3688 3694 3701 3708 3708 3708 3717 3728 3732 3734 3740 3745 3745 3746 3749 3751 3751 3755 3756 3758 3761 3762 3765 3770 3772 3774 3774 3780 3781 3787 3788 3788 3789 3793 3798 3803 3806 3814 3816 3821 3823 3841 3841 3843 3846 3847 3850 3857 3857 3858 3863 3864 3864 3868 3871 3871 3874 3877 3884 3885 3887 3889 3893 3894 3901 3902 3908 3912 3912 3917 3918 3924 3925 3937 3944 3944 3945 3950 3958 3959 3961 3969 3970 3971 3972 3975 3977 3977 3978 3979 3986 3988 3989 3992 3997 4002 4005 4009 4013 4015 4018 4021 4022 4024 4032 4034 4036 4038 4040 4059 4059 4065 4068 4071 4072 4072 4074 4075 4075 4076 4080 4081 4088 4090 4092 4101 4101 4106 4115 4116 4120 4121 4123 4124 4133 4134 4137 4139 4143 4149 4152 4153 4162 4172 4172 4173 4176 4177 4181 4188 4188 4189 4189 4190 4190 4191 4194 4196 4199 4201 4203 4205 4218 4221 4228 4229 4237 4244 4248 4249 4252 4252 4253 4253 4264 4265 4268 4275 4277 4278 4284 4286 4286 4290 4291 4295 4299 4300 4302 4303 4304 4316 4318 4319 4319 4320 4329 4333 4335 4352 4361 4362 4365 4366 4366 4366 4366 4367 4368 4374 4379 4381 4390 4391 4392 4400 4400 4401 4406 4408 4412 4415 4415 4417 4420 4431 4433 4433 4434 4436 4449 4449 4450 4461 4462 4463 4465 4466 4467 4475 4477 4479 4479 4479 4481 4481 4483 4489 4494 4507 4507 4509 4521 4530 4530 4530 4530 4537 4537 4537 4546 4556 4562 4562 4564 4564 4565 4565 4578 4578 4578 4578 4580 4580 4580 4580 4580 4590 4592 4592 4592 4592 4594 4596 4596 4606 4606 4622 4622 4693"
//val input = "0 57 57 71 87 87 87 97 97 97 99 99 99 101 101 101 101 101 101 103 103 103 113 113 113 113 113 113 114 114 114 114 115 128 128 128 131 137 137 147 147 147 154 156 156 158 163 163 163 163 163 186 186 186 186 186 188 188 194 194 198 198 202 204 204 212 212 215 216 217 226 227 227 228 228 229 241 243 243 245 250 250 251 253 256 259 260 262 264 264 266 273 277 277 278 287 289 291 291 291 294 295 299 299 299 299 307 310 310 312 314 317 318 319 323 324 329 330 330 332 342 342 342 349 349 351 352 354 354 356 362 363 363 365 367 371 372 378 388 391 392 396 398 400 404 404 406 407 408 409 411 415 419 425 425 426 429 429 431 436 436 436 438 445 445 446 450 452 452 453 454 455 462 463 464 466 468 470 473 475 479 484 485 485 489 491 492 492 493 501 503 505 510 512 512 518 528 534 535 538 539 539 542 546 549 549 551 553 553 555 558 559 559 559 563 566 566 572 573 576 582 586 589 590 590 592 592 593 593 594 598 600 601 602 611 613 615 615 616 622 622 622 626 629 631 640 641 641 647 648 652 652 653 662 666 672 672 673 674 674 677 679 679 681 687 690 690 691 694 695 697 698 700 701 701 702 703 716 716 723 727 728 729 729 730 735 736 739 739 740 741 742 745 749 753 755 757 761 761 769 776 778 778 778 785 787 787 788 790 791 791 794 802 802 802 803 804 809 815 815 816 828 829 829 830 830 833 835 836 838 841 844 844 844 846 848 852 852 856 860 864 866 867 873 875 881 885 886 888 888 888 889 889 891 900 900 902 902 903 908 915 917 917 918 922 925 930 930 932 941 943 943 943 943 944 945 947 949 951 957 961 965 965 974 976 976 980 981 985 989 990 991 994 995 998 999 1000 1001 1001 1004 1004 1007 1011 1014 1015 1016 1018 1019 1022 1028 1029 1030 1031 1037 1038 1042 1043 1044 1048 1052 1056 1056 1057 1071 1071 1075 1077 1077 1079 1082 1088 1090 1093 1093 1094 1095 1102 1104 1104 1105 1105 1106 1107 1108 1108 1108 1112 1112 1113 1114 1115 1123 1129 1129 1130 1132 1141 1145 1147 1153 1154 1156 1158 1165 1166 1170 1170 1174 1175 1177 1178 1179 1180 1186 1192 1193 1195 1195 1198 1201 1202 1204 1205 1206 1206 1207 1207 1208 1212 1215 1220 1221 1223 1227 1231 1237 1238 1240 1240 1242 1242 1251 1254 1255 1257 1260 1267 1267 1269 1273 1273 1275 1280 1281 1288 1292 1292 1293 1293 1294 1294 1294 1295 1299 1299 1303 1308 1308 1309 1315 1319 1320 1321 1330 1333 1335 1336 1338 1339 1341 1342 1343 1351 1351 1352 1353 1356 1363 1367 1368 1368 1368 1370 1370 1372 1374 1379 1381 1386 1391 1394 1394 1395 1395 1400 1401 1403 1405 1406 1406 1407 1407 1409 1413 1422 1430 1431 1435 1436 1440 1441 1444 1444 1446 1448 1451 1452 1455 1456 1457 1464 1466 1466 1466 1468 1471 1471 1478 1479 1482 1482 1485 1485 1493 1500 1502 1502 1503 1507 1508 1508 1514 1516 1516 1519 1519 1519 1522 1523 1529 1531 1533 1541 1547 1547 1554 1554 1557 1558 1558 1558 1559 1560 1563 1564 1564 1565 1567 1569 1572 1583 1592 1593 1594 1596 1598 1599 1603 1603 1603 1606 1615 1615 1616 1616 1617 1618 1622 1622 1622 1623 1624 1629 1629 1630 1630 1634 1642 1645 1645 1650 1657 1659 1659 1661 1661 1664 1666 1667 1671 1671 1672 1673 1678 1679 1691 1692 1696 1697 1702 1704 1704 1704 1705 1705 1707 1712 1716 1717 1719 1721 1723 1728 1730 1730 1731 1733 1735 1740 1745 1746 1746 1751 1755 1758 1758 1759 1759 1763 1766 1770 1774 1776 1778 1785 1785 1785 1786 1789 1792 1792 1793 1803 1806 1806 1809 1810 1810 1813 1815 1817 1818 1820 1820 1820 1820 1831 1832 1832 1843 1845 1847 1847 1849 1853 1854 1858 1859 1860 1865 1867 1871 1873 1877 1877 1879 1882 1886 1886 1889 1889 1891 1893 1896 1902 1906 1907 1909 1910 1914 1916 1916 1922 1922 1923 1931 1932 1934 1937 1941 1944 1945 1945 1948 1952 1952 1956 1956 1957 1958 1960 1960 1961 1967 1971 1973 1974 1976 1978 1981 1983 1983 1983 1986 1990 1990 1995 1996 2001 2003 2009 2010 2010 2017 2022 2023 2029 2033 2035 2038 2041 2042 2042 2044 2044 2047 2048 2051 2053 2057 2058 2058 2065 2070 2070 2072 2075 2075 2079 2084 2084 2087 2088 2093 2095 2097 2098 2100 2103 2103 2104 2106 2108 2108 2108 2109 2109 2110 2123 2123 2124 2124 2130 2134 2136 2139 2141 2144 2145 2155 2157 2157 2164 2164 2169 2169 2171 2181 2182 2185 2185 2188 2189 2194 2195 2196 2198 2201 2203 2203 2205 2206 2207 2209 2210 2211 2211 2212 2215 2216 2217 2221 2221 2223 2231 2235 2235 2237 2237 2238 2238 2238 2239 2251 2255 2256 2263 2270 2270 2281 2282 2285 2286 2288 2293 2294 2294 2294 2295 2297 2302 2304 2304 2307 2308 2310 2312 2312 2316 2318 2319 2320 2324 2331 2332 2335 2338 2344 2348 2349 2350 2351 2351 2351 2352 2352 2352 2352 2352 2352 2357 2366 2368 2371 2374 2375 2381 2383 2384 2394 2398 2399 2402 2403 2405 2405 2407 2407 2407 2407 2407 2413 2420 2421 2432 2435 2442 2444 2445 2447 2447 2448 2449 2451 2451 2452 2455 2462 2464 2465 2465 2465 2466 2468 2475 2475 2480 2480 2483 2494 2494 2494 2497 2498 2499 2501 2504 2506 2506 2507 2508 2508 2510 2512 2515 2515 2520 2533 2534 2544 2545 2545 2548 2548 2548 2549 2549 2561 2561 2561 2562 2567 2568 2569 2576 2576 2577 2578 2579 2579 2589 2593 2593 2593 2593 2598 2601 2605 2607 2607 2608 2611 2611 2614 2614 2618 2621 2625 2630 2634 2635 2641 2646 2646 2648 2648 2648 2649 2651 2657 2658 2661 2662 2662 2663 2664 2664 2669 2671 2673 2677 2680 2680 2682 2683 2692 2693 2694 2698 2698 2705 2706 2706 2706 2708 2717 2718 2720 2724 2730 2734 2735 2736 2739 2745 2747 2752 2758 2759 2761 2761 2762 2764 2765 2770 2771 2774 2774 2774 2775 2776 2777 2777 2777 2779 2781 2783 2787 2794 2795 2796 2797 2797 2801 2804 2804 2805 2807 2811 2811 2811 2819 2820 2820 2827 2831 2834 2836 2839 2843 2857 2860 2861 2862 2863 2863 2866 2871 2875 2876 2878 2880 2884 2886 2890 2891 2891 2891 2892 2896 2897 2897 2900 2902 2903 2905 2908 2908 2910 2910 2910 2911 2914 2918 2924 2928 2932 2933 2933 2934 2939 2942 2944 2953 2958 2959 2960 2960 2960 2960 2963 2964 2964 2967 2976 2978 2979 2981 2990 2993 2993 2994 2997 2997 2997 2997 2998 2999 3004 3013 3015 3018 3023 3024 3024 3025 3029 3033 3039 3043 3046 3047 3047 3047 3049 3049 3052 3054 3055 3057 3060 3060 3061 3065 3066 3066 3066 3067 3071 3073 3077 3079 3081 3082 3086 3091 3094 3094 3095 3096 3097 3100 3114 3118 3121 3123 3126 3130 3137 3137 3138 3146 3146 3146 3150 3152 3153 3153 3156 3160 3160 3161 3162 3163 3170 3174 3176 3178 3180 3180 3180 3181 3182 3183 3183 3183 3186 3187 3192 3193 3195 3196 3196 3198 3199 3205 3210 3212 3218 3221 3222 3223 3227 3233 3237 3239 3240 3249 3251 3251 3251 3252 3259 3259 3263 3264 3265 3274 3275 3277 3277 3280 3284 3286 3288 3293 3293 3294 3295 3295 3296 3299 3300 3306 3308 3309 3309 3309 3311 3311 3316 3322 3323 3327 3332 3336 3339 3343 3343 3346 3346 3349 3350 3350 3352 3356 3359 3364 3364 3364 3364 3368 3378 3378 3379 3380 3381 3381 3388 3389 3390 3395 3396 3396 3396 3408 3408 3409 3409 3409 3412 3412 3413 3423 3424 3437 3442 3442 3445 3447 3449 3449 3450 3451 3451 3453 3456 3458 3459 3460 3463 3463 3463 3474 3477 3477 3482 3482 3489 3491 3492 3492 3492 3493 3495 3502 3505 3506 3506 3508 3509 3510 3510 3512 3513 3515 3522 3525 3536 3537 3544 3550 3550 3550 3550 3550 3552 3552 3554 3555 3558 3559 3563 3573 3574 3576 3582 3583 3586 3589 3591 3600 3605 3605 3605 3605 3605 3605 3606 3606 3606 3607 3608 3609 3613 3619 3622 3625 3626 3633 3637 3638 3639 3641 3645 3645 3647 3649 3650 3653 3653 3655 3660 3662 3663 3663 3663 3664 3669 3671 3672 3675 3676 3687 3687 3694 3701 3702 3706 3718 3719 3719 3719 3720 3720 3722 3722 3726 3734 3736 3736 3740 3741 3742 3745 3746 3746 3747 3748 3750 3751 3752 3754 3754 3756 3759 3761 3762 3763 3768 3769 3772 3772 3775 3776 3786 3788 3788 3793 3793 3800 3800 3802 3812 3813 3816 3818 3821 3823 3827 3833 3833 3834 3834 3847 3848 3848 3849 3849 3849 3851 3853 3854 3854 3857 3859 3860 3862 3864 3869 3870 3873 3873 3878 3882 3882 3885 3887 3887 3892 3899 3899 3900 3904 3906 3909 3910 3913 3913 3915 3915 3916 3919 3922 3924 3928 3934 3935 3940 3947 3947 3948 3954 3956 3961 3962 3967 3967 3971 3974 3974 3974 3976 3979 3981 3983 3984 3986 3990 3996 3997 3997 3999 4000 4001 4001 4005 4005 4009 4012 4012 4013 4016 4020 4023 4025 4026 4034 4035 4035 4041 4041 4043 4047 4048 4050 4051 4055 4061 4064 4066 4068 4068 4071 4071 4075 4078 4080 4080 4084 4086 4090 4092 4097 4098 4099 4103 4104 4108 4110 4110 4112 4114 4125 4125 4126 4137 4137 4137 4137 4139 4140 4142 4144 4147 4147 4148 4151 4151 4154 4164 4165 4165 4168 4171 4172 4172 4172 4179 4181 4183 4187 4191 4194 4198 4198 4199 4199 4202 4206 4211 4211 4212 4217 4222 4224 4226 4227 4227 4229 4234 4236 4238 4240 4241 4245 4250 4252 4252 4253 4253 4253 4255 4260 4261 4265 4266 4278 4279 4284 4285 4286 4286 4290 4291 4293 4296 4296 4298 4298 4300 4307 4312 4312 4315 4323 4327 4327 4328 4328 4333 4334 4335 4335 4335 4339 4340 4341 4341 4342 4342 4351 4354 4354 4354 4358 4359 4361 4363 4364 4365 4374 4385 4388 4390 4392 4393 4393 4394 4397 4398 4399 4399 4399 4400 4403 4403 4410 4410 4416 4424 4426 4428 4434 4435 4438 4438 4438 4441 4441 4443 4449 4449 4450 4454 4455 4455 4457 4464 4472 4472 4475 4475 4478 4479 4486 4486 4489 4491 4491 4491 4493 4500 4501 4502 4505 4506 4509 4511 4513 4513 4516 4517 4521 4522 4526 4527 4535 4544 4548 4550 4550 4551 4551 4552 4554 4556 4557 4562 4562 4563 4563 4566 4571 4576 4578 4583 4585 4587 4587 4589 4589 4589 4590 4594 4601 4604 4605 4606 4606 4614 4615 4616 4618 4619 4621 4622 4624 4627 4636 4637 4638 4642 4648 4649 4649 4654 4658 4658 4662 4663 4663 4663 4664 4664 4665 4665 4669 4676 4677 4682 4684 4684 4688 4690 4690 4697 4700 4702 4703 4706 4715 4715 4717 4717 4719 4720 4726 4730 4734 4736 4737 4742 4745 4749 4750 4750 4751 4751 4752 4753 4755 4756 4759 4762 4762 4764 4765 4771 4777 4778 4779 4780 4782 4783 4787 4787 4791 4792 4799 4801 4803 4804 4810 4812 4816 4825 4827 4828 4828 4834 4842 4843 4844 4845 4845 4849 4849 4849 4850 4851 4852 4852 4853 4853 4855 4862 4863 4864 4864 4867 4869 4875 4878 4880 4880 4882 4886 4886 4900 4901 4901 4905 4909 4913 4914 4915 4919 4920 4926 4927 4928 4929 4935 4938 4939 4941 4942 4943 4946 4950 4953 4953 4956 4956 4957 4958 4959 4962 4963 4966 4967 4968 4972 4976 4977 4981 4981 4983 4992 4992 4996 5000 5006 5008 5010 5012 5013 5014 5014 5014 5014 5016 5025 5027 5027 5032 5035 5039 5040 5040 5042 5049 5054 5055 5055 5057 5057 5066 5068 5068 5069 5069 5069 5071 5072 5076 5082 5084 5090 5091 5093 5097 5101 5105 5105 5109 5111 5113 5113 5113 5116 5119 5121 5122 5124 5127 5127 5128 5128 5129 5141 5142 5142 5148 5153 5154 5155 5155 5155 5163 5166 5166 5167 5169 5170 5170 5172 5179 5179 5179 5181 5188 5196 5196 5200 5202 5204 5208 5212 5215 5216 5217 5218 5218 5221 5222 5227 5228 5228 5229 5230 5234 5241 5241 5254 5255 5256 5256 5257 5259 5260 5262 5263 5266 5267 5267 5270 5276 5278 5278 5280 5283 5283 5284 5285 5285 5291 5295 5304 5305 5305 5309 5310 5316 5316 5317 5326 5328 5331 5335 5335 5335 5341 5342 5342 5344 5346 5355 5356 5357 5359 5363 5364 5364 5365 5365 5367 5367 5368 5371 5375 5381 5384 5385 5391 5391 5394 5398 5398 5398 5399 5402 5404 5404 5406 5408 5408 5411 5415 5418 5418 5419 5422 5423 5429 5439 5445 5445 5447 5452 5454 5456 5464 5465 5465 5466 5468 5472 5472 5473 5478 5482 5484 5487 5489 5491 5493 5494 5495 5502 5503 5504 5505 5505 5507 5511 5512 5512 5519 5521 5521 5521 5526 5528 5528 5531 5532 5532 5538 5542 5546 5548 5549 5550 5551 5553 5553 5557 5559 5561 5565 5566 5569 5579 5585 5586 5590 5592 5594 5594 5595 5601 5603 5603 5605 5606 5608 5608 5615 5615 5615 5625 5627 5627 5628 5633 5634 5638 5639 5640 5643 5645 5647 5647 5650 5658 5658 5658 5658 5662 5663 5666 5666 5666 5668 5670 5679 5680 5680 5684 5691 5693 5693 5695 5697 5698 5701 5704 5706 5707 5707 5712 5714 5714 5716 5728 5729 5729 5730 5730 5731 5740 5741 5742 5745 5745 5753 5753 5755 5759 5759 5763 5763 5769 5769 5771 5771 5771 5771 5771 5794 5794 5794 5794 5794 5799 5801 5801 5803 5810 5810 5810 5820 5820 5826 5829 5829 5829 5842 5843 5843 5843 5843 5844 5844 5844 5844 5844 5844 5854 5854 5854 5856 5856 5856 5856 5856 5856 5858 5858 5858 5860 5860 5860 5870 5870 5870 5886 5900 5900 5957"
//val spec = input.split(" ").map(_.toInt)
//val s = CycloSequencer(spec)
//val peptide = Protein("PGHIVSQSLYFYQQDLTCTIILHWIVSTYPWYNMFFIPTSARPTNNYTTP").map(ProteinWeights.weightOf)
//s.spectrumCounts
//s.score(peptide)
