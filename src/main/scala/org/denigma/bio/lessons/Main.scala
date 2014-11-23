package org.denigma.bio

import java.io.File

import me.shadaj.genalgo.codontable.StandardTable
import me.shadaj.genalgo.sequences._
import org.denigma.bio.lessons.peptides._
import org.denigma.bio.lessons.peptides.sequencer.{LinearSequencer, ConvolutionCycloSequencer, SimpleCycloSequencer, CycloSequencer}
import org.denigma.bio.lessons.peptides.spectrum.{LinearSpectrum, CycloSpectrum, Convolution}

import scala.io.Source


object Main extends scala.App{

  def specFromStr(spec:String) = spec.split(' ').map(v=>v.toInt)

  def conv(spec:Seq[Int]) = {
    object Conv extends Convolution
    val res = Conv.getConvolution(spec).groupBy(c=>c).toSeq.sortWith((a,b)=>a._2.size>b._2.size)
    print(res)
  }
  //conv(specFromStr("0 57 118 179 236 240 301"))


  def linearScoreFromSpec(spec:Seq[Int], p:Protein) = {
    val ms = ProteinWeights.massesOf(p)
    val cs = CycloSequencer(spec)
    val s= cs.linearScore(ms)
    println(p)
    println(s)
  }

  val prots = Seq(
    strToProtein("QCV"),
  strToProtein("TVQ"),
      strToProtein("CTV"),
      strToProtein("VAQ"),
      strToProtein("AQV"),
      strToProtein("AVQ")
  )
  //peptideConsistentWithLinearSpectrum(specFromStr("0 71 99 101 103 128 129 199 200 204 227 230 231 298 303 328 330 332 333"),prots)

  //scoreFromSpec(specFromStr("0 71 71 71 131 131 131 156 198 199 199 202 202 202 333 333 333 404 404"),  strToProtein("MAMA"))
  //linearScoreFromSpec(specFromStr("0 97 129 129 129 194 226 323 323 355 452"),strToProtein("PEEP"))

  def peptideConsistentWithLinearSpectrum(spe:Seq[Int],prots:Seq[Protein])  = {
    val ss = LinearSequencer(spe)
    val pvs = prots.map(p=>p->ProteinWeights.massesOf(p)).toMap
    val res = pvs.collect{case (key,value) if LinearSpectrum(value).value.forall(ss.spectrum.contains)=>key}.toList
    println(res.mkString(" "))
  }


  def scoreFromSpec(spec:Seq[Int], p:Protein) = {
    val ms = ProteinWeights.massesOf(p)
    val cs = CycloSequencer(spec)
    val s= cs.score(ms)
    println(p)
    println(s)
  }

  def peptidesFromLinearSpectrum(spe:Seq[Int]) = {
    val ss = LinearSequencer(spe)
    val result = ss.bestMatches
    val prots = result.flatMap(r=>ProteinWeights.candidateProteins(r.toList))
    println(prots)
  }


  def strToProtein(str:String) = Protein(str.map(s=>AminoAcid.fromChar(s)))

  //peptideConsistentWithLinearSpectrum(specFromStr("0 71 99 101 103 128 129 199 200 204 227 230 231 298 303 328 330 332 333"),Seq("AQV", "TCE", "TCQ",  "CET",  "CTV",  "CTQ").map(s=>strToProtein(s)))



  //peptidesFromCycloSpectrum(specFromStr("0 71 101 113 131 184 202 214 232 285 303 315 345 416"))

  def peptidesFromCycloSpectrum(spe:Seq[Int]) = {
    val ss = SimpleCycloSequencer(spe)
    val result = ss.bestMatches
    val prots = result.flatMap(r=>ProteinWeights.candidateProteins(r.toList))
    println(prots)
  }


  def proteinDNANumber(prot:Protein) = {
    val num = prot.foldLeft(1)( (acc,el)=> acc * StandardTable.codonsForAmino(el).size   )
    println(num)
  }

}
  trait StepicTasks {

  def convolutionSpectrum() = {
    val m = 20
    val n =1000
    val input = "0 97 99 113 114 115 128 128 147 147 163 186 227 241 242 244 244 256 260 261 262 283 291 309 330 333 340 347 385 388 389 390 390 405 435 447 485 487 503 504 518 544 552 575 577 584 599 608 631 632 650 651 653 672 690 691 717 738 745 770 779 804 818 819 827 835 837 875 892 892 917 932 932 933 934 965 982 989 1039 1060 1062 1078 1080 1081 1095 1136 1159 1175 1175 1194 1194 1208 1209 1223 1322" // "57 57 71 99 129 137 170 186 194 208 228 265 285 299 307 323 356 364 394 422 493"

    //val m = 18//19
    //val n = 342//343
    //val input = "384 1318 147 271 749 696 512 1446 1071 114 746 115 991 284 683 992 1048 480 1081 1176 365 515 864 1446 568 1048 171 1332 700 185 128 455 1445 246 811 128 218 901 1326 830 697 1404 1080 1473 1291 1048 348 1020 977 602 384 1413 967 269 1158 991 1261 1503 512 228 479 1432 1319 877 1195 1342 569 750 1227 864 1446 1262 299 1162 1162 593 462 489 1289 87 1045 1106 1404 636 1176 131 749 1276 398 427 1098 924 454 1163 618 113 1429 1389 860 917 696 242 942 773 787 594 402 1105 397 1029 1212 1261 659 569 531 877 626 241 583 1447 234 1299 57 156 412 708 863 1185 1314 1560 298 1133 1375 540 114 643 1290 375 810 1148 270 333 683 512 114 0 811 966 398 261 934 1432 156 730 958 814 299 852"

    val spec = input.split(' ').map(_.toInt)
    val cs = ConvolutionCycloSequencer(spec,m)
    //print(cs.bestMatches(n))
    val res = cs.bestMatches(n).toList
    println(res)
    val str = cs.prettyString(res.map(v=>v._2))
    println(str)
  }

  def convolution() = {
    val input = "358 230 1023 487 681 545 651 394 101 1160 394 731 252 1045 766 837 172 271 408 930 0 895 908 867 802 673 1004 780 766 380 129 1031 115 536 408 495 988 802 323 752 1023 594 615 71 301 156 257 186 665 128 495 903 509 665 859 257 550 752 610 566 293 137 974 479 1089 137 1032 903 1059 624 265 429 358 889"
    val spectrum = input.split(' ').map(_.toInt).sorted
    //object Conv extends Convolution
    val cs = ConvolutionCycloSequencer(spectrum)
    val conv = cs.getConvolution(spectrum).mkString(" ")

    //val conv = Conv.getConvolution(spectrum).mkString(" ")
    println(conv)

  }

  def noisySeq() = {
    //val input = "0 97 99 113 114 115 128 128 147 147 163 186 227 241 242 244 244 256 260 261 262 283 291 309 330 333 340 347 385 388 389 390 390 405 435 447 485 487 503 504 518 544 552 575 577 584 599 608 631 632 650 651 653 672 690 691 717 738 745 770 779 804 818 819 827 835 837 875 892 892 917 932 932 933 934 965 982 989 1039 1060 1062 1078 1080 1081 1095 1136 1159 1175 1175 1194 1194 1208 1209 1223 1322"
    val input = "0 97 99 113 114 115 128 128 147 147 163 186 227 241 242 244 244 256 260 261 262 283 291 309 330 333 340 347 385 388 389 390 390 405 435 447 485 487 503 504 518 544 552 575 577 584 599 608 631 632 650 651 653 672 690 691 717 738 745 770 779 804 818 819 827 835 837 875 892 892 917 932 932 933 934 965 982 989 1039 1060 1062 1078 1080 1081 1095 1136 1159 1175 1175 1194 1194 1208 1209 1223 1322"
    val spec = input.split(" ").map(_.toInt)
    val n = 1000
    val s = CycloSequencer(spec)
    val res = s.trimWithScore(s.bestMatches(n),1).toList
    println(res)
    println(s"---------${res.size}---${res.count(kv=>kv._1>=82)}-------------")
    println(s.prettyString(res.map(v=>v._2)))
  }


  def leaderBoardCyclopeptideSequencing() = {

    //val input =  "0 71 71 71 87 97 97 99 101 103 113 113 114 115 128 128 129 137 147 163 163 170 184 184 186 186 190 211 215 226 226 229 231 238 241 244 246 257 257 276 277 278 299 300 312 316 317 318 318 323 328 340 343 344 347 349 356 366 370 373 374 391 401 414 414 415 419 427 427 431 437 441 446 453 462 462 462 470 472 502 503 503 511 515 529 530 533 533 540 543 547 556 559 569 574 575 584 590 600 600 604 612 616 617 630 640 640 643 646 648 660 671 683 684 687 693 703 703 719 719 719 729 730 731 737 740 741 745 747 754 774 780 784 790 797 800 806 818 826 827 832 833 838 846 846 847 850 868 869 877 884 889 893 897 903 908 913 917 930 940 947 956 960 960 961 964 965 966 983 983 985 1002 1009 1010 1011 1021 1031 1031 1036 1053 1054 1058 1059 1062 1063 1074 1076 1084 1092 1103 1113 1122 1124 1130 1133 1134 1145 1146 1146 1149 1150 1155 1156 1171 1173 1174 1187 1191 1193 1200 1212 1221 1233 1240 1242 1246 1259 1260 1262 1277 1278 1283 1284 1287 1287 1288 1299 1300 1303 1309 1311 1320 1330 1341 1349 1357 1359 1370 1371 1374 1375 1379 1380 1397 1402 1402 1412 1422 1423 1424 1431 1448 1450 1450 1467 1468 1469 1472 1473 1473 1477 1486 1493 1503 1516 1520 1525 1530 1536 1540 1544 1549 1556 1564 1565 1583 1586 1587 1587 1595 1600 1601 1606 1607 1615 1627 1633 1636 1643 1649 1653 1659 1679 1686 1688 1692 1693 1696 1702 1703 1704 1714 1714 1714 1730 1730 1740 1746 1749 1750 1762 1773 1785 1787 1790 1793 1793 1803 1816 1817 1821 1829 1833 1833 1843 1849 1858 1859 1864 1877 1886 1890 1893 1900 1900 1903 1904 1918 1922 1930 1930 1931 1961 1963 1971 1971 1971 1980 1987 1992 1996 2002 2006 2006 2014 2018 2019 2019 2032 2042 2059 2060 2063 2067 2077 2084 2086 2089 2090 2093 2105 2110 2115 2115 2116 2117 2121 2133 2134 2155 2156 2157 2176 2176 2187 2189 2192 2195 2202 2204 2207 2207 2218 2222 2243 2247 2247 2249 2249 2263 2270 2270 2286 2296 2304 2305 2305 2318 2319 2320 2320 2330 2332 2334 2336 2336 2346 2362 2362 2362 2433"
    val n = 357
    val input = "0 87 87 87 101 103 113 114 114 114 128 128 131 131 137 137 147 163 163 163 186 186 186 201 224 234 234 234 238 244 245 250 250 251 256 264 266 277 299 300 300 300 314 314 317 321 338 338 347 348 364 367 387 387 397 397 401 401 425 428 428 430 430 431 431 435 442 442 463 484 485 488 498 501 504 510 511 524 533 544 550 556 556 559 561 564 572 572 588 591 598 598 611 617 625 628 635 638 651 659 664 667 675 678 687 696 697 712 719 725 730 735 735 739 742 742 745 748 754 778 784 788 797 798 806 822 822 825 826 827 849 856 858 861 872 873 873 885 891 898 902 905 911 912 928 934 936 941 953 959 964 973 985 986 987 989 989 992 998 1005 1019 1022 1026 1035 1040 1042 1059 1060 1065 1092 1092 1097 1099 1106 1117 1122 1122 1127 1135 1136 1136 1139 1150 1154 1163 1172 1173 1179 1184 1220 1223 1223 1223 1226 1228 1228 1236 1237 1239 1249 1250 1253 1255 1285 1286 1291 1303 1315 1321 1326 1336 1336 1337 1340 1340 1342 1356 1365 1370 1378 1383 1384 1386 1406 1413 1417 1422 1423 1429 1435 1449 1452 1454 1457 1471 1477 1483 1484 1489 1493 1500 1520 1522 1523 1528 1536 1541 1550 1564 1566 1566 1569 1570 1570 1580 1585 1591 1603 1615 1620 1621 1651 1653 1656 1657 1667 1669 1670 1678 1678 1680 1683 1683 1683 1686 1722 1727 1733 1734 1743 1752 1756 1767 1770 1770 1771 1779 1784 1784 1789 1800 1807 1809 1814 1814 1841 1846 1847 1864 1866 1871 1880 1884 1887 1901 1908 1914 1917 1917 1919 1920 1921 1942 1947 1953 1965 1970 1972 1978 1994 1995 2001 2004 2008 2015 2021 2033 2033 2034 2045 2048 2050 2057 2079 2080 2081 2084 2084 2100 2108 2109 2118 2122 2128 2152 2158 2161 2164 2164 2167 2171 2171 2176 2181 2187 2194 2209 2210 2219 2228 2231 2239 2242 2247 2255 2268 2271 2278 2281 2289 2295 2308 2308 2315 2318 2334 2334 2342 2345 2347 2350 2350 2356 2362 2373 2382 2395 2396 2402 2405 2408 2418 2421 2422 2443 2464 2464 2471 2475 2475 2476 2476 2478 2478 2481 2505 2505 2509 2509 2519 2519 2539 2542 2558 2559 2568 2568 2585 2589 2592 2592 2606 2606 2606 2607 2629 2640 2642 2650 2655 2656 2656 2661 2662 2668 2672 2672 2672 2682 2705 2720 2720 2720 2743 2743 2743 2759 2769 2769 2775 2775 2778 2778 2792 2792 2792 2793 2803 2805 2819 2819 2819 2906"

 //   val input = "0 71 113 129 147 200 218 260 313 331 347 389 460"
//    val n = 10

    val spec = input.split(" ").map(_.toInt)

    val s = CycloSequencer(spec)
    val res = s.best(n)
    val str = s.prettyProtein(res.get._2)
    println(str)
  }

    def cycloScore() = {

      val pep = "NQEL"
      val inp = "0 99 113 114 128 227 257 299 355 356 370 371 484"
      val input = inp.split(" ").map(_.toInt)
      val prot = Protein(pep.map(AminoAcid.fromChar))
      val ms = ProteinWeights.massesOf(prot)
      val cs = CycloSequencer(input)
      println(s"SCORE:\n${cs.score(ms)}")

    }



  def simpleCycloSequencer() = {
    //copypasted here, but should be from file
    //val inp = "0 101 128 131 137 137 147 147 163 232 248 265 268 275 284 300 310 369 379 385 396 412 438 447 447 497 516 516 543 548 575 575 594 644 644 653 679 695 706 712 722 781 791 807 816 823 826 843 859 928 944 944 954 954 960 963 990 1091"
    val inp = "0 113 128 186 241 299 314 427"
    val input = inp.split(" ").map(v => v.toInt).toList

    val s = SimpleCycloSequencer(input)
    println(s.stringValue)
  }

  def loadLeaderBoard() = {


    val sc = Source.fromFile("/home/antonkulaga/Downloads/dataset_4913_3 (2).txt").getLines.toList
    val protoString =sc.head.split(" ").mkString(" ")
    val protos = protoString.split(" ").map(a=> Protein(a.map(AminoAcid.fromChar)))
    val input = sc.tail.head
    val n = sc.tail.tail.head.toInt
    val protomaps = protos.map(p=>ProteinWeights.massesOf(p)->p).toMap
    val protomasses = protos.map(p=>ProteinWeights.massesOf(p))
    val cs = CycloSequencer.fromMasses(input)
    val lead = cs.linearLeaderboard(protomasses)
    val lnew = cs.trim(lead,n)
    val leaders = lnew.map(m=>protomaps(m.toIndexedSeq))
    val res =leaders.mkString(" ")
    println(res)

  }
}
