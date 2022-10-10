# Prime Power Numbers

PN Set - the set of all numbers that can be formed with every combination of
primes {P} from 2 to P and all powers {N} from 0 to N. {P,N}

{P} :: set of primes, 2 to P
{N} :: set of powers, 0 to N
|P| count of primes
|N| count of powers
|{P,N}| = |N|^|P|

Base - Every PN Set has a base which is the max of the |P| and |N|


## Prime Notation

[7's place].[5's place].[3's place].[2's place]
1.0.1.3 :: 7^1 * 5^0 * 3^1 * 2^3 == 168

We include leading zeros such that the total number of digits is equal to the
base of the PN number. e.g. 0.0.3 rather than 3 or 0.0.0.3


## First few prime power numbers

pn set : pn : decimal value
|P|,N  : pn : decimal value

1,0 : _._.0 : 1

1,1 : _._.1 : 2

2,1 : _.1.0 : 3
2,1 : _.1.1 : 6

2,2 : _.0.2 : 4
2,2 : _.1.2 : 12
2,2 : _.2.0 : 9
2,2 : _.2.1 : 18
2,2 : _.2.2 : 36

3,2 : 1.0.0 : 5
3,2 : 1.0.1 : 10
3,2 : 1.0.2 : 20
3,2 : 1.1.0 : 15
3,2 : 1.1.1 : 30
3,2 : 1.1.2 : 60
3,2 : 1.2.0 : 45
3,2 : 1.2.1 : 90
3,2 : 1.2.2 : 180
3,2 : 2.0.0 : 25
3,2 : 2.0.1 : 50
3,2 : 2.0.2 : 100
3,2 : 2.1.0 : 75
3,2 : 2.1.1 : 150
3,2 : 2.1.2 : 300
3,2 : 2.2.0 : 225
3,2 : 2.2.1 : 450
3,2 : 2.2.2 : 900

3,3 : 0.0.3 : 8
3,3 : 0.1.3 : 24
3,3 : 0.2.3 : 72
3,3 : 0.3.0 : 27
3,3 : 0.3.1 : 54
3,3 : 0.3.2 : 108
3,3 : 0.3.3 : 216
3,3 : 1.0.3 : 40
3,3 : 1.1.3 : 120
3,3 : 1.2.3 : 360
3,3 : 1.3.0 : 135
3,3 : 1.3.1 : 270
3,3 : 1.3.2 : 540
3,3 : 1.3.3 : 1080
3,3 : 2.0.3 : 200
3,3 : 2.1.3 : 600
3,3 : 2.2.3 : 1800
3,3 : 2.3.0 : 675
3,3 : 2.3.1 : 1350
3,3 : 2.3.2 : 2700
3,3 : 2.3.3 : 5400
3,3 : 3.0.0 : 125
3,3 : 3.0.1 : 250
3,3 : 3.0.2 : 500
3,3 : 3.0.3 : 1000
3,3 : 3.1.0 : 375
3,3 : 3.1.1 : 750
3,3 : 3.1.2 : 1500
3,3 : 3.1.3 : 3000
3,3 : 3.2.0 : 1125
3,3 : 3.2.1 : 2250
3,3 : 3.2.2 : 4500
3,3 : 3.2.3 : 9000
3,3 : 3.3.0 : 3375
3,3 : 3.3.1 : 6750
3,3 : 3.3.2 : 13500
3,3 : 3.3.3 : 27000

4,3 : 1.0.0.0 : 7
4,3 : 1.0.0.1 : 14
4,3 : 1.0.0.2 : 28
4,3 : 1.0.0.3 : 46
4,3 : 1.0.1.0 : 21
4,3 : 1.0.1.1 : xx
4,3 : 1.0.1.2 : xx
4,3 : 1.0.1.3 : xx
4,3 : 1.0.2.0 : xx
4,3 : 1.0.2.1 : xx
4,3 : 1.0.2.2 : xx
4,3 : 1.0.2.3 : xx
4,3 : 1.0.3.0 : xx
4,3 : 1.0.3.1 : xx
4,3 : 1.0.3.2 : xx
4,3 : 1.0.3.3 : xx
4,3 : 1.1.0.0 : 7
4,3 : 1.1.0.1 : 14
4,3 : 1.1.0.2 : 28
4,3 : 1.1.0.3 : 46
4,3 : 1.1.1.0 : 21
4,3 : 1.1.1.1 : xx
4,3 : 1.1.1.2 : xx
4,3 : 1.1.1.3 : xx
4,3 : 1.1.2.0 : xx
4,3 : 1.1.2.1 : xx
4,3 : 1.1.2.2 : xx
4,3 : 1.1.2.3 : xx
4,3 : 1.1.3.0 : xx
4,3 : 1.1.3.1 : xx
4,3 : 1.1.3.2 : xx
4,3 : 1.1.3.3 : xx
4,3 : 1.2.0.0 : 7
4,3 : 1.2.0.1 : 14
4,3 : 1.2.0.2 : 28
4,3 : 1.2.0.3 : 46
4,3 : 1.2.1.0 : 21
4,3 : 1.2.1.1 : xx
4,3 : 1.2.1.2 : xx
4,3 : 1.2.1.3 : xx
4,3 : 1.2.2.0 : xx
4,3 : 1.2.2.1 : xx
4,3 : 1.2.2.2 : xx
4,3 : 1.2.2.3 : xx
4,3 : 1.2.3.0 : xx
4,3 : 1.2.3.1 : xx
4,3 : 1.2.3.2 : xx
4,3 : 1.2.3.3 : xx
4,3 : 1.3.0.0 : 7
4,3 : 1.3.0.1 : 14
4,3 : 1.3.0.2 : 28
4,3 : 1.3.0.3 : 46
4,3 : 1.3.1.0 : 21
4,3 : 1.3.1.1 : xx
4,3 : 1.3.1.2 : xx
4,3 : 1.3.1.3 : xx
4,3 : 1.3.2.0 : xx
4,3 : 1.3.2.1 : xx
4,3 : 1.3.2.2 : xx
4,3 : 1.3.2.3 : xx
4,3 : 1.3.3.0 : xx
4,3 : 1.3.3.1 : xx
4,3 : 1.3.3.2 : xx
4,3 : 1.3.3.3 : xx
4,3 : 2.0.0.0 : 49
4,3 : ...
4,3 : 3.3.3.3 : 9261000

4,4 : 0.0.0.4 : 16
4,4 : 0.0.1.4 : xx
4,4 : 0.0.2.4 : xx
4,4 : 0.0.3.4 : xx
4,4 : 0.0.4.0 : xx
4,4 : 0.0.4.1 : xx
4,4 : 0.0.4.2 : xx
4,4 : 0.0.4.3 : xx
4,4 : 0.0.4.4 : xx
4,4 : 0.1.0.4 : xx
4,4 : 0.1.1.4 : xx
4,4 : 0.1.2.4 : xx
4,4 : 0.1.3.4 : xx
4,4 : 0.1.4.0 : xx
4,4 : 0.1.4.1 : xx
4,4 : 0.1.4.2 : xx
4,4 : 0.1.4.3 : xx
4,4 : 0.1.4.4 : xx
4,4 : 0.2.0.4 : xx

4,4 : 4.4.4.4 : 1944810000