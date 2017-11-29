def isPrime(i: Int): Boolean = {
  if (i <= 1)
    false
  else if (i == 2)
    true
  else
    !(2 to (i - 1)).exists(x => i % x == 0)
}


isPrime(11)

def twinPrimes(n: Int, r: Int): Boolean = {
    if(isPrime(n) && isPrime(r)) {
        if ( (r - n == 2) ||(n - r == 2) )
            return true
        else
            false
    }
    else
        false
}

twinPrimes(7,11)
twinPrimes(17,19)

/*Twin Primes List (1 points). Create a function, named twinprimeslist,
that takes an integer, n, parameter and returns an integer list of all
the twin primes starting up to n. For example, twinprimeslist (50) should
return [3, 5, 7, 11, 13, 17, 19, 29, 31, 41, 43] (no duplicates).*/

def twinPrimesList(num : Int): List[Int] = {
    num match {
        case a if a < 3 => Nil
        case _ =>
            if(twinPrimes(num - 2, num) || twinPrimes(num + 2, num)){
                num :: twinPrimesList(num - 1)
            }
            else {
                twinPrimesList(num - 1)
            }
    }
}

twinPrimesList(50)
twinPrimesList(100)

/*Goldbach’s Conjecture (2 points). Create a function, named goldbach, that
 takes an integer and prints the solution satisfying the Goldbach Conjecture.
 The Goldbach Conjecture states that every positive even number greater than 2
 is the sum of two prim numbers. For example, 28 = 5 + 23. Your function is to find
 the two prime numbers that sum up to a given even integer and print the composition.
 For example goldbach(28) would print 5 + 23 = 28. You should provide error checking
  to make sure the integer parameter is even and greater than 2.*/

def goldbach(n : Int): Unit = {
    n match {
        case n if n <= 2 => println("Yo Number is too small!")
        case n if n%2 != 0 => println("Yo Number is Odd!")
        case _ =>
            val testL = twinPrimesList(n)
            testL.filter(p => isPrime(p)) match {
                case head :: _ =>
                    if (testL.contains(n - head)) {
                        if (true) {
                            println(n - head + " + " + head + " = " + n + "\n")
                        }
                        else {
                            Nil
                        }
                    }
                   //else
                       // print("ODD!")
            }
    }
}

goldbach(50)
goldbach(7)
goldbach(-3)
goldbach(0)


