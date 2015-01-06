/**
 * Fall 2014 CS3101-2 - Programming Languages: Scala
 * Problem Set 1, Part 1
 *
 * Using the function collatz, write a function longest collatz, that takes an
 * integer n as its parameter and return the positive integer m, m <= n for
 * which collatz needs the largest number of steps. Add your function to the
 * file Collatz.scala.
 *
 * Answer the following questions as comments at the end of Collatz.scala:
 *
 *   - Which number m, m <= 1000, produces the longest Collatz sequence? How
 *     many steps are in this sequence?
 *   - For very large n (e.g. n = 1, 000, 000) the naive implementation of
 *     longest collatz becomes very slow. Explain why. Describe (in words)
 *     how you could improve the function to terminate faster (you do not
 *     have to implement a better solution at this point. The naive one
 *     is fine).
 */

def collatz_rec(n: Int, counter: Int) : Int = 
    if (n == 1)
        counter
    else if (n % 2 == 0)
        collatz_rec(n / 2, counter + 1)
    else
        collatz_rec(n * 3 + 1, counter + 1)


def collatz(n:Int) : Int = collatz_rec(n, 0)


// Your code here
def longest_collatz(n : Int) : Int = {

    // which number produces the longest Collatz sequence
    var large_number : Int = 0
    // how many steps in the longest Collatz sequence
    var large_step : Int = collatz(n)
    // to store the current steps
    var tmp : Int = 0
    // the current number we are looking at
    var current: Int = 1

    // while loop for current number is less than n
    while(current < n ){
      
      // calculate the steps for current
      tmp = collatz(current)
      // if it is larger than our record
      // we simply change it
      if(tmp > large_step){
        large_step = tmp
        large_number = current
      }
      current = current + 1
    }
    //println(large_step)
    large_number
}

// testing
var test:Int = longest_collatz(1000) //output 871
println(test)


/*
 * Answers to questions here
 *   - Which number m, m <= 1000, produces the longest Collatz sequence? How
 *     many steps are in this sequence?
 *
 *      Answer: "871" produces the longest Collatz sequene.
 *               There are 178 steps in this sequence.
 *
 *   - For very large n (e.g. n = 1, 000, 000) the naive implementation of
 *     longest collatz becomes very slow. Explain why. Describe (in words)
 *     how you could improve the function to terminate faster (you do not
 *     have to implement a better solution at this point. The naive one
 *     is fine).
 *
 *      Answer:
 *      That's because we keep recursively calling the collatz_rec() function for 
 *      each number. This would keep growing the stack memory, causing the implementation slow.
 *      The time complexity would be:
 *      case 1: all n's are even --> O(n*logn)
 *      case 2: all n's are odd --> O(n^2) + O(n*logn) = O(n^2)
 *      by worst case analysis, the running time for the naive implementation would be O(n^2)
 *      
 *      To improve the function to terminate faster, we can avoid using recursively calling, but instead
 *      we can use a iterative method, such as a for loop or while loop.
 *      while( n!= 1){
 *        if(n%2 == 0)
 *          n = n/2
 *        else
 *          n = 3*n + 1
 *        count += 1
 *      }
 *
 */
