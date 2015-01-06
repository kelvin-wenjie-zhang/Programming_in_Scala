/**
 * Fall 2014 CS3101-2 - Programming Languages: Scala
 * Problem Set 1, Part 2
 * 
 * a) Write a function rotate that takes a list of integers
 * and returns a new list that has been "rotated" left.
 * 
 * For instance: 
 *
 * scala> val x = rotate(1 :: 2 :: 3 :: 4 :: 5 :: Nil)
 * x: List[Int] = List(2, 3, 4, 5, 1)
 * 
 * scala> rotate(x)
 * res1: List[Int] = List(3, 4, 5, 1, 2)
 *
 * Hint: 
 *    - lists have a method head, that returns the first element 
 *      of the list. 
 *      E.g    
 *           scala> List(42,23,5).head
 *           res0: Int = 42
 * 
 *    - lists also have a method tail that returns a list containing
 *      all elements except for the first one. 
 *      E.g. 
 *          scala> List(42,23,5).tail
 *          res1: List[Int] = List(23, 5)
 *
 * b) write a recursive function rotate_n, that takes a list of
 * integers and an integer n as parameters, and rotates the list
 * n times. 
 */

// Code for part a here
def rotate(l : List[Int]) : List[Int] = {
	
	//the list method :+ would append an element to the end of the list
	l.tail:+l.head
}

// Code for part b here
def rotate_n(l : List[Int], n: Int) : List[Int] = {
	var count : Int = 0
	var result : List[Int] = l
	while(count < n){
		result = rotate(result)
		count = count + 1
	}
	result
}


//testing
var mylist: List[Int] = List(1,2,3,4,5)
println(rotate(mylist))
println(rotate(rotate(mylist)))
println(rotate_n(mylist, 4))

