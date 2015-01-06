//Part 1 - Function Composition

// a. Composition Function

def compose(f: Int=>Int, g: Int=>Int): Int=>Int = {

	// apply g on x, and then apply g(x) on f
	(x:Int) => f(g(x))

}

/* test

val square = (x:Int) => x*x
val inc = (_ : Int) + 1
val squareinc = compose(square, inc)

println(squareinc(6))

*/



// b. Repeated applicatoin

def repeat(f: Int=>Int, n: Int): Int=>Int = {

	// the function that we will return
	var result: Int=>Int = f
	// to count the times of application of f
	var count:Int = n

	// if n is 1 or less, simply return f
	if( count <= 1 ) f
	else {
		// keep applying f
		while(count > 1){
			// composing f on itself
			result = compose(f,result)
			count -= 1
		}
		// return the result function
		result
	}

}

/* test
val square = (x:Int) => x*x
val square4 = repeat(square, 4)
println(square4(2))
*/
