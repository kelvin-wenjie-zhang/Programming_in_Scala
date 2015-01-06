//part 1b

object part1b {

	def bob(x: =>Int):Int = { println("Bob"); x + 1}

	def joe(x: =>Int,y: =>Int):Int = {
		println("Joe");
		val a =x;
		val b = y;
		println(a+b);
		a+b
	}

	def ron(x: =>Int, y: =>Int,z: =>Int){
		println("Ron")
		val a = x;
		val b = y;
		val c = z;
		println(a+b)
		println(a+b+c)
	}

	def buggy(x: =>Int):Int = {
		if(x>=0){
			println(x)
			buggy(x-1)
		}else 0
	}

	def foo(x: =>Int, y: =>Int):Int = {
		println(x)
		x + 2
	}

	def main(args: Array[String]){

		println("Part 1 (b):")
		println("------------------------------Program 1--------------------------------")
		println("The output of Program 1 is: ")
		ron(bob(joe(bob(1),2)),3,4)
		println()
		println("Explanation: ")
		println(
			"""
			Ron --> called by the outer function ron()
			Bob --> called by the first input of ron(), which would call bob() function
			Joe --> called by the input of bob() function, which is joe(bob(1),2)
			Bob --> called by the first input of joe(), which is bob(1)
			4   --> joe(bob(1),2) would print 4
			8   --> since bob(joe(bob(1),2)) = bob(4) = 5, then in ron() function,
					a = 5, b = 3, thus a+b = 8
			12  --> in outer function ron(), print(a+b+c) = print(5+3+4) = print(12)
			""")
		println("------------------------------Program 1--------------------------------")
		println()
		println("------------------------------Program 2--------------------------------")
		println("The output of Program 2 is: ")
		println(foo(1,buggy(10)))
		println("Explanation: ")
		println(
			"""
			1   ==> called by print(x) in foo() function since x = 1
			3   ==> called by x + 2 in foo() function

			buggy(10) would not be called since buggy() now is called by reference in 
			foo() function. But foo() function does not use or call the result of buggy(),
			thus buggy would not be explicitly called.
			""")
		println("------------------------------Program 2--------------------------------")
	}
}