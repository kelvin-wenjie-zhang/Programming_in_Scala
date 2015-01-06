//part 1a

object part1a {

	def bob(x:Int):Int = { println("Bob"); x + 1}

	def joe(x:Int,y:Int):Int = {
		println("Joe");
		val a =x;
		val b = y;
		println(a+b);
		a+b
	}

	def ron(x:Int, y:Int,z:Int){
		println("Ron")
		println(x+y)
		println(x+y+z)
	}

	def buggy(x:Int):Int = {
		if(x>=0){
			println(x)
			buggy(x-1)
		}else 0
	}

	def foo(x:Int, y:Int):Int = {
		println(x)
		x + 2
	}

	def main(args: Array[String]){

		println("Part 1 (a):")
		println("------------------------------Program 1--------------------------------")
		println("The output of Program 1 is: ")
		ron(bob(joe(bob(1),2)),3,4)
		println()
		println("Explanation: ")
		println(
			"""
			Bob --> called by bob(1), now bob(1) = 2
			Joe --> called by joe(bob(1),2) => joe(2,2) => 4
			4   --> then we print 4 inside the joe function
			Bob --> called by bob(joe(bob(1),2)) => bob(4) => 5
			8   --> called by the outer function ron(5,3,4) => 5+3
			12  --> called by the outer function ron(5,3,4) => 5+3+4
			""")
		println("------------------------------Program 1--------------------------------")
		println()
		println("------------------------------Program 2--------------------------------")
		println("The output of Program 2 is: ")
		println(foo(1,buggy(10)))
		println("Explanation: ")
		println(
			"""
			10 --
			9   |
			8   |
			7   |
			6   |
			5   ==> called by the buggy(10) function, which keep printing
			4   |	and decrementing until x = 0
			3   |
			2   |
			1   |
			0  --
			1   ==> called by print(x) in foo() function since x = 1
			3   ==> called by x + 2 in foo() function
			""")
		println("------------------------------Program 2--------------------------------")
	}
}