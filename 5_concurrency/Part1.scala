import scala.collection.mutable.Map

//Part 1 - Maps (8pts)

object Part1 {

	def reverse[A,B](map: Map[A,B]): Map[B, List[A]] = {

		// get the first element of the map
		val (a,b) = map.head
		// create the empty map of the same type as the return type
		var result : Map[B, List[A]] = Map(b->(a::Nil)).empty

		// then iterate the map
		map.foreach{

			// for each key value pair
			keyVal => {
				if(result.get(keyVal._2) == None){
					// if the result map does not have the key,
					// we add it to the map
					result.put(keyVal._2, keyVal._1::Nil)

				} else {
					// if the result map has the key,
					// we append the value to the value list
					result(keyVal._2) = result(keyVal._2):::(keyVal._1::Nil)
				}
			}
		}

		// return the result map
		result
	}

	def main(args: Array[String]) {

		val fruit_to_color: Map[String,String] = Map("banana"->"yellow",
													"blueberry"->"blue",
													"cherry"->"red",
													"lemon"->"yellow",
													"kiwi"->"green")

		println(reverse(fruit_to_color))
	}
}