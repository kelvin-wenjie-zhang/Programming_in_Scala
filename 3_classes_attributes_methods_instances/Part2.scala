/** Abstract base class for Nodes and Leafs */
abstract class Tree[T] { 
 
    val content : T

    /** A higher-order generalization for tree operations.
      *                   
      * This method provides depth first tree traversal as an 
      * abstraction over different tree operations.
      * Tree operations can be implemented by creating first-class functions 
      * proc_node and proc_leaf and passing them to traverse.
      * The abstract method is implemented in Node and Leaf.                    
      */                     
    def traverse[A](proc_node: (A,A,T) => A, proc_leaf: T=>A) : A 

    override def toString = {

      // the type of tree operation is String, thus we use toString method
      val proc_node: (String,String,T) => String = (x:String, y:String, z:T) => "(" + (z.toString) + " " + x + " " + y + ")"
      val proc_leaf: T=>String = (a:T) => a.toString
      
      // traverse the tree by printing all the content
      traverse[String](proc_node, proc_leaf)

    }
   
    def leafs : List[T] =  {

      // the result list that we will return
      var result : List[T] = Nil
      
      // type of tree operation is boolean.
      // proc_node should be indicated as false since nodes are NOT leafs.
      val proc_node: (Boolean,Boolean,T) => Boolean = (x:Boolean, y:Boolean, z:T) => false
      // proc_leaf should be indicated as true so that we could append it to the result list
      val proc_leaf: T=>Boolean = (a:T) => {result = result:::List(a);true}

      // traverse the tree
      traverse[Boolean](proc_node, proc_leaf)

      // return the result list
      result
    }

    def evaluate : Int = {

      // the final result
      var result : Int = 0

      // proc_node would apply the operation "z" to x and y
      // then it return 0 to indicate the successful application
      val proc_node: (Int,Int,T) => Int = (x:Int, y:Int, z:T) => {
        //multiplication
        if(z.toString == "*") {result = result + x*y;0}
        // summation
        else if(z.toString == "+") {result = result + x + y;0}
        // subtraction
        else if(z.toString == "-") {result = result + (x - y);0}
        // the operation "z" is not applicable
        else 0
      }

      val proc_leaf: T => Int = (a:T) => a.toString.toInt

      // traverse the tree
      traverse[Int](proc_node, proc_leaf)

      result
    }
}

/** A Tree with exactly two subtrees. */
class Node[T](val content: T, val left: Tree[T], val right: Tree[T]) extends Tree[T]{

    /** The traverse implementation for Node calls proc_node on the 
      * results returned by calling traverse recusively on each 
      * subtree and the content of this node. 
      */
    def traverse[A](proc_node : (A,A,T) =>A, proc_leaf: T=>A) = 
            proc_node(left.traverse(proc_node, proc_leaf), 
                     right.traverse(proc_node, proc_leaf),
                     content)
}
/** Companion object for the Node -- only used to define an apply method */
object Node {
    def apply[T](content: T, left : Tree[T], right: Tree[T]) = 
        new Node(content, left, right)
}

/** A Tree that does not have any further subtrees
  * (i.e. a single leaf node itself).
  */
class Leaf[T](val content: T) extends Tree[T] {

    /** The traverse implementation for Leaf calls proc_laf on the content of
      * the node. proc_leaf usually just converts the content into the correct
      * result type.
      */
    def traverse[A](proc_node : (A,A,T)=>A, proc_leaf: T=>A) =
        proc_leaf(content) 
}
/**  Companion object for the Leaf, only used to define an apply method */
object Leaf {
    def apply[T](content: T) = new Leaf(content) 
}    


/**
 * Main object to test tree operations. 
 */
object Part2{
  
    def main(args : Array[String]) {
        val tree : Tree[String] = 
            Node("-",
                Node("+",
                    Leaf("3"),
                    Node("*",
                        Leaf("5"),
                        Leaf("6"))),
                Leaf("7"))

        println(tree)
        // Should print (- (+ 3 (* 5 6)) 7)
        
        println(tree.leafs)  
        // Should print List(3, 5, 6, 7)

        println(tree.evaluate)
        // Should print 26       
    }
}
