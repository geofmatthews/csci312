(*   Bintree from EOPL p. 46 in ML
     Illustrating datatypes
     Geoffrey Matthews
     Thu Apr  4 10:25:26 PST 2002    
     Tested in SML/NJ
     *)

datatype bintree =
  Leaf of int |
  InteriorNode of string * bintree * bintree;

fun spaces n =
  if n = 0 then "" else "  " ^ spaces (n-1)

fun stringTree (Leaf(datum)) depth =
      (spaces depth) ^ Int.toString(datum) ^ "\n"
  | stringTree (InteriorNode(key,left,right)) depth =
     (spaces depth) ^ key ^ "\n" ^
     (stringTree left (depth + 1)) ^ (stringTree right (depth + 1));
  
fun printTree (Leaf(datum)) = print(Int.toString(datum))
  | printTree (InteriorNode(key,left,right)) =
      print(key ^ "\n"  ^ (stringTree left 1) ^ (stringTree right 1));

fun leafSum (Leaf(datum))  = datum
  | leafSum (InteriorNode(key,left,right)) =
    (leafSum left) + (leafSum right);

val x = InteriorNode("Fuzzy",
                     InteriorNode("Wuzzy",
                                  Leaf(11),
				  InteriorNode("Was",
				               Leaf(22),
					       Leaf(33))),
                     InteriorNode("A",
		                  InteriorNode("Bear",
				               Leaf(44),
					       Leaf(55)),
	                          Leaf(66)));

printTree x;
print("The sum is: " ^ Int.toString(leafSum x) ^ "\n");




