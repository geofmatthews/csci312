/* Bintree from EOPL p. 46 in Java
   Illustrating object oriented programming.
   Geoffrey Matthews
   Tested using javac on linux
   Thu Apr  4 10:54:55 PST 2002  
*/

class BinaryTree {
    public int leafSum() {
	return 0;
    }
    public void printTree() {
	printTreeIndented(0);
    }
    public void printTreeIndented(int depth) {
	System.out.println("Nonexistent tree.\n");
    }
}

class Leaf extends BinaryTree {
    public Leaf(int n) {
	datum = n;
    }
    public void printTreeIndented(int depth) {
	for (int i = 0; i < depth; i++)
	    System.out.print("  ");
	System.out.println(datum);
    }
    public int leafSum () {
	return datum;
    }
    private int datum;
}

class InteriorNode extends BinaryTree {
    public InteriorNode(String k, BinaryTree lf, BinaryTree rt) {
	key = k;
	left = lf;
	right = rt;
    }
    public void printTreeIndented(int depth) {
	for (int i = 0; i < depth; i++)
	    System.out.print("  ");
	System.out.println(key);
	left.printTreeIndented(depth+1);
	right.printTreeIndented(depth+1);
    }	
    public int leafSum () {
	return left.leafSum() + right.leafSum();
    }
    private String key;
    private BinaryTree left;
    private BinaryTree right;
}
	
class bintree {
    public static void main(String[] args) {
	BinaryTree x =
	    new InteriorNode(
		"Fuzzy",
		new InteriorNode(
		    "Wuzzy",
		    new Leaf(11),
		    new InteriorNode(
			"Was",
			new Leaf(22),
			new Leaf(33))),
		new InteriorNode(
		    "A",
		    new InteriorNode(
			"Bear",
			new Leaf(44),
			new Leaf(55)),
		    new Leaf(66)));
	x.printTree();
	System.out.println("The sum is: " + x.leafSum());
    }
}
	
