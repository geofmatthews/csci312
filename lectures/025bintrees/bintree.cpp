/* Bintree from EOPL p. 46 in C++
   Illustrating object oriented programming.
   Geoffrey Matthews
   Wed Apr  3 15:27:34 PST 2002
   Tested with g++ on linux
*/

#include <string>
#include <iostream>

using namespace std;

// Superclass:
class BinTree;
// Subclasses:
class Leaf;
class InteriorNode;

class BinTree {
public:
    BinTree() {};
    // This will make << a "friend" to all subclasses:
    friend ostream& operator<<(ostream& os, BinTree *tree) {
	tree->printIndented(os, 0);
	return os;
    };
    virtual void printIndented(ostream& os, int depth) {};
    virtual int leafSum() = 0;
};

class Leaf : public BinTree {
public:
    Leaf(int num) {
	datum = num;
    };
    virtual void printIndented(ostream& os, int depth) {
	for (int i = 0; i < depth; i++) cout << "  ";
	cout << datum << endl;
    };
    virtual int leafSum() {
	return datum;
    }
private:
    int datum;
};

class InteriorNode : public BinTree {
public:
    InteriorNode(string k, BinTree *lf, BinTree *rt) {
	key = k;
	left = lf;
	right = rt;
    };
    virtual void printIndented(ostream& os, int depth) {
	for (int i = 0; i < depth; i++) cout << "  ";
	cout << key << endl;
	left->printIndented(os, depth+1);
	right->printIndented(os, depth+1);
    };
    virtual int leafSum() {
	return left->leafSum() + right->leafSum();
    }
private:
    string key;
    BinTree *left, *right;
};


int main (int argc, char *argv[]) {
    BinTree *x = 
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
    
    cout << x;
    cout << "The sum is: " << x->leafSum() << endl;
    return 0;
}

    


