/* Bintree from EOPL p. 46 in C
   Illustrating discriminated unions.
   Geoffrey Matthews
   Wed Apr  3 15:27:34 PST 2002
   Tested with gcc on linux
*/


#include <stdio.h>

typedef enum {
    LEAF,
    INTERIOR_NODE
} binTreeTag;

typedef struct binTreeUnionType *binTree;

typedef struct {
    int datum;
} leafType;

typedef struct {
    char *key;
    binTree left;
    binTree right;
} interiorNodeType;

struct binTreeUnionType {
    binTreeTag isa;
    union {
	leafType leaf;
	interiorNodeType node;
    } u;
} ;

binTree makeLeaf(int num) {
    binTree item;
    item = (binTree)malloc(sizeof(struct binTreeUnionType));
    item->isa = LEAF;
    item->u.leaf.datum = num;
    return item;
}

int getDatum(binTree tree) {
    return tree->u.leaf.datum;
}

binTree makeInteriorNode(char *k, binTree lf, binTree rt) {
    binTree item;
    item = (binTree)malloc(sizeof(struct binTreeUnionType));
    item->isa = INTERIOR_NODE;
    item->u.node.key = k;
    item->u.node.left = lf;
    item->u.node.right = rt;
    return item;
}

char *getKey(binTree tree) {
    return tree->u.node.key;
}
binTree getLeft(binTree tree) {
    return tree->u.node.left;
}
binTree getRight(binTree tree) {
    return tree->u.node.right;
}

void printBinTreeIndented(binTree tree, int depth) {
    int i;
    switch (tree->isa) {
	case LEAF:
	    for (i = 0; i < depth; i++) printf("  ");
	    printf("%d\n", getDatum(tree));
	    break;
	case INTERIOR_NODE:
	    for (i = 0; i < depth; i++) printf("  ");
	    printf("%s\n", getKey(tree));
	    printBinTreeIndented(getLeft(tree), depth+1);
	    printBinTreeIndented(getRight(tree), depth+1);
	    break;
    }
}

void printBinTree(binTree tree) {
    printBinTreeIndented(tree, 0);
}

int leafSum(binTree tree) {
    switch(tree->isa) {
	case LEAF:
	    return getDatum(tree);
	case INTERIOR_NODE:
	    return leafSum(getLeft(tree)) + leafSum(getRight(tree));
    }
}

int main (int argc, char *argv[]) {
    binTree x =
	makeInteriorNode(
	    "Fuzzy",
	    makeInteriorNode(
		"Wuzzy",
		makeLeaf(11),
		makeInteriorNode(
		    "Was",
		    makeLeaf(22),
		    makeLeaf(33))),
	    makeInteriorNode(
		"A",
		makeInteriorNode(
		    "Bear",
		    makeLeaf(44),
		    makeLeaf(55)),
		makeLeaf(66)));
    
    printBinTree(x);
    printf("The sum is: %d\n", leafSum(x));
    return 0;
}

    


