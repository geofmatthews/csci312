#   Bintree from EOPL p. 46 in Python
#   Illustrating object oriented programming.
#   Geoffrey Matthews
#   Tested using python on linux
#   Thu Apr  4 12:16:36 PST 2002     


class BinaryTree:
  def leafSum():
    return 0
  def printTree(self):
    self.printTreeIndented(0)
  def printTreeIndented(self,depth):
    print("Nonexistent tree.")
    
class Leaf(BinaryTree):
  def __init__(self,n):
    self.datum = n
  def leafSum(self):
    return self.datum
  def printTreeIndented(self,depth):
    for i in range(depth):
      print(" ", end='')
    print(self.datum)
  
class InteriorNode(BinaryTree):
  def __init__(self,k,lf,rt):
    self.key = k;
    self.left = lf;
    self.right = rt;
  def leafSum(self):
    return self.left.leafSum() + self.right.leafSum()
  def printTreeIndented(self,depth):
    for i in range(depth):
      print(" ", end='')
    print(self.key)
    self.left.printTreeIndented(depth+1)
    self.right.printTreeIndented(depth+1)

x = InteriorNode("Fuzzy",
                 InteriorNode("Wuzzy",
                              Leaf(11),
                              InteriorNode("Was",
                                           Leaf(22),
                                           Leaf(33))),
                 InteriorNode("A",
                              InteriorNode("Bear",
                                           Leaf(44),
                                           Leaf(55)),
                              Leaf(66)))

x.printTree()
print("The sum is:", x.leafSum())



