-- Bintree from EOPL p. 46 in Ada
-- Illustrating Variant records
-- Geoffrey Matthews
-- Thu Mar 10 15:43:57 PST 2005
-- Tested with gnat on linux

with Ada.Text_IO, Ada.Strings.Unbounded.Text_IO;
use Ada.Text_IO, Ada.Strings.Unbounded, Ada.Strings.Unbounded.Text_IO;

procedure BinTree is

   BadAccess : exception;

   subtype NameType is Unbounded_String;
   type BinTreeTag is (Leaf, Interior_Node);
   type BinTreeRecord;
   type BinTree is access BinTreeRecord;
   type BinTreeRecord(Isa : BinTreeTag) is record
      case Isa is
         when Leaf =>
            Datum : Integer;
         when Interior_Node =>
            Key : NameType;
            Left : BinTree;
            Right : BinTree;
      end case;
   end record;

   function MakeLeaf(Num : Integer) Return BinTree is
      Item : BinTree := new BinTreeRecord(Leaf);
   begin
      Item.all.Datum := Num;
      return Item;
   end MakeLeaf;

   function GetDatum(Tree : BinTree) return Integer is
   begin
      case Tree.Isa is
         when Leaf =>
            return Tree.all.Datum;
         when Interior_Node =>
            raise BadAccess;
      end case;
   end GetDatum;

   function MakeInteriorNode(K : String; Lf : BinTree; Rt : BinTree)
                            return BinTree is
      Item : BinTree := new BinTreeRecord(Interior_Node);
   begin
      Item.all.Key := To_Unbounded_String(K);
      Item.all.Left := Lf;
      Item.all.Right := Rt;
      return Item;
   end;

   function GetKey(Tree : BinTree) return NameType is
   begin
      case Tree.Isa is
         when Leaf =>
            raise BadAccess;
         when Interior_Node =>
            return Tree.all.Key;
      end case;
   end GetKey;

   function GetLeft(Tree : BinTree) return BinTree is
   begin
      case Tree.Isa is
         when Leaf =>
            raise BadAccess;
         when Interior_Node =>
            return Tree.all.Left;
      end case;
   end GetLeft;

   function GetRight(Tree : BinTree) return BinTree is
   begin
      case Tree.Isa is
         when Leaf =>
            raise BadAccess;
         when Interior_Node =>
            return Tree.all.Right;
      end case;
   end GetRight;

   procedure PrintBinTreeIndented(Tree : BinTree; Depth : Integer) is
   begin
      case Tree.Isa is
         when Leaf =>
            for I in 1..Depth loop
               Put("  ");
            end loop;
            Put_Line(Integer'Image(GetDatum(Tree)));
         when Interior_Node =>
            for I in 1..Depth loop
               Put("  ");
            end loop;
            Put_Line(GetKey(Tree));
            PrintBinTreeIndented(GetLeft(Tree), Depth+1);
            PrintBinTreeIndented(GetRight(Tree), Depth+1);
      end case;
   end PrintBinTreeIndented;

   procedure PrintBinTree(Tree : BinTree) is
   begin
      PrintBinTreeIndented(Tree, 0);
   end;

   function LeafSum(Tree : BinTree) return Integer is
   begin
      case Tree.Isa is
         when Leaf =>
            return GetDatum(Tree);
         when Interior_Node =>
            return LeafSum(GetLeft(Tree)) + LeafSum(GetRight(Tree));
      end case;
   end LeafSum;

   X : BinTree;

begin
   X := MakeInteriorNode(
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
   PrintBinTree(X);
   Put("The sum is: ");
   Put_Line(Integer'Image(LeafSum(X)));

end BinTree;




