import unittest
import sys
sys.path.append('../../../../csci312/homework/hw01/')
from ast_homework import *

class ast_Test(unittest.TestCase):
    def test_ast_calc(self):
        self.assertAlmostEqual(calc('9 + 3 * 4 / 2'),
                                    9 + 3 * 4 / 2)
        self.assertAlmostEqual(calc('13+4**(5+6)/7'),
                                    13+4**(5+6)/7)

    def test_ast_postfix(self):
        self.assertEqual(postfix('2+3+4'),
                         '2 3 + 4 +')


if __name__ == '__main__':
    unittest.main()
