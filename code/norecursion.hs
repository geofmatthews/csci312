f :: (int->int) -> int -> int
f g n = if (n < 1)
        then 1
	else g g (n - 1)

h :: int -> int
h n = f f n

main :: IO ()
main = do
  print (h 5)
  