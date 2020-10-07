import System.IO.Unsafe

foo = unsafePerformIO $ putStrLn "hi"
bar x = unsafePerformIO $ putStrLn x

twiceM a = a >> a

twice a = (a, a)

main = print $ Just (bar "hi") >>= \x -> Just (bar "ho")
