main = do
    print( head ( tail ['a', 'b', 'c'] ) )
    print( tail ( head [['a', 'b'], ['c','d']] ) )
    print( tail ( head ( tail [['a', 'c', 'd'], ['a','b']] ) ) )
    print( head ( head ( tail [['a','d'], ['b', 'c']] ) ) )