## debug-show

### Summary

This library provides a function that prints a representation for any `Generic` data type. This is especially useful for data 
types that cannot derive a `Show` instance.

```haskell
-- Simplified type signature
debugPrint :: Generic a => a -> IO ()

-- Actual type signature
debugPrint :: (Generic a, GDebugShow (Rep a), MonadIO m) => a -> m ()
```

### Dragons

This library defines the following highly invasive orphan instances, and should thus _never_ be imported permanently in library
or application code. It's only for debugging.

```haskell
instance Show a
instance Generic Double
instance Generic Int
instance Generic Char
instance Generic Float
instance Generic Word
instance Generic (a -> b)
instance Generic (IO a)
```

### Examples

TODO: Show something massive like `DynFlags`
