# scip-ghc-plugin

A ghc plugin that modify the Ext.Ast module to generate SCIP file.

## usage

Usage is simple, just add the following to your cabal file:

```cabal
build-depends:  scip-ghc-plugin
ghc-options:    -fplugin=scip-ghc-plugin
```

## Limitations

It only supports 9.8.2.
