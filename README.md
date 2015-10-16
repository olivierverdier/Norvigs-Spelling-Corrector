Norvig's Spelling Corrector
===== 

[![Build Status](https://travis-ci.org/olivierverdier/Norvigs-Spelling-Corrector.svg?branch=master)](https://travis-ci.org/olivierverdier/Norvigs-Spelling-Corrector)

A Haskell implementation of Peter Norvig's spelling corrector, based on that of [Marco Sero](http://marcosero.com/blog/norvig-haskell-spelling-corrector/).

The code demonstrates

* Using the List monad
* Creating a typeclass and defining instances
* Programming using polymorphic types

## Usage

In order to use or compile the program you need to have [Haskell](http://www.haskell.org/) installed.

After you cloning the repository, go the repository folder.

If you do not want to clutter the global Haskell installation (recommended), run:

```bash
cabal sandbox init
```

Install the dependencies, if any are required:
```bash
cabal install --only-dependencies
```

Now you can build the executable:
```bash
cabal configure && cabal build
```

You can now run the executable by typing:

```bash
cabal run
```

You are now in the interactive spell checker. Try a few words and see the proposed corrected spelling. You can quit with `ctrl-D`.

## License

The MIT License (MIT)

