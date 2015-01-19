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

After you cloning the repository, go the repository folder and do

```bash
cabal build
```

Now you compiled the program. There is an example Prolog file in the "usage" folder. You can run it like this:

```bash
./dist/build/Norvigs-Spelling-Corrector/Norvigs-Spelling-Corrector
```

Then you can enter terms to the program to get the corrected version of them.

## License

The MIT License (MIT)

