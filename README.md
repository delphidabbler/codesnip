# CodeSnip Caboli

The `caboli` branch of the CodeSnip repo is where it is intended to:

1. convert CodeSnip to compile with Delphi 11 Alexandria
2. give the UI a major overhaul and polish.

> :information_source: "Caboli" is a Welsh language word that translates into English as to "polish" or to "buff".

## Rationale 

Their have been several attempts, spanning 9 years, to create a radically different CodeSnip version 5. These plans all required a significant rewriting of the code base. All the attempts have failed because they were too ambitious. (See the `pagoda`, `pavilion` & `belvedere` branches.)

The planned change of compiler & the UI overhaul were both to be included in the the v5 release. This left CodeSnip 4 looking more and more dated and being stuck with using Delphi XE as its compiler.

In the light of the failure of the ambitious attempts to rewrite the program, it seems prudent to take a more evolutionary approach to overhauling CodeSnip.

Looking at the [wishlist of changes](https://github.com/delphidabbler/codesnip/blob/belvedere/README.md#original-plans-for-codesnip-5-belvedere) that CodeSnip 5 was supposed to include, it seems that the one change that is required before anything else can be implemented is the switch to a modern version of Delphi.

For various historical reasons, CodeSnip 4 can only be compiled successfully with Delphi XE. With a few changes it _will_ compile with Delphi 11 Alexandria, but the UI is broken in the resulting executable.

Consequently the UI needs to be heavily revised so that it will work after compiling with Delphi 11. Therefore it makes sense to overhaul & modernise the UI at the same time as switching compilers.

## Development

All development will take place in the `caboli` branch.

`caboli` was branched from `master` as of release v4.20.2. As new releases are made to `master`, the changes will be merged into `caboli`.

At present, any contributions should be made to the main `develop` branch, not to `caboli`.

## Will this branch succeed?

After series of failures, its hard to say. However I think there's more chance of success this time, because this project, although requiring some significant changes, is nowhere near as ambitious as the CodeSnip 5 attempts.
