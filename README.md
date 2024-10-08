# CodeSnip Cupola

## Contents

1. [Overview](#overview)
2. [Aims of `cupola`](#aims-of-cupola)
3. [Source Code](#source-code)
4. [Compiling](#compiling)
5. [Contributing](#contributing)
6. [Analysis of past failures](#analysis-of-past-failures)
7. [Road map](#road-map)

## Overview 

This is an experimental branch of CodeSnip whose purpose is to develop a "lite" version of the program with a modern UI.

The decision to create a lite version was arrived at after [analysing the reasons](#analysis-of-past-failures) for failure of all previous attempts to create a fully fledged v5 successor to CodeSnip 4. I'm not putting a timescale on this, and neither am I optimistic that `cupola` will succeed where others have failed, but this seems to be the most realistic plan so far.

## Aims of `cupola`

The following are the aims of this project:

* To be a "lite" version of CodeSnip, providing only a subset of the features in CodeSnip 4.
* Use all new code wherever possible.
* Use a modern Delphi compiler instead of Delphi XE.
* Implement high DPI support and light/dark themes.
* Target Windows 64 as a priority, possibly with an option for a 32 bit build if demand is there.
* Create only a single edition instead of separate standard and portable editions.
* Only implement as much cross-compatibility with CodeSnip 4 as necessary to enable existing snippets to be imported.
* No longer treat snippets from the DelphiDabbler Code Snippets database as read-only and distinct from the user's own snippets.
* Permit more than one snippets database to be maintained.
* Do not delay release until the program is feature complete: release early alpha and beta versions that incremenatally add back features.

## Source code

The `cupola` branch was branched from `master` as at [`version-4.21.1`](https://github.com/delphidabbler/codesnip/tree/version-4.21.1).

Subsequently the changes made in master at [`version-4.21.2`](https://github.com/delphidabbler/codesnip/tree/version-4.21.2), [`version-4.22.0`](https://github.com/delphidabbler/codesnip/tree/version-4.22.0) and [`version-4.23.0`](https://github.com/delphidabbler/codesnip/tree/version-4.23.0) have been merged into `cupola`.

Because `cupola` was branched from `master`, all the existing code base is available to it. To make it easy to distinguish the `cupola` code from the existing code, all development will take place in a `cupola` sub-directory off the repo root.

Even though `cupola` is aiming to use all new code where possible, it is unrealistic to believe that none of the existing code will be re-used.  However any code being considered for re-use should be carefully reviewed. If accepted, the code must be renamed into a suitable unit scope and moved into the `cupola` sub-directory. In particular, for code to be re-used it:

* must not dig down into the Windows API.
* not be closely related to the GUI. 
* be 64 bit compatible.

## Compiling

The `cupola` source code is targetted at Delphi 11.3 Alexandria. Other Delphi compilers may be suitable, providing they have support for inline variable declarations.

All debug code will be compilable directly from the Delphi IDE. Unlike the original code base, it will not be necessary to perform full builds from the command line using a `Makefile`.

However, final releases will be created from a script run from the command line.

### Dependencies

The build process requires that [DelphiDabbler Version Information Editor](https://delphidabbler.com/software/vied) is installed and that its installation directory is stored in the `VIEdRoot` environment variable.

The release script additionally requires that [InfoZIP `zip.exe`](https://delphidabbler.com/extras/info-zip) is installed and that its installation directory is stored in the `ZipRoot` environment variable.

## Contributing

⛔ Sorry, contributions are not being accepted to the `cupola` branch at the moment.

> Contributions to the main CodeSnip 4 code base in the [`develop`](https://github.com/delphidabbler/codesnip/tree/develop) branch are more than welcome. See [`CONTRIBUTING.md`](https://github.com/delphidabbler/codesnip/blob/develop/CONTRIBUTING.md) for details of how to go about this.

## Analysis of past failures

There have been several attempts at creating a new major version of CodeSnip. All have failed. Each failed attempt has a code name, and a branch with the same name in CodeSnip's GitHub repository. In chronological order, they are:

* [`pagoda`](https://github.com/delphidabbler/codesnip/tree/pagoda)
* [`pavilion`](https://github.com/delphidabbler/codesnip/tree/pavilion) 
* [`belvedere`](https://github.com/delphidabbler/codesnip/tree/belvedere)
* [`caboli`](https://github.com/delphidabbler/codesnip/tree/caboli)

### `pagoda`

The plan was to: 

1. Generalise CodeSnip to be a code bank for several different languages instead of just Pascal, while still providing some additional support for test-compiling Pascal code.
2. Increase the focus on the user's own code while downplaying the importance of code downloaded from the DelphiDabbler Code Snippets database.

The branch was based on the then-current CodeSnip code base. A _lot_ of work was done. This may have succeeded had my interest in programming not waned. The last commits were in 2014 and there things languished until 2022, when `pavilion` came along.

### `pavilion`

In January 2020 it was looking likely that the web services that CodeSnip relied on would be closing, as they eventually did. This meant that all the features that relied on the web services needed to be ripped out of CodeSnip. Two approaches were considered:

1. To revise the existing code base
2. To revisit `pagoda` and to strip the web services out of that. `pavilion` was simply a branch of `pagoda` where the work on ripping out the web services took place.

It became apparent that `pavilion` couldn't be finished in time for the web service closure deadline, so the 1st option was persued and `pavilion` became moribund and later abandoned.

### `belvedere`

1st January 2022 and optimism ran high. It was time for another attempt at CodeSnip 5.

The objectives were similar to `pagoda`, but even more ambitious. To and already long list of objectives were added the requirement to modernise the UI, to convert to 64 bit and switch to a more modern Delphi compiler. 

It was thought that the previous attempts failed because they were built on the legacy code base. So the decision was taken to build CodeSnip 5 from the ground up, using all (or mostly) new code.

This proved to be much too ambitious and the project foundered.

### `caboli`

By the end of 2022 it was becoming apparent that `belvedere` was going nowhere. `caboli` was proposed as a much less ambitious update: the compiler would be changed to Delphi 11 and the UI would be modernised. This would all be built on the exisiting code base, except that some of the UI code would be built from scratch (because compilation with Delphi 11 broke the UI).

At first results were promising. But font scaling problems persisted and strange memory access issues started appearing.

It was concluded that the old code base, going back in parts to 2005, made too many assumptions about pointer and integer sizes and was potentially hiding numerous obscure bugs.

`caboli` was abandoned after 5 months because of lack of confidence in the stability of the underlying code.

### Learning points

There are three main learning points to be taken from the above review:

1. The original code base is not reliable and doesn't lend itself to modern UI features. This indicates that the code needs to be rewritten.
2. Rewriting the existing code base is possibly unrealistic. Evidence from other real world examples supports this conclusion.
3. Attempts to radically change the program all at once are over-ambitious.

Points 1 and 2 are conflicting, which means that there is no simple solution.

`caboli` addressed points 2 and 3 by having limited goals and by re-using the old code base, but fell at point 1. `belvedere` addressed point 1 by attempting ground up rewrite but fell at point 3: over-ambition. `pagoda` and `pavillion` failed on ponts 1 and 3.

It seems there is only one path that has not been taken. That is to do a ground up rewrite that avoids over-ambition.

## Road map

Considering the analysis above, a two stage strategy has been adopted:

1. Create a feature-limited, "lite", version of CodeSnip. This should be as close as practical to being a ground up rewrite. This is `cupola`.

2. If, and only if, `cupola` succeeds, create a branch off it where original CodeSnip features will be added back. This project would have code name `rotunda`.

Should all go well, development of `cupola` and `rotunda` would continue in parallel and CodeSnip 4 would be retired.
