# CodeSnip Caboli

> **Branch abandoned**
>
> After quite some effort, the attempt to rebuild the existing CodeSnip v4 code base in the `caboli` branch has proved too problematic. Therefore the branch has been abandoned as of 2023-04-09.
> 
> The branch will remain in the repository, for now, in case some of its code can be salvaged for later use.
>
> :no_entry: Pull requests for this branch are not being accepted.

## Rationale behind the `caboli` branch

There had been several attempts, spanning 9 years, to create a radically different CodeSnip version 5. These plans all required a significant (or complete) rewrite of the existing code. All the attempts seemed to have failed because they were too ambitious. (See the `pagoda`, `pavilion` & `belvedere` branches.)

In the light of this it was decided to take a less ambitious, more evolutionary, approach. The idea was to identify the most important changes proposed for CodeSnip 5 and to attempt to achieve them by starting from the existing CodeSnip 4 code base.

Looking at the [wishlist of changes](https://github.com/delphidabbler/codesnip/blob/belvedere/README.md#original-plans-for-codesnip-5-belvedere) that CodeSnip 5 was supposed to include, it seemed that the one change upon which everything else depended was to switch to a modern version of Delphi.

Unfortunately, it was already known that, although the CodeSnip 4 code base could be modified to compile with Delphi 11, the UI was broken in the resulting executable. Consequently the UI needed to be heavily revised so that it would work after compiling with Delphi 11. 

Therefore it made sense to overhaul & modernise the UI at the same time as switching compilers. If these two goals could be acheived then the resulting code base could be used as a springboard for further developments.

The `caboli` branch was created to attempt this development. It was branched from `master` as of release v4.20.2.

> :information_source: "Caboli" is a Welsh language word that translates into English as to "polish" or to "buff".

## Failure of `caboli`

Quite a lot of work was done in two main tranches between December 2022 and April 2023, with some initially encouraging results:

* The CodeSnip 4 code base _was_ made to compile with Delphi 11.
* The program's main form was rebuilt from scratch to avoid the problem of the broken UI.
* Dialogue boxes were also adjusted.

Unfortunately, as development proceeded more and more obscure bugs raised their heads, including:

* Memory access failures (albeit that some, but not all, of them related to conversion to 64 bit compilation).
* Various UI alignment and font sizing problems. 
* Font and form scaling problems on high DPI displays.

It gradually became apparent that numerous bugs were hiding in the old code base and there was no obvious method of predicting where the next one would arise. Often fixing one bug would resulting in another one being found.

The conclusion was that it could take nearly as long to track down and fix these problems as it would take to rewrite the program. And at the end of the process there would be a significant probability of ending up with a program with very flaky and buggy underpinnings.

Wanting to avoid the [sunk cost falacy](https://dictionary.cambridge.org/dictionary/english/sunk-cost-fallacy), the decision was taken to back out before the investment in time became too great.

## Where next?

The current position is that rewriting from scratch has failed and so has adapting the old code base.

For now, at least, it seems that the only realistic approach is to keep on incrementally updating the v4 code base and sticking with the venerable Delphi XE.
