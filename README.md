# CodeSnip Belvedere

## What is the `belvedere` branch for?

This is yet another attempt to create CodeSnip v5.

OK, the previous 2 (or 3?) attempts (`parsnip`/`pagoda` and `pavilion`) failed, but they were building on the existing CodeSnip 4 code base and that proved too difficult. There's a lot of _old_ code in there and it's been getting messy for years.

What is more the CodeSnip 4 code base is stuck with using Delphi XE to compile it. Attempts to get it to compile with Delphi 11 finally succeeded, but introduced so many bugs it didn't seem worth the trouble.

So, `belvedere` is planned to be a ground-up rebuild - a fresh start.

Now, the `belvedere` branch contains all the old code - it was branched off CodeSnip 4.19.0's code base - **but** new development will take place in a new folder. Some of the old code may be used where it's suitable: it will be copied into the the new folder structure as required.

And will this attempt succeed where the others have failed? Who knows - I just think that it's got more of a chance than the other attempts! Regardless this is a long term project and CodeSnip 4 will continue to be developed.

## Plans for CodeSnip 5 Belvedere

Initial thoughts are that CodeSnip 5 will:

* Be designed to be 64 bit from the start.
* The plan to use FMX didn't survive contact with reality for long. I want to use SynEdit, and I've just discovered that's a Windows & VCL only component, so that decides it: I'm sticking with the VCL and not looking to support platforms other than Windows in this version. ~~Use the FireMonkey framework to maximise the chance that the final program will be cross-platform - possibly Windows and Linux. **But** it will be built as Windows first. It's not planned to port it to mobile, and I don't do Mac, so that would be for someone else.~~
* Have high DPI support built in from the ground up. Not having that support in CodeSnip 4 is getting to be a big problem.
* Support storing snippets in other languages while still providing extra facilities for Pascal code, like test compilation.
* Switch to using SynEdit as the code editor to gain the benefit of syntax highlighted code in various languages.
* No longer render the main display as HTML (if at all possible).
* Finally get rid of the distinction between user-defined snippets and those downloaded from the DelphiDabbler Code Snippets Database - there will only be one database and all snippets will be editable. I'll try to retain a link back to the source of each snippet so those from the DelphiDabbler database will be remain updatable.
* Have a completely new and extensible database format. Could be custom binary, could be SQLite, could be file based like now. But I want to get away from INI and XML files - too cumbersome and too slow.
* Use a new JSON based config file format?
* Drop support for older OSs - may be Win10/Win11 only?
* Drop support for legacy CodeSnip file formats from before CodeSnip 4.
* Bring together the standard and portable versions: no more separately compiler versions. CodeSnip will probably run as a standard Windows application if installed in `%ProgramFiles%` and act as a portable application is its installed anywhere else.
* Support different themes: e.g. light and dark.
* Be more _testable_.

There may be some code in the `pavilion` or `pagoda` branches that I can use, because they were also going in this direction.

Seems a bit ambitious? Well we'll see how it goes.

## Timetable

Oh no, not getting into promising anything! It all depends on how much time and enthusiasm I have. This is my hobby after all.

## Comments & Suggestions Welcome

I'm more than happy to receive suggestions, comments, ideas etc. and you are welcome to use the [CodeSnip 5 Belvedere](https://github.com/delphidabbler/codesnip/discussions/42) discussion thread to share your thoughts. Please use that thread in preference to the Issues page for anything to do with `belvedere`.

## Blog

I'm planning to do an occasional blog post or two if and when I make progress. Those posts will appear on the existing [CodeSnip Blog](https://codesnip-app.blogspot.com/).
