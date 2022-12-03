# CodeSnip Belvedere

> **Branch abandoned**
>
> The `belvedere` branch, which never really got off the ground, has been moribund since May 2022 and was officially abandoned on 2022-12-03.
>
> The branch will remain in the repository in case some of its code can be salvaged for later use.
>
> :no_entry: Pull requests for this branch are not being accepted.

## Rationale behind the `belvedere` branch

This was yet another attempt to create CodeSnip v5, rashly announced on new year's day 2022 as a new year's resolution. I really am old enough to know better!

The previous 2 (or 3?) attempts (`parsnip`/`pagoda` and `pavilion`) had failed. The thought was that they failed because they were building on the existing CodeSnip 4 code base and that had proved too difficult. There's a lot of very old code in CodeSnip 4 and it's been getting messy for years.

What is more, the CodeSnip 4 code base was stuck with using Delphi XE to compile it. Attempts to get it to compile with Delphi 11 finally succeeded, but introduced so many bugs it didn't seem worth the trouble.

So, `belvedere` was planned to be a ground-up rebuild - a fresh start.

While the `belvedere` branch did contain all the old code (it was originally branched off CodeSnip 4.19.0's code base), new development was to take place in a new folder. Some of the old code may have been used where it was suitable, but in principle most of it would be rewritten.

It was thought that there may be some code from the earlier `pavilion` or `pagoda` branches that may have been some use, because those branches were heading in the same direction.

### Original plans for CodeSnip 5 Belvedere

Initial thoughts were that CodeSnip 5 would:

* Be designed to be 64 bit from the start.
* Use the FireMonkey framework (though this plan soon changed to retaining the VCL).
* Have high DPI support built in from the ground up.
* Support storing snippets in languages other than Pascal.
* Switch to using SynEdit as the code editor (hence the need to keep the VCL).
* No longer render the main display as HTML (if at all possible).
* Get rid of the distinction between user-defined snippets and those downloaded from the DelphiDabbler Code Snippets Database.
* Have a completely new and extensible database format.
* Have greatly improved import / export capability.
* Switch to using JSON for config files.
* Drop support for older OSs - may be Win10/Win11 only.
* Drop support for legacy CodeSnip file formats from before CodeSnip 4.
* Bring together the standard and portable versions in one program.
* Support different themes: e.g. light and dark.
* Be more testable.
* Be compiled with Delphi 11.

## Failure of `belvedere`

It soon became obvious that this plan was far too ambitious and development quickly faltered.

Added to that, CodeSnip 4 was still receiving updates and many other projects needed attention, limiting the time available.

So, reluctantly, after 3 or 4 attempts over _nine years_, the plans for a radically different CodeSnip 5 were finally abandoned.
