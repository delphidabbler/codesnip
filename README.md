# CodeSnip v5 `pavilion`

> **Experimental, development branch**

Following a false start back in 2013/4 in the `parsnip` branch, development of CodeSnip 5 restarted in the `pagoda` branch, which picked up where `parsnip` left off. `parsnip` became moribund in 2016 and was officially abandoned in Jan 2020.

At the same time the impending closure of the delphidabbler.com web services that CodeSnip 4 depends on meant that urgent work started on try to:

1. remove CodeSnip's dependency on the web services
2. unifying the online Code Snippets database (no longer updated) with the user's own databases in a single database.

One strategy is to develop from the CodeSnip 4 code base in the `development` branch. After some work on that it was decided to explore taking the `pagoda` development in a slightly different direction. That's what the `pavilion` branch is all about.

`pavilion` is a branch off `pagoda` as it was on 13 July 2016. `pagoda` has been abandoned now.

Only time will tell if this works out. I may bring it to completion if I can do it in a timely manner, but `pavilion` may also find itself abandoned if time runs out.

The `pagoda` branch has been left in place in case anyone else wants to continue development along different lines. All my own development effort will continue in `pavilion`.

> Anyone who decides to continue with `pagoda` development please fork the repo. Do not submit changes back here - no pull requests will be accepted. However, it will be interesting to know what you plan. **Do not use any code that accesses the delphidabbler.com web services** - they **really** are going away on 15 June 2020 Thanks.

The original idea behind v5 was to:

1. Generalise CodeSnip to be a code bank for several different languages instead of just Pascal, while still providing some additional support for test-compiling Pascal code.
2. Increase the focus on the user's own code while downplaying the importance of code downloaded from the DelphiDabbler [Code Snippets database](http://snippets.delphidabbler.com/), like [SWAG](http://swag.delphidabbler.com/) is at present.<sup> †</sup>
3. Remove the program's dependency on web services provided by [DelphiDabbler.com](http://delphidabbler.com) given the current uncertainty over that websites future.<sup> ‡</sup>

<sup>†</sup> It is hoped to move the Code Snippets database to GitHub before DelphiDabbler closes down on 15 June 2020. A provisional version already in a private GitHub repo.

<sup>‡</sup> No longer uncertain - the web services go away on 15 June 2020.

It remains to be seen how much of the original plans will actually make it into the v5 release.
