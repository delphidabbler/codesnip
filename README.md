# CodeSnip Pagoda

> **Branch abandoned?? - not quite - change of heart **

Following a false start back in 2013/4 in the `parsnip` branch, development of CodeSnip 5 restarted in this `pagoda` branch, which picked up where `parsnip` left off. (Don't ask where those names came from!)

While `pagoda` has also been abandoned now, a recent branch named `pavilion` has been started to explore whether this code is the quickest route to getting a new version of CodeSnip working that both removes dependency on the soon-to-close DelphiDabbler.com webserver *and* unifies the user and "main" databases.

Only time will tell if this works out. I may bring it to completion if I can do it in a timely manner, but `pavilion` may also find itself abandoned if time runs out.

The `pagoda` branch has been left in place in case anyone else wants to continue development along different lines. All my development effort will continue in `pavilion`.

The original idea behind v5 was to:

1. Generalise CodeSnip to be a code bank for several different languages instead of just Pascal, while still providing some additional support for test-compiling Pascal code.
2. Increase the focus on the user's own code while downplaying the importance of code downloaded from the DelphiDabbler [Code Snippets database](http://snippets.delphidabbler.com/), like [SWAG](http://swag.delphidabbler.com/) is at present.<sup> †</sup>
3. Remove the program's dependency on web services provided by [DelphiDabbler.com](http://delphidabbler.com) given the current uncertainty over that websites future.

<sup>†</sup> It is hoped to move the Code Snippets database to GitHub before DelphiDabbler closes down on 15 June 2020.

> If you decide to continue with this development please fork the repo. Do not submit changes back here - no pull requests will be accepted. However, it will be interesting to know what you plan. **Do not use any code that accesses the delphidabbler.com web services** - they **really** are going away on 15 June 2020 Thanks.
