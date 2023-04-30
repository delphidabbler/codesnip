About the Src/Res/Img/AltBranding directory
===========================================

If you are creating a fork of CodeSnip, or a program based on it, then
the CodeSnip license does not permit you to use the images in the
`Src/Res/Img/Branding` directory. But, since CodeSnip expects to find
the files `CodeSnip.ico`, `icon.gif` and `Splash.gif` in that directory,
simply deleting the images will cause CodeSnip to fail to build.

Alternative versions of the above images are provided in the
`Src/Res/Img/AltBranding` directory. These files are Public Domain, and
therefore can be used and modified in derived programs.

Simply copy the image files from the `Src/Res/Img/AltBranding` directory
into `Src/Res/Img/Branding`, overwriting the existing files. CodeSnip
can then be built successfully without using the prohibited files.

It is not expected that the images will be used as-is. They are provided
only as placeholders to enable CodeSnip to build successfully.
Evetually you may want to edit the images to meet your needs.
Alternatively, the CodeSnip source could be changed so that the images
are not required at all.

Because the files are Public Domain, you may relicense any modified
version as you wish.


Information about the files
---------------------------

`CodeSnip.ico`

  This is the program's main icon.

  It contains images at four different resolutions: 16×16, 32×32, 48×48
  and 256×256 pixels.

`icon.gif`

  A 32×32 pixel GIF file that is displayed in the About box.

`Splash.gif`

  A 325×155 pixel GIF image that is displayed as a splash screen while
  the program is loading.

  The program's version number is overlaid in the bottom quarter of the
  image.
