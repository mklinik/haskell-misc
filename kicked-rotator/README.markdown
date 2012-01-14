kicked-rotator
==============

calculates colorful visualisations for the kicked rotator model.

![output of kicked-rotator](https://github.com/mklinik/haskell-misc/raw/master/kicked-rotator/foo.png)

[Kicked Rotator at Wikipedia](http://en.wikipedia.org/wiki/Kicked_rotator)

If you have an image viewer that can display ppm files, run like this:

    ghc --make Main && ./Main > foo.ppm

If you have the netpbmtools installed, run like this to generate a png image:

    ghc --make Main && ./Main | pnmtopng > foo.png
