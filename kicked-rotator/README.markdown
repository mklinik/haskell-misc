kicked-rotator
==============

calculates colorful visualisations for the kicked rotator model.

[Kicked Rotator at Wikipedia](http://en.wikipedia.org/wiki/Kicked_rotator)

If you have an image viewer that can display ppm files, run like this:

    ghc --make Main && ./Main > foo.ppm

If you have the netpbmtools installed, run like this to generate a png image:

    ghc --make Main && ./Main | pnmtopng > foo.png
