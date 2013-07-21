### spec.unmixing: An R package for unmixing of hyperspectral images

`spec.unmixing is a WORK IN PROGRESS`.  The fundamental structures & behavior are changing frequently.

`spec.unmixing` is supported by [Google Summer of Code](http://www.google-melange.com/gsoc/homepage/google/gsoc2013).  Thank you!

#### Things to do and Things to remember + Misc Notes

An informal list to keep us on track.  Naturally, edit as you see fit.

##### Top priority
* nfindr variants
    * nfindr99: In working order, could be optimized
    * nfindrLDU
    * nfindrSeqLDU
* predict.nfindr
* class definition(s)
* changes needed for build & check
* vignettes

##### Lower priority

* methods for finding the number of endmembers (recommended in Harsayni2012)
    * hfc (prelim version seems OK)
    * nwhfc
    * sml (second moment linear)
    * Hysime (hyperspectral signal subspace identification by minimum error)

##### Misc notes

* A possible name for the package is `unmixR`

