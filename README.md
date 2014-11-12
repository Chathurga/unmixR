### unmixR
#### An R package for unmixing of hyperspectral images

`unmixR` is a **WORK IN PROGRESS**.  The fundamental structures & behavior may change frequently. Use at your own risk.

**Installation:** works easiest using `devtools::install_github`:

    require ("devtools")
    install_github (repo="Chathurga/unmixR/pkg/unmixR")


`unmixR` is supported by [Google Summer of Code](http://www.google-melange.com/gsoc/homepage/google/gsoc2013).  Thank you!

Note: hyperspectral data are also called 'imaging spectroscopy' and 'imaging spectrometer data' depending upon the discipline.  Such data consists of spectra collected over an x, y grid.  Data sets like this are found in airborne land imaging studies, biomedical studies and art history investigations.  The spectra are often visible, infrared, near-infrared, raman spectra or mass spectrometer data sets.

#### Things to do and Things to remember + Misc Notes

An informal list to keep us on track.  Naturally, edit as you see fit.

##### Top priority
* Unit tests on nfindr variants
* Add'l comments for nfindrLDU and nfindrSeqLDU
* Add options beyond nnls to predict.nfindr
* Improve endmember.R
* nfindr variants (do these all work as expected?)
    * nfindr99
    * nfindrLDU
    * nfindrSeqLDU
* class definition(s): nfindr currently informally defined at end of nfindr.default.  Do we need/want to create an Rd page about it?
* changes needed for build & check (?)
* vignettes

##### Lower priority

* methods for finding the number of endmembers (recommended in Harsayni2012)
    * hfc (hfcVAR and hfcSAMPLE need discussion and testing)
    * nwhfc
    * sml (second moment linear)
    * Hysime (hyperspectral signal subspace identification by minimum error)

##### Misc notes

* ?

