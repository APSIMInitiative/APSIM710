      module cropmodmodule
      use cropmoddata
      use croplibrary

      contains

      include 'CropModComms.for'
      include 'cropmodtree.for'
c      include 'maize.for'
      include 'process.for'
      include 'sorg.for'
      include 'sunf.for'
      include 'wheat.for'

      end module cropmodmodule
