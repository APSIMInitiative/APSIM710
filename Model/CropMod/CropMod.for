      module cropmodmodule
      use cropmoddata
      use croplibrary
      use infrastructure

      contains

      include 'CropModComms.for'
      include 'cropmodtree.for'
c      include 'maize.for'
      include 'process.for'
      include 'sorg.for'
      include 'wheat.for'
      include 'sunf.for'

      end module cropmodmodule
