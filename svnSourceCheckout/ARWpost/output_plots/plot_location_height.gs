* $Id: plot_location_height.gs 862 2017-03-10 06:34:16Z bmg2@uwm.edu $
* Plots a time and height series at a location.
function main( args )

* Get the GrADS *.ctl filename.
filename = subwrd( args, 1 )
* Get the field to be plotted.
field = subwrd( args, 2 )
* Get the latitude of the location.
loclat = subwrd( args, 3 )
* Get the longitude of the location.
loclon = subwrd( args, 4 )
* Get the prefix name of the output file.
outfile = subwrd( args, 5 )
* Get the plot title.
title = subwrd( args, 6 )

* Open the GrADS *.ctl file.
'open 'filename

* Get the number of output times in the GrADS output file.
'query file 1'
output = sublin( result, 5 )
numlevs = subwrd( output, 9 )
numtimes = subwrd( output, 12 )

'set lat 'loclat
'set lon 'loclon
'set z 1 'numlevs
'set t 1 'numtimes

'display 'field

* Exit GrADS if the variable isn't found.
if ( rc != 0 )
   'quit'
endif

'set grid on'

'draw xlab Time    [UTC]'
'draw ylab Vertical Level Index'
'draw title 'title

'enable print 'outfile'.m'
'print'
'disable print'

* Close the GrADS *.ctl file.
'close 1'

* Exit GrADS
'quit'
