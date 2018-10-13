* $Id$
* Plot a surface time series.
function main( args )

* Get GrADS *.ctl filename.
filename = subwrd( args, 1 )
* Get the field to be plotted.
field = subwrd( args, 2 )
* Get the prefix name of the output file.
outfile = subwrd( args, 3 )
* Get the y-axis label.
ylabel = subwrd( args, 4 )
* Get the units for the y-axis label.
ylabelun = subwrd( args, 5 )
* Get the plot title.
title = subwrd( args, 6 )

* Open the GrADS *.ctl file.
'open 'filename

* Get the number of vertical levels and the number of output timesteps.
'query file 1'
output = sublin( result, 5 )
numtimes = subwrd( output, 12 )

time1 = 1
time2 = numtimes

'set t 'time1' 'time2

* Plot the field
'display 'field

* Exit GrADS if the variable isn't found.
if ( rc != 0 )
   'quit'
endif

'draw xlab Time    [UTC]'
'draw ylab 'ylabel'    'ylabelun
'draw title 'title

'enable print 'outfile'.m'
'print'
'disable print'

* Close the GrADS *.ctl file.
'close 1'

* Exit GrADS
'quit'
