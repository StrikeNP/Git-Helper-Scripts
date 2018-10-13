* $Id: plot_profile.gs 861 2017-03-02 05:55:34Z bmg2@uwm.edu $
* Plot a vertical profile that can be time averaged.
function main( args )

* Get GrADS *.ctl filename.
filename = subwrd( args, 1 )
* Get the field to be plotted.
field = subwrd( args, 2 )
* Get the initial time in time averaging.
timeinit = subwrd( args, 3 )
* Get the last time in time averaging.
timelast = subwrd( args, 4 )
* Get the prefix name of the output file.
outfile = subwrd( args, 5 )
* Get the x-axis label.
xlabel = subwrd( args, 6 )
* Get the units for the x-axis label.
xlabelun = subwrd( args, 7 )
* Get the plot title.
title = subwrd( args, 8 )

* Open the GrADS *.ctl file.
'open 'filename

* Get the number of vertical levels and the number of output timesteps.
'query file 1'
output = sublin( result, 5 )
numlevs = subwrd( output, 9 )
numtimes = subwrd( output, 12 )

* Set the vertical domain.
'set z 1 'numlevs

* Set the time averaging period.
* For output from one time, set timeinit and timelast to the same time index.
* Set timeinit to 0 to average over all output times.
if ( timeinit > 0 )
   if ( timeinit <= numtimes )
      time1 = timeinit
   else
      time1 = numtimes
   endif
   if ( timelast <= numtimes )
      time2 = timelast
   else
      time2 = numtimes
   endif
else
   if ( timeinit = 0 )
      time1 = 1
      time2 = numtimes
   endif
endif

* Plot the field
'display ave( 'field', t = 'time1', t = 'time2' )'

* Exit GrADS if the variable isn't found.
if ( rc != 0 )
   'quit'
endif

'draw xlab 'xlabel'    'xlabelun
'draw ylab Height    [m]'
'draw title 'title

'enable print 'outfile'.m'
'print'
'disable print'

* Close the GrADS *.ctl file.
'close 1'

* Exit GrADS
'quit'
