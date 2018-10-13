* $Id: plot_map.gs 862 2017-03-10 06:34:16Z bmg2@uwm.edu $
* Plots the map.
function main( args )

* Get the GrADS *.ctl filename.
filename = subwrd( args, 1 )
* Get the field to be plotted.
field = subwrd( args, 2 )
* Get the vertical level index for the plot level.
vertlev = subwrd( args, 3 )
* Get the starting time index for plots.
startidx = subwrd( args, 4 )
* Get the increment between the plots.
timeincr = subwrd( args, 5 )
* Get the prefix name of the output file.
outfile = subwrd( args, 6 )
* Get the plot title.
title = subwrd( args, 7 )

* Open the GrADS *.ctl file.
'open 'filename

* Get the number of output times in the GrADS output file.
'query file 1'
output = sublin( result, 5 )
numlons = subwrd( output, 3 )
numlats = subwrd( output, 6 )
numtimes = subwrd( output, 12 )

'set x 1 'numlons
'set y 1 'numlats
* When vertlev > 0, plot the field at the requested level.
* When vertlev = 0, plot the average value of the field over all levels.
* When vertlev = -1, plot the maximum value of the field over all levels.
* When vertlev = -2, plot the minimum value of the field over all levels.
if ( vertlev > 0 )
   'set z 'vertlev
else
   numlevs = subwrd( output, 9 )
endif

* Use the high resolution map background.
'set mpdset hires'

timeidx = startidx
while ( timeidx <= numtimes )
   'set t 'timeidx
* When vertlev > 0, plot the field at the requested level.
* When vertlev = 0, plot the average value of the field over all levels.
* When vertlev = -1, plot the maximum value of the field over all levels.
* When vertlev = -2, plot the minimum value of the field over all levels.
   if ( vertlev > 0 )
      'display 'field
      if ( rc != 0 )
      'quit'
      endif
   else
      if ( vertlev = 0 )
         'display ave( 'field', z = 1, z = 'numlevs' )'
         if ( rc != 0 )
            'quit'
         endif
      endif
      if ( vertlev = -1 )
         'display max( 'field', z = 1, z = 'numlevs' )'
         if ( rc != 0 )
            'quit'
         endif
      endif
      if ( vertlev = -2 )
         'display min( 'field', z = 1, z = 'numlevs' )'
         if ( rc != 0 )
            'quit'
         endif
      endif
   endif
* Print the title time information on the plot.
'query time'
timestr=subwrd( result, 3 )
hourstr=substr( timestr, 1, 3 )
daystr=substr( timestr, 4, 2 )
monthstr=substr( timestr, 6, 3 )
yearstr=substr( timestr, 9, 4 )
'set strsiz 0.18'
'draw string 0.75 1 'monthstr' 'daystr', 'yearstr' 'hourstr
'draw string 4.5 1 'title
* Modify the time index portion of the output file name so that it always uses
* the same number of digits.
   if ( numtimes >= 100 )
      if ( timeidx < 10 )
         'enable print 'outfile'-00'timeidx'.m'
      else
         if ( timeidx < 100 )
            'enable print 'outfile'-0'timeidx'.m'
         else
            'enable print 'outfile'-'timeidx'.m'
         endif
      endif
   else
      if ( numtimes >= 10 )
         if ( timeidx < 10 )
            'enable print 'outfile'-0'timeidx'.m'
         else
            'enable print 'outfile'-'timeidx'.m'
         endif
      else
         'enable print 'outfile'-'timeidx'.m'
      endif
   endif
   'print'
   'disable print'
   'clear'
   timeidx = timeidx + timeincr
endwhile

* Close the GrADS *.ctl file.
'close 1'

* Exit GrADS
'quit'
