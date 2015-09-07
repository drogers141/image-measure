# image-measure

Tool for estimating linear and square areas on 2d images given some known quantity
to scale with.  Designed to find unknown lengths or area in floorplans where at
least one length or the area is known.  Implemented in Clojure using the Seesaw library and some custom
Swing.

## Usage
Download the jar file [image-measure.jar] (./image-measure.jar):

```
java -jar image-measure.jar [options] [image-file]

Options:
    -h, --help  Print this help

image-file:
    Path to image file to work with.
    Note gui cannot resize image.  Recommended to work with images at least 800 px wide for clarity of labeling, etc.
    If image-file is not provided, program is run with image used in the examples below.
```

## Intro/Tutorial
The way the image-measure program works to obtain scaled distances and areas is
to allow the user to draw lines and polygons over the image, which will then be
labeled by a calculation.  The calculation uses as input the scaled length of
either the length of a line, the area of a polygon, or in the case of free lines
drawn after a polygon has been computed, no input.

Running the program with no image file parameter will use a stock image of a
floorplan that I took from a Craigslist ad and expanded with ImageMagick:

![Image not available..)] (doc/tut-1.jpg)

Looking at the top of the gui from left to right we have: the click-mode selection -
either Polygons or Lines; the general operating mode selection - either Draw or
Calculate; the entry for Area; a Line selector and Line Length entry; the Calculate
button; and a Line Width selector for width of drawn lines.

The general flow of usage is to draw lines or polygons in Draw mode, then switch
to Calculate mode, possibly select a polygon if the calculation is for a polygon,
then input either area or line length for a selected line and click the calculate
button.  At that point all drawn lines will be labeled with their scale length and
any polygons will be labeled with their area.  After calculating you can click back
into Draw mode and repeat the process to add more lines or polygons.  Lines and
labels can be cleared using the buttons on the bottom of the gui.  The image with
all lines and labels can be saved by clicking the Save Image button.  Note that
the image can only be saved as a PNG file and *.png must be used for the file name.

### Calculating From Known Area
For this part of the tutorial, I will be drawing in Polygon mode, then calculating based on
area, then adding some free lines which can be calculated without specifying any
input line length as the calculation will be made based on the the lines from the
polygon.

To draw a polygon, click on a point and drag to create a first line.

![Image not available..)] (doc/tut-2.jpg)

Then add lines to the polygon by clicking on one of the end points to start the
next line.

![Image not available..)] (doc/tut-3.jpg)

To close the polygon, click on one end point and drag to the other.  If the program
recognizes a polygon, the lines will turn green.

![Image not available..)] (doc/tut-4.jpg)

With a polygon drawn, we are ready to make a calculation.  First click the Calculate
radio button to enter Calculate mode, then click on a line of the polygon to select
it.  This will enable the Area and Line Length inputs, and populate the Line selector.
It will also label each line with a number which corresponds to the Line selector wheel.
If you wanted to use the length of a line as the basis for the scale you would select that
line with the Line input and enter its length in the Line Length input.

![Image not available..)] (doc/tut-5.jpg)

Since we have the area of the apartment given as 587 square feet, we instead enter
587 in the Area input and hit the Calculate button.  This populates all line labels
and the area label for the polygon.

![Image not available..)] (doc/tut-6.jpg)

This gives us a lot of information about the estimated sizes in the apartment, but
we can do more.  Now select the Draw radio to go back into Draw mode, and the Lines
radio to go into the Lines click-mode.  Then draw lines of desired dimensions.  Note
that you can draw a line on top of another line if it makes the drawing more clear.
The line labels will always be centered on their line and on top of all drawn lines
so you can figure out which applies.  Note lines drawn in Lines mode are blue.

![Image not available..)] (doc/tut-7.jpg)

Note that I have drawn two blue lines over the right line (22 ft).  This will add
two more labels on the same line, but keeps the rest of the floorplan a little
clearer.  Now, since we already have calculations made to determine the scale from
the previous polygon, just select the Calculate radio and hit the Calculate button
without entering any input.

![Image not available..)] (doc/tut-8.jpg)

This result can be saved as a PNG file by clicking the Save Image button and providing
a *.png filename.

### Calculating From A Known Length
What if we didn't know the area, but knew the length of at least one line?  Here
I've recreated our initial polygon, entered Calculate mode, and selected it by
clicking on it.

![Image not available..)] (doc/tut-9.jpg)

Looking at this image, I can see that the rightmost line is labeled number 2.  I
know that this line worked out to be 22 feet long last time, so I select Line 2
in the Line wheel selector and enter 22 in the input.

![Image not available..)] (doc/tut-10.jpg)

Now I click calculate and get similar results from the area-based calculation.

![Image not available..)] (doc/tut-11.jpg)

Note their are differences, since we are estimating based off of drawn lines, but
the measurements are quite close.



## License

Copyright © 2015 Dave Rogers

Distributed under the Eclipse Public License either version 1.0 or any later
version.
