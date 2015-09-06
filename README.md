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

Looking at the top of the gui from left to right we have: the click-mode selection
- either Polygons or Lines; the general operating mode selection - either Draw or
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



## License

Copyright © 2015 Dave Rogers

Distributed under the Eclipse Public License either version 1.0 or any later
version.
