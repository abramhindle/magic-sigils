#*********************************************************************#
#                                                                     #
#                          Caml Images                                #
#                                                                     #
#            François Pessaux, projet Cristal, INRIA Rocquencourt     #
#            Pierre Weis, projet Cristal, INRIA Rocquencourt          #
#            Jun Furuse, projet Cristal, INRIA Rocquencourt           #
#                                                                     #
#  Copyright 1999-2004,                                               #
#  Institut National de Recherche en Informatique et en Automatique.  #
#  Distributed only by permission.                                    #
#                                                                     #
#*********************************************************************#

#(* $Id: Makefile.config.in,v 1.28 2004/10/02 15:49:05 weis Exp $ *)

PACKAGE=camlimages
VERSION=2.2.0

MV=mv -f
RM=rm -fR
CP=cp -pfR

CAMLDIR=/usr/lib/ocaml/3.08
LIBDIR=/usr/lib/ocaml/2.08/camlimages

CAMLC = ocamlc 
CAMLOPT = ocamlopt 
CAMLC_LABEL = ocamlc -labels
CAMLOPT_LABEL = ocamlopt -labels
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc
CAMLMKTOP = ocamlmktop
CAMLMKLIB = ocamlmklib
CUSTOM=-custom

SUPPORTED=  lablgtk lablgtk2 lablgl bmp ppm gif png jpeg tiff xpm freetype(2) ps

SUPPORT_GIF=true
SUPPORT_PNG=true
SUPPORT_JPEG=true
SUPORT_TIFF=true
SUPPORT_FREETYPE=true
SUPPORT_FREETYPE2=true
SUPPORT_PS=true
SUPPORT_LABLGTK=true
SUPPORT_GDKPIXBUF=
SUPPORT_LABLGTK2=false

LIBGIF=-lgif
LIBBMP=
LIBJPEG=-ljpeg
LIBTIFF=-ltiff
LIBFREETYPE=-lfreetype -lz
LIBPNG=-lpng -lz
LIBXVTHUMB=
LIBXPM=-L/usr/X11R6/lib -lXpm
LIBPPM=
LIBPS=

CFLAGS=-g -O2
LDFLAGS=
X_CFLAGS=

INCLUDE_FREETYPE=-I/usr/include/freetype2

LABLGTKDIR=/usr/lib/ocaml/3.08/lablgtk
LABLGTK2DIR=/usr/lib/ocaml/3.08/lablgtk2

RANLIB=ranlib

LIBPREFIX =	ci_

WITH_UNIX=	unix.cmxa

WITH_CORELIB  = $(LIBPREFIX)core.cmxa

WITH_GRAPHICS =	graphics.cmxa $(LIBPREFIX)graphics.cmxa

WITH_GIF = 	$(LIBPREFIX)gif.cmxa

WITH_JPEG =	$(LIBPREFIX)jpeg.cmxa

WITH_TIFF =	$(LIBPREFIX)tiff.cmxa

WITH_XPM =	$(LIBPREFIX)xpm.cmxa

WITH_BMP =	$(LIBPREFIX)bmp.cmxa

WITH_PPM =	$(LIBPREFIX)ppm.cmxa

WITH_XVTHUMB=	$(LIBPREFIX)xvthumb.cmxa

WITH_PNG =	$(LIBPREFIX)png.cmxa

WITH_PS =	$(LIBPREFIX)ps.cmxa

WITH_FREETYPE=	$(LIBPREFIX)freetype.cmxa

WITH_CAMLIMAGES = $(WITH_CORELIB) $(WITH_GRAPHICS) $(WITH_FREETYPE) \
	$(WITH_GIF) $(WITH_JPEG) $(WITH_TIFF) $(WITH_BMP) $(WITH_PPM) \
        $(WITH_PNG) $(WITH_XVTHUMB) $(WITH_XPM) $(WITH_PS)

# only lablgtk_img lablgtk2_img is separated from $(WITH_CAMLIMAGES)

WITH_LABLGTKIMAGE = $(LIBPREFIX)lablgtk.cmxa
WITH_LABLGTK2IMAGE = $(LIBPREFIX)lablgtk2.cmxa

######################################

#GSL_INC=-I +bigarray bigarray.cmxa -I /usr/lib/ocaml/site-lib/gsl/ -I /usr/lib/ocaml/3.09.2/gsl/  gsl.cmxa 

GSL_INC=-I +bigarray bigarray.cmxa -I `ocamlfind query gsl` -I /usr/lib/ocaml/site-lib/gsl/ gsl.cmxa 

COMPFLAGS_CAMLIMAGES= -I $(LIBDIR)
LINKFLAGS_CAMLIMAGES= $(addprefix -ccopt \"-L, $(addsuffix\", $(LIBDIR))) $(WITH_CAMLIMAGES) $(GSL_INC)

#IMAGE=./aim/aim.1962.1143183171.jpg 
#IMAGE=./x.jpg
IMAGE=data/rogers/images/out/A0TQGZ.jpg

view: rogers
	./$< ${IMAGE}

aim_c:	aim_c.ml abez.cmx munkres.cmx shape.cmx captchas.cmx lapjv.cmx
	ocamlopt.opt -w s -o aim_c  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx aim_c.ml

shrinker_test:	shrinker_test.ml abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx
	ocamlopt.opt -w s -o shrinker_test  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx shrinker_test.ml

erode_test:	erode_test.ml abez.cmx munkres.cmx shape.cmx captchas.cmx erode.cmx
	ocamlopt.opt -w s -o erode_test  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx erode.cmx erode_test.ml

inverse:	inverse_test.ml abez.cmx munkres.cmx shape.cmx captchas.cmx inverse.cmx
	ocamlopt.opt -w s -o inverse_test  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx inverse.cmx inverse_test.ml

hard_pixel_remove:	hard_pixel_remove.ml abez.cmx munkres.cmx shape.cmx captchas.cmx line.cmx
	ocamlopt.opt -w s -o hard_pixel_remove  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx line.cmx hard_pixel_remove.ml

threshold:	threshold.ml abez.cmx munkres.cmx shape.cmx captchas.cmx line.cmx
	ocamlopt.opt -w s -o threshold  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx line.cmx threshold.ml

shrinker:	shrinker_driver.ml abez.cmx munkres.cmx shape.cmx captchas.cmx line.cmx shrinker.cmx inverse.cmx
	ocamlopt.opt -w s -o shrinker  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} abez.cmx shrinker.cmx lapjv.cmx munkres.cmx shape.cmx captchas.cmx inverse.cmx line.cmx shrinker_driver.ml



skeletonize_test:	skeletonize_test.ml abez.cmx munkres.cmx shape.cmx captchas.cmx skeletonize.cmx erode.cmx bmatrix.cmx
	ocamlopt.opt -w s -o skeletonize_test  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx bmatrix.cmx erode.cmx skeletonize.cmx skeletonize_test.ml


rogers:	rogers.ml abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx contour.cmx pca.cmx rotter.cmx
	ocamlopt.opt -w s -o rogers  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx contour.cmx pca.cmx rotter.cmx rogers.ml

rogers_segmenter:	rogers_segmenter.ml abez.cmx shape.cmx captchas.cmx shrinker.cmx contour.cmx pca.cmx rotter.cmx
	ocamlopt.opt -w s -o rogers_segmenter  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx shape.cmx captchas.cmx shrinker.cmx contour.cmx pca.cmx rotter.cmx rogers_segmenter.ml


#digg:	digg.ml abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx contour.cmx pca.cmx rotter.cmx
#	ocamlopt.opt -w s -o digg  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx contour.cmx pca.cmx rotter.cmx digg.ml

digg:	digg_solver.ml abez.cmx munkres.cmx shape.cmx contour.cmx captchas.cmx shrinker.cmx pca.cmx rotter.cmx kmeans.cmx digg.cmx
	ocamlopt.opt -w s -o digg  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx contour.cmx pca.cmx rotter.cmx kmeans.cmx digg.cmx digg_solver.ml

sigils_driver:	sigils_driver.ml abez.cmx munkres.cmx shape.cmx contour.cmx captchas.cmx shrinker.cmx pca.cmx rotter.cmx kmeans.cmx sigils.cmx
	ocamlopt.opt -w s -o sigils_driver  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx contour.cmx pca.cmx rotter.cmx kmeans.cmx sigils.cmx sigils_driver.ml



SEEDPEERCMX=bmatrix.cmx digg.cmx erode.cmx skeletonize.cmx seedpeer.cmx 
seedpeer: seedpeer_solver.ml abez.cmx munkres.cmx shape.cmx contour.cmx captchas.cmx shrinker.cmx pca.cmx rotter.cmx kmeans.cmx ${SEEDPEERCMX}
	ocamlopt.opt -w s -o seedpeer  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx contour.cmx pca.cmx rotter.cmx kmeans.cmx ${SEEDPEERCMX} seedpeer_solver.ml



cumulative_segmenter:	cumulative_segmenter.ml abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx
	ocamlopt.opt -w s -o cumulative_segmenter  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx cumulative_segmenter.ml

digg_segmenter:	digg_segmenter.ml abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx pca.cmx contour.cmx rotter.cmx kmeans.cmx digg.cmx 
	ocamlopt.opt -w s -o digg_segmenter  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx pca.cmx contour.cmx rotter.cmx kmeans.cmx digg.cmx  digg_segmenter.ml

seedpeer_segmenter:	seedpeer_segmenter.ml abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx pca.cmx contour.cmx rotter.cmx kmeans.cmx digg.cmx seedpeer.cmx ${SEEDPEERCMX}
	ocamlopt.opt -w s -o seedpeer_segmenter  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx pca.cmx contour.cmx rotter.cmx kmeans.cmx ${SEEDPEERCMX}  seedpeer_segmenter.ml


graphbased_test:	graphbased_test.ml abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx contour.cmx graphbased.cmx
	ocamlopt.opt -w s -o graphbased_test  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx graphbased.cmx contour.cmx graphbased_test.ml

rotter_test:	rotter_test.ml abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx contour.cmx pca.cmx rotter.cmx
	ocamlopt.opt -w s -o rotter_test  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx contour.cmx pca.cmx rotter.cmx rotter_test.ml


aim_segmenter:	aim_segmenter.ml abez.cmx munkres.cmx shape.cmx captchas.cmx lapjv.cmx
	ocamlopt.opt -w s -o aim_segmenter  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx aim_segmenter.ml

aim_ml:	aim_ml.ml abez.cmx munkres.cmx shape.cmx captchas.cmx lapjv.cmx ml.cmx fft.cmx
	ocamlopt.opt -w s -o aim_ml  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx fft.cmx ml.cmx captchas.cmx aim_ml.ml

shape_test: shape_test.ml abez.cmx munkres.cmx shape.cmx
	ocamlopt.opt -w s -o shape_test  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} abez.cmx munkres.cmx shape.cmx shape_test.ml

shape_sketch: shape_sketch.ml abez.cmx munkres.cmx shape.cmx contour.cmx captchas.cmx pca.cmx rotter.cmx lapjv.cmx
	ocamlopt.opt -w s -o shape_sketch  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} abez.cmx lapjv.cmx munkres.cmx shape.cmx captchas.cmx  contour.cmx pca.cmx rotter.cmx shape_sketch.ml




fill_segmenter:	fill_segmenter.ml lapjv.cmx captchas.cmx abez.cmx munkres.cmx shape.cmx 
	ocamlopt.opt -w s -o fill_segmenter  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} abez.cmx lapjv.cmx munkres.cmx shape.cmx captchas.cmx fill_segmenter.ml

toarff: toarff.ml abez.cmx captchas.cmx munkres.cmx 
	ocamlopt.opt -w s -o toarff  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} abez.cmx lapjv.cmx munkres.cmx shape.cmx captchas.cmx toarff.ml




phpbb: phpbb.ml 
	ocamlopt.opt -w s -o phpbb -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES}   phpbb.ml

ebaum: ebaum.ml 
	ocamlopt.opt -w s -o ebaum -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES}   ebaum.ml

lilo: lilo.ml 
	ocamlopt.opt -w s -o lilo -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES}   lilo.ml


ebaum: ebaum.ml 
	ocamlopt.opt -w s -o ebaum  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES}   ebaum.ml

captchas.cmx: captchas.ml abez.cmx lapjv.cmx munkres.cmx shape.cmx 
	ocamlopt.opt -c -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx munkres.cmx shape.cmx abez.cmx  captchas.ml

shrinker.cmx: shrinker.ml captchas.cmx abez.cmx lapjv.cmx munkres.cmx shape.cmx
	ocamlopt.opt -c -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx munkres.cmx shape.cmx abez.cmx  shrinker.ml

erode.cmx: erode.ml captchas.cmx abez.cmx lapjv.cmx munkres.cmx shape.cmx
	ocamlopt.opt -c -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx munkres.cmx shape.cmx abez.cmx  erode.ml

inverse.cmx: inverse.ml captchas.cmx abez.cmx lapjv.cmx munkres.cmx shape.cmx
	ocamlopt.opt -c -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx munkres.cmx shape.cmx abez.cmx  inverse.ml

line.cmx: line.ml captchas.cmx abez.cmx lapjv.cmx munkres.cmx shape.cmx
	ocamlopt.opt -c -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx munkres.cmx shape.cmx abez.cmx  line.ml


skeletonize.cmx: skeletonize.ml captchas.cmx abez.cmx lapjv.cmx munkres.cmx shape.cmx bmatrix.cmx
	ocamlopt.opt -c -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx munkres.cmx shape.cmx abez.cmx  skeletonize.ml


graphbased.cmx: graphbased.ml captchas.cmx abez.cmx lapjv.cmx munkres.cmx shape.cmx
	ocamlopt.opt -c -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx munkres.cmx shape.cmx abez.cmx  graphbased.ml

digg.cmx: digg.ml captchas.cmx abez.cmx lapjv.cmx munkres.cmx shape.cmx contour.cmx kmeans.cmx
	ocamlopt.opt -c -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx munkres.cmx shape.cmx abez.cmx contour.cmx kmeans.cmx digg.ml

sigils.cmx: sigils.ml captchas.cmx abez.cmx lapjv.cmx munkres.cmx shape.cmx contour.cmx kmeans.cmx
	ocamlopt.opt -c -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx munkres.cmx shape.cmx abez.cmx contour.cmx kmeans.cmx sigils.ml


seedpeer.cmx: seedpeer.ml captchas.cmx abez.cmx lapjv.cmx munkres.cmx shape.cmx contour.cmx kmeans.cmx digg.cmx
	ocamlopt.opt -c -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx munkres.cmx shape.cmx abez.cmx contour.cmx kmeans.cmx digg.cmx seedpeer.ml


rotter.cmx: rotter.ml captchas.cmx abez.cmx lapjv.cmx munkres.cmx shape.cmx pca.cmx contour.cmx
	ocamlopt.opt -c -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx munkres.cmx shape.cmx abez.cmx pca.cmx rotter.ml


contour.cmx: contour.ml captchas.cmx abez.cmx lapjv.cmx munkres.cmx shape.cmx shrinker.cmx
	ocamlopt.opt -c -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx munkres.cmx shape.cmx abez.cmx  contour.ml

pca.cmx: pca.ml
	ocamlopt.opt -c ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} pca.ml

bmatrix.cmx: bmatrix.ml
	ocamlopt.opt -c ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} bmatrix.ml

pca_test: pca_test.ml pca.cmx
	ocamlopt.opt -o pca_test -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} pca.cmx pca_test.ml

abez.cmx: abez.ml 
	ocamlopt.opt -c abez.ml

kmeans.cmx: kmeans.ml 
	ocamlopt.opt -c kmeans.ml

knn.cmx: knn.ml 
	ocamlopt.opt -c knn.ml


munkres.cmx: munkres.ml
	ocamlopt.opt -c munkres.ml

ml.cmx: ml.ml fft.cmx
	ocamlopt.opt -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES}  -c ml.ml

fft.cmx: fft.ml 
	ocamlopt.opt -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES}  -c fft.ml


lapjv.cmx: lapjv.ml abez.cmx
	ocamlopt.opt  -c lapjv.ml

shape.cmx: shape.ml lapjv.cmx abez.cmx
	ocamlopt.opt  -c shape.ml

captcha: captcha.ml 
	ocamlopt.opt -w s -o captcha  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES}   captcha.ml

fmatrix.ml: fmatrix.ml.m4 absmatrix.ml
	m4 fmatrix.ml.m4 > fmatrix.ml

imatrix.ml: imatrix.ml.m4 absmatrix.ml
	m4 imatrix.ml.m4 > imatrix.ml

bmatrix.ml: bmatrix.ml.m4 absmatrix.ml
	m4 bmatrix.ml.m4 > bmatrix.ml


PIRATE_BAYCMX=bmatrix.cmx digg.cmx erode.cmx skeletonize.cmx inverse.cmx line.cmx pirate_bay.cmx 
PUNBBCMX=bmatrix.cmx digg.cmx erode.cmx skeletonize.cmx inverse.cmx line.cmx punbb.cmx 

REDDITCMX=bmatrix.cmx digg.cmx erode.cmx skeletonize.cmx inverse.cmx line.cmx knn.cmx reddit.cmx 

pirate_bay: pirate_bay_solver.ml abez.cmx munkres.cmx shape.cmx contour.cmx captchas.cmx shrinker.cmx pca.cmx rotter.cmx kmeans.cmx ${PIRATE_BAYCMX}
	ocamlopt.opt -w s -o pirate_bay  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx contour.cmx pca.cmx rotter.cmx kmeans.cmx ${PIRATE_BAYCMX} pirate_bay_solver.ml

pirate_bay_segmenter:	pirate_bay_segmenter.ml abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx pca.cmx contour.cmx rotter.cmx kmeans.cmx digg.cmx pirate_bay.cmx ${PIRATE_BAYCMX}
	ocamlopt.opt -w s -o pirate_bay_segmenter  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx pca.cmx contour.cmx rotter.cmx kmeans.cmx ${PIRATE_BAYCMX}  pirate_bay_segmenter.ml

pirate_bay.cmx: pirate_bay.ml captchas.cmx abez.cmx lapjv.cmx munkres.cmx shape.cmx contour.cmx kmeans.cmx digg.cmx bmatrix.cmx digg.cmx erode.cmx skeletonize.cmx line.cmx inverse.cmx
	ocamlopt.opt -c -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES}  pirate_bay.ml



reddit: reddit_driver.ml abez.cmx munkres.cmx shape.cmx contour.cmx captchas.cmx shrinker.cmx pca.cmx rotter.cmx kmeans.cmx ${REDDITCMX}
	ocamlopt.opt -w s -o reddit  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx contour.cmx pca.cmx rotter.cmx kmeans.cmx ${REDDITCMX} reddit_driver.ml

reddit_segmenter:	reddit_segmenter.ml abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx pca.cmx contour.cmx rotter.cmx kmeans.cmx digg.cmx reddit.cmx ${REDDITCMX}
	ocamlopt.opt -w s -o reddit_segmenter  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx pca.cmx contour.cmx rotter.cmx kmeans.cmx ${REDDITCMX}  reddit_segmenter.ml

reddit.cmx: reddit.ml captchas.cmx abez.cmx lapjv.cmx munkres.cmx shape.cmx contour.cmx kmeans.cmx digg.cmx bmatrix.cmx digg.cmx erode.cmx skeletonize.cmx line.cmx inverse.cmx
	ocamlopt.opt -c -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES}  reddit.ml



punbb_solver: punbb_solver.ml abez.cmx munkres.cmx shape.cmx contour.cmx captchas.cmx shrinker.cmx pca.cmx rotter.cmx kmeans.cmx ${PUNBBCMX}
	ocamlopt.opt -w s -o punbb_solver  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx contour.cmx pca.cmx rotter.cmx kmeans.cmx ${PUNBBCMX} punbb_solver.ml

punbb_segmenter:	punbb_segmenter.ml abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx pca.cmx contour.cmx rotter.cmx kmeans.cmx digg.cmx punbb.cmx ${PUNBBCMX}
	ocamlopt.opt -w s -o punbb_segmenter  -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES} lapjv.cmx abez.cmx munkres.cmx shape.cmx captchas.cmx shrinker.cmx pca.cmx contour.cmx rotter.cmx kmeans.cmx ${PUNBBCMX}  punbb_segmenter.ml

punbb.cmx: punbb.ml captchas.cmx abez.cmx lapjv.cmx munkres.cmx shape.cmx contour.cmx kmeans.cmx digg.cmx bmatrix.cmx digg.cmx erode.cmx skeletonize.cmx line.cmx inverse.cmx
	ocamlopt.opt -c -I +camlimages ${COMPFLAGS_CAMLIMAGES}  ${LINKFLAGS_CAMLIMAGES}  punbb.ml





munkres_test: munkres_test.ml munkres.cmx
	ocamlopt.opt -o munkres_test munkres.cmx munkres_test.ml


webcam_provider:        webcam_provider.ml ocaml-v4l/grabcaml.cmxa
	ocamlopt.opt -o webcam_provider -I ocaml-v4l/ -I `ocamlfind query sdl` grabcaml.cmxa  unix.cmxa bigarray.cmxa sdl.cmxa  sobel.cmx webcam_provider.ml ${OCAMLIB}
webcam_requirer:        webcam_requirer.ml ocaml-v4l/grabcaml.cmxa
	ocamlopt.opt -o webcam_requirer -I ocaml-v4l/ -I `ocamlfind query sdl` grabcaml.cmxa  unix.cmxa bigarray.cmxa sdl.cmxa  sobel.cmx webcam_requirer.ml ${OCAMLIB}




clean:

IMAGES=

FILE=report

${FILE}.pdf: ${FILE}.tex ${IMAGES}
	pdflatex $<
	pdflatex $<
	pdflatex $<

.SUFFIXES: .pdf .jpg .eps .png 

.png.pdf:
	convert $< $@

.jpg.pdf:
	convert $< $@

.eps.pdf:
	epstopdf $<


clean:
	rm -rf *.cmx
