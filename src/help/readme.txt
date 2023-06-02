--------------------------The MAUD program readme-------------------------------

Please reports bugs to maud@ing.unitn.it

If you think that you got an error in Maud and the program refuse to do
something or stop a computation; have a look in the  console window. 
Note that errors will go on the console as "Exceptions" with a long list 
corresponding to the class that has generated the error and the classes calling 
it. 
File not found or similar warnings by the program itself instead are shown on 
the console as well but are just a one line without the word exception.
If something seems not to work, please look at the console if an exception
is generated when you do something. You can send to the author the content of 
the console with a brief comment of what you were trying to do.

Have a look on the tutorials on the Maud web page on how to do an analysis.
Also:
After starting the program try to open an anlysis (*.par files) in the files
folder. Actually there are the following examples availables (you can find
some information about these examples in the online tutorial help):
alzrc.par        - two phases example on a Bragg-Brentano XRD instrument
sio250.par       - corundum + silica glass analysis, 50% wt each.
gtial1.par       - gamma-TiAl at D1B (ILL), several spectra collected, texture 
                   by WIMV
bbm48bis.par     - faulting analysis on a beta-brass (two phases) sample
cpd-y2o3.par     - the y2o3 example from the cpd Round Robin
cpd-1h.par       - the example 1h from the cpd Round Robin
Steel16CrNi4.par - example for gamma/alpha/martensite analysis on steels
film1.par        - example of a Ni3Al intermetallic compound from film developed 
                   and digitalized by a normal scanner

Using compute spectrum will compute the fitting. You can have an idea of what
is possible to do with Maud. To enhance the refinement use the wizard dialog.
Read the online help for more. The manual is still in preparation.

The default.par is the starting parameter file used by Maud when the new 
analysis menu item is chosen. It doesn't contain a real analysis.

------------------------------------------------------------------------------
The version notes are a good reading to know what Maud does and also sometimes 
how to work with it. The advice is to read them in the reverse order starting 
from the older one.

Maud Version notes:

(02/06/2023):   2.997. The "Siegfried" release.
                It is so sad we have lost this year such a brilliant mind. Probably
                there wouldn't be Maud as it is now without him or at all.
                Fixing some old bugs and refactoring the weighting schemes. Now you
                can independently set if to subtract background or use calibrated
                data in the weighting for refinement. Other options are to modify
                them by Q (for PDF like fitting) or choosing between linear, sqrt
                or log. Not only the weights but also the real data/fit intensity
                is modified in the WSS depending on the Maud preference:
                testing.intensityModifiedForStatistic (true by default, use false
                if you want to affect only weights).

(16/08/2022):   2.996. The "Arches" release.
                Lot of Arches and red rocks in the weekend.
                Maud is using XrayLib for all scattering and absorption properties
                calculations. Now the number of available atoms from periodic
                table goes to 109.

(12/08/2022):   2.995. The "Matt & Dan" release.
                Thanks to them for the bugs hunting.....
                A lot of improvements, bug fixing. Not released because needed
                extensive testing.

(29/07/2022):   2.994. The "Stout" release.
                You have to empty the beer keg while working on angles conversion.
                That's why it takes so long.....
                The angles conversion routine was re-done again as we spot some
                problems still remain. We found actually (thanks Matt and Sven)
                the last problems and fixed. We are working on a paper to publish
                the details to make it clear for all.
                Finally the angle conversion is working good for Hippo and the rest
                of the world.

(16/02/2022):   2.993. The "Zara" release.
                Jack now has a small replica. Difficult to contain.
                The angle conversion routine at the base of all texture,
                absorption and geometry corrections has been re-worked from
                scratch. There is a Maud preference to either use the new one
                or the old one (1.992 and before): testing.useNewRotationMatrices
                if set to true the new conversion will be used. It is true by
                default. The angle conversion used is still the one defined in
                Figure 1 of the paper: Grasslin et al. J. Appl. Cryst. 46,
                173-180, 2013. There is one error in the appendix there: in the
                matrix multiplication not only the Theta matrix should have a
                minus sign, but also the Eta matrix just after Theta. Otherwise
                we end up with the wrong rotation as it has been corrected
                in version 2.992 (see notes below). Please notice that not all
                rotations are CCW or right ended. But this is what it is in
                Maud. For the sample the angles are not changing the coordinates
                system, but adding the angles of rotation. The difference is
                that of the sign of the angle. So in Maud it is the same
                sign or rotation as for the goniometer rotations and not the
                inverse rotation as it should be for a change in the coordinates
                system of the sample.
                There is the possibility now to change the background in all
                the openGl renderings (the rotating 3D figures like the
                crystallite etc.). You have to edit the Properties.3D file
                in the maud directory (located under your user home directory
                in AppData/Local in Windows or Documents for MacOS/Linux).
                At the end of the "# Crystallite" section before "# Pole Figure"
                there may be a line:
                0.0  0.0  0.0  0.0     // Background only OpenGl
                if the line is not there but it ends with ""// Shineness ...",
                just add this new line for the background.
                The 4 numbers are the Red Green Blue and Alpha values for the
                background color. All zeros is for a black background. The
                maximum value is 1.0 for all 4 and corresponds to white. You
                have to edit the file before starting Maud. Otherwise just
                restart it to change the color.

(28/11/2021):   2.992. The "goofy" release.
                Eta has a left handed coordinates system.
                Fixed an error in loading the GSAS parameter file. To fix it
                temporarily MAUD now loads only the function 1: PRCF1 and ignore
                the others.
                There is a second change that concern how texture angles are
                calculated. There was a mistake in the eta angles tensor that
                affected the texture angles calculation. To correct it now Maud
                considers that the eta angle follow a left handed system when
                viewed from the source as for the others tensors. In most cases
                this only affected the reference system for the texture, but
                in few cases (X-ray in reflection with both eta and chi angles
                different than zero) it affected also the absorption
                correction computation.
                In this version the eta angle is inverted in the calculations
                resulting in the correct angles calcultation for all cases and
                a better absorption correction for some cases.
                But this will change the computation for the old refinements
                when the eta angle is involved (from the angular calibration
                to the texture orientation). So to keep the old analysis files
                still working in this release, Maud also inverts the eta angles
                set for each datafile when it detects a version of the file
                older than this release. So your calculation will remain the
                same. For all the new analyses be careful you may need to
                redo the calibration also if the eta angles are used, and the
                texture may look inverted respect to the vertical axis in
                the pole figures.
                To check the difference between the old and the new way to
                compute texture (and absorption) angles, you can switch
                between the new and old computation using the Maud preference
                "testing.invertEta". If it is true the new (correct) calculation
                is used, false otherwise. Keep in mind that old analyses when
                loaded from an analysis file will have eta inverted before
                the calculation (in the angle setting for each datafile) in
                order to compensate the inversion in the calculation.

(08/07/2021):   2.991. The "supernull" release.
                Sometimes I think the french people have the best definition.
                Fixed a problem on reflectivity. The reflectivity computation
                was never called by mistake.

(06/07/2021):   2.99. The "never ending waiting" release.
                When needed time goes too slow.
                Fixed exporting summed spectra when datafiles have not overlapping
                ranges. Fixed an error in the computation of the polarization
                factor for Bragg-Brentano geometries. The error was introduced
                with version 2.94. Now is fixed in this version.

(31/05/2021):   2.98. The "Knee" release.
                I do not want to talk about it.
                Fixed a couple of outstanding bugs affecting the Hippo
                wizard and the loading of older datafiles using multibanks
                TOF data.

(12/05/2021):   2.97. The "Rudy" release.
                Rudy asked for it by long....
                Improved the 2D plotting for multispectra. Now the routine does
                not interpolate anymore over gaps, but the plot remains white.
                Added some utility functions to help image integration. See the
                new youtube video on how to work with images.
                Update (13/05/2021): fix for new xrdml format.

(05/05/2021):   2.96. The "BCH" release.
                The day today turned for good!
                There is a new plugin for ImageJ to convert .odeiger images
                exported by Crysalis to tiff images ready to be used in Maud.

(12/09/2020):   2.95. The "Forgot" release.
                I forgot what was changed in this release, probably just bug fixes.
                It is also based on a newer java virtual machine. And it was not
                released to public.

(06/04/2020):   2.94. The "Covid-19" release.
                The snow is still out there, plenty of, but we are stuck here...
                Fixed a problem with the Lorentz-polarization (X-ray). There
                were some cases with alpha1 and alpha2 for which one peak
                was below 180 degs but the other not. The program was not
                checking for alpha2 before computing the L-P correction leading
                to very high number if close to 180 degs.
                Some of the LP factors were not computed correctly for some
                geometries (images mainly). So with the new version, loading old
                analysis files may give you a different fitting. Re-run the
                refinement to fix it. It should be just scales and B-factors.
                It may have affected quantitative analysis if the phases had
                very different cell sizes (one large, the other small).

(13/11/2019):   2.93. The "Early snow" release.
                Will we get finally a good "deep snow" winter this year?
                Fixed a problem with strain computation not working.
                Fixed the plugins system that was not loading anymore an
                external jar. For windows/linux the plugins directory to use
                is still in the Maud directory next to the "lib" directory.
                For Macos you should use a directory named "Maud/plugins"
                in your user home under "~/Library/Application Support".

(29/07/2019):   2.92. The "White sands" release.
                You can die easily out there, but thanks to Sven I can still push
                out another release.
                Fixed some bugs for Hippo/TOF refinement plus some minor problems
                with the wizard. New batch refinement for LCLS2 data. MTex
                texture is working, but the odf is still not saved. So everytime
                you close Maud or the analysis you loose the odf and you have
                to recompute it again. Will be fixed in a next release.

(07/01/2019):   2.91. The "Chopper" release.
                The bigdog is running again.
                Beta only release. Added wizard for LCLS images data.
                Some big changes to add energy dispersive support. Need heavy debug.
                Added to example a LCLS CeO2 calibration example. The initial
                analysis file was prepared with the LCLS2 wizard using the config
                file for LCLS available in the supporting files for Maud (extracted
                in the same location as the default.par, marker.txt and xraydata.db
                files are. To be used by the LCLS2 Wizard along with the calibration
                files and directories provided by LCLS Mec beam line.

(07/01/2019):   2.90. The "Boring" release.
                Revised the pole figure log plot, now the true values are
                reported instead of the log values.
                Added the d-space to the info panel displayable from the
                plots.

(07/12/2018):   2.83. The "Sleeping Cat" release.
                Fixing the Lamarkian refinements.

(31/07/2018):   2.82. The "Small Cat" release.
                If you are a dog what would it be your best desire?
                Angle-energy maps on the way.

(15/06/2018):   2.81. The "Beer" release.
                What you think Beer is?.
                Corrected a bug when treating rotation as separated datasets
                in the Hippo Wizard. Other small bugs corrected.

(11/01/2018):   2.80. The "Balilla" release.
                Have you ever heard of the "Calcio Balilla" ?.
                Improved import of esg file format for old files.
                You have a comment string to input in the interface that will be saved
                in the analysis.

(03/11/2017):   2.79. The "Utah" release.
                Ready to leave for ICOTOM.
                Fixed the Hippo wizard.
                Improved the automatic image slicing for tif in arbitrary
                position.

(10/07/2017):   2.78. The "Astix" release.
                We achieved 4E9 photon/sec in the lab.
                Fixed an issue using an interpolated background with intensity
                calibration.
                Implemented the proper asymmetry function for X-ray fluorescence.

(10/07/2017):   2.76. The "kite" release.
                Finally is flying.
                Now if you have the "default.par" file messed up (the one loaded
                by Maud when you choose "New analysis") you can start a new
                analysis, delete all the objects you have (first all phases if
                any, than the sample), the sample and the dataset will be recreated
                as the default one. Then, save the analysis, this new default.par
                will be saved over the one messed up.
                Now,  when you edit the dataset, under general you can specify and
                force a specific dataset to use random texture and no strains.
                This is usefull when you mix datasets measured under stress with
                data measured with no stress applied. Same as for texture.

(04/07/2017):   2.75. The "Fixed" release.
                Let's hope it is.
                On Mac OS by mistake the use of the cctbx library for space group
                calculation was activated, but it is still not fully working.
                I removed the library to avoid the problem and it will be added
                only when ready.
                Now when you create a new dataset, the default instrument will
                correctly define the default "Instrument disalignment" model
                for the angular calibration as the twotheta shift error has
                been removed and substituted by this model.

(26/06/2017):   2.74. The "Bugged" release.
                Renamed as too many annoying errors where introduced.

(21/06/2017):   2.73. The "Vilnius" release.
                Dancing researchers on the streets.
                Export for FPSM, from the separate plot window in the tools
                menu. The exported cif can be loaded as datafile into the
                web version of FPSM providing also the instrument/measurement
                specification that are used instead of the ones on the web
                interface.

(1/01/2017):    2.72. The "Quails" release (unreleased).
                Think about fortheen of them.
                Bug fixing release.

(17/08/2016):   2.71. The "Coco Bongo" release.
                The one in Cancun is not exactly as in the movie "The Mask".
                Speed up of the spherical component calculation in standard
                functions in the case of texture broadening coefficients.
                Corrected the behaviour of the omega offset in the Hippo
                Wizard.

(15/08/2016):   2.70. The "Francisco & Raul" release.
                They made my day.
                Fixed a bug introduced in 2.69 about adding more atoms in the
                same site.
                Fixed the roi selection in ImageJ Maud plugins. Now using any
                kind of roi (rectangular, circular) only the pixels inside
                the roi are used.
                Correction of a bug on the weighting schemes by which the
                default weights were never used and a sqrt of the intensity
                was forced instead. Sqrt or default were the same before,
                now if the datafile define their esd/weight these are used
                if the weighting scheme in compute options are set to
                default.

(11/08/2016):   2.69. The "Cancun" release.
                Preparing for iMRC...
                Corrected a bug when importing cif structures without the
                atom label specified inside a loop. Before in such cases
                the atom scatterer type was not set, now is inferred from
                the site label. Version before the 2.6x had this feature
                implemented.

(26/07/2016):   2.68. The "Notch and Steve" release.
                You don't need more than a week to be brave...
                Corrected few bugs for fluorescence computation for M, N lines
                of heavy elements and photoabsorption on the edges.

(21/07/2016):   2.67.
                Maud.jar has been moved to the lib directory.

(20/07/2016):   2.66. The "Swimming" release.
                I learned you have to start swimming in air first.....
                Corrected a bug resulting in wrong elemental fraction in the
                output results.
                This was affecting principally results coming from XRF. The bug
                was introduced after 2.5 version.
                Improved the fitting for high asymmetry coming from the Warren
                planar defect model. You cannot use the Warren model in
                combination with distributions of cystallites and microstrains.
                This will be targeted in a future release.

(29/06/2016):   2.65. The "Out for breakfast" release.
                Some breakfast may take longer than others.....
                Bug correction release.

(23/06/2016):   2.64. The "Asino" release.
                You are never too old to do mistakes.....
                Speed up release. Found a bug with the new preferences system
                that was slowing down everything, from calculation to plotting.
                Some other bugs connected to the new initialization of Maud
                corrected.

(23/06/2016):   2.61. The "Tangerin" release.
                Rediscovering an old group.....
                This will be released as a public beta finally. The version is
                adressing all installation and first running problems as well
                as transitioning to the new OSes (starting to use the sandboxing
                when required). This means you may encounter some issues with
                the old analysis files like the data is not loaded, but need
                to be reloaded manually. From this version on, the data is
                saved inside the analysis file and is no more an option.
                The XRF computation is now reliable and nearly mature, M and
                N lines also.
                To help and to make more easy to insert impurities atoms
                (fundamental for XRF) the definition of atom sites and
                content has changed. It is no more necessary to duplicate
                an atom site to add a different atom. Each atom site may
                be shared by several different atoms for which only the
                type and partial occupation must be specified. The sum
                of all partial occupations in a site is kept by the program
                normalized to the total occupation of the site that can
                be also specified.
                The Java Virtual machine is now embedded in the program and
                does not need to be separately installed. The installation
                archive is larger for downoad, but the program run more reliably
                with the correct JVM it needs.
                On OS X or future macOS, the program is sandboxed. This means
                it can only load and save files that the user specifically
                select or specify through the dialog box or in a special
                directory inside: ~/Library/Containers/com.radiographema.maud
                You need also to specify the extension when saving. Is no
                more added by default (for datafiles and analysis files).

(2/04/2016):    2.57. The "Montparnasse" release.
                Should say everything.....
                Switched to java 1.7 as a requirement.

(7/26/2015):    2.56. The "Insomnia" release.
                I like when is only the computer not sleeping...
                Corrected a bug for tensor homogenization when using spherical
                components in standard functions.

(4/30/2015):    2.55. The "Chinese VISA" release.
                It is harder to get one than to get rid of all Maud bugs...
                Some bugs corrected and an update to the fluorescence
                background computation when using a Bremsstrahlung.

(4/01/2015):    2.54. The "April fool" release.
                Trust it or not....
                Updated the image importer for GADDS to include new detectors
                with more than 1k x 1k pixels.
                Corrected a bug on the XRF analysis on conversion from atom
                fractions to atom weights.

(2/04/2015):    2.53. The "......" release.
                Not very good today.
                Added the possibility to show peaks information (hkl, phase)
                on the plot. Right-click the mouse and select the last entry
                on Peaks info, then move the mouse over your pattern and see
                which are the more close-by peaks. Works for fluorescence too.

(1/28/2015):    2.52. The "Rigolor" release.
                For the one who like the fresh snow.
                Multithreading related bug on fluorescence computation was
                resolved. Adjusted some plot label position to make them
                visible in certain cases these were not.
                Added the "_maud_output_plot_filename" option to the Maud
                batch system. Adding the keyword in the loop and specifying
                the filename, the full data/fitting is exported in such file
                (fitting including the pattern calculated for each phase).

(1/15/2015):    2.51. The "Panarotta" release.
                For the kids.
                Adjusted and optimized computation for the Ebel tube diffraction.
                Some bugs corrected.

(10/28/2014):   2.50. The "Equinox" release.
                Its working well now.
                Options added to reduce the analysis file on saving.
                The options can be found in the refinement options panel and
                these have also a default value in the Maud preferences as:
                analysis_default.storeStructureFactors (true the advised value)
                analysis_default.storeTextureFactors (true the advised value).
                You can turn them off (false) per analysis in the refinement
                options panel to reduce the analysis file size. It is advisable
                to do it only if you need to send the analysis by email or have
                disk space problem, as doing it and you will loose on saving
                all the structure factors and/or texture factors extracted and
                on reloading you will need to recalculate those factors (this
                was in reality the standard saving mode for Maud 2.33 or
                earlier).

(10/02/2014):   2.48. The "Michele" release.
                Thanks to Michele perseverance, D19 and EWIMV/WIMV fixed.
                This is an important release as finally the small bug affecting
                the texture refinement using EWIMV and WIMV in the beta version
                was found and corrected. Also D19 is working perfectly now
                like D1B always has.
                Fixed also a bug in the computation of the fluorescence line
                intensities for air and windows.

(09/08/2014):   2.47. The "Smart Lab" release.
                Back to Trento, new lab, new instrument, new office, old work.
                Few bugs corrected.

(07/03/2014):   2.46. The "Formation" release.
                Maud formation in Caen.
                Some bugs correction. The absorption for the thin film thickness
                was not working for diffraction in the last couple of version.
                Another bug introduced recently for chi instrument broadening
                and a tan(theta) option in the asymmetry not loaded correctly
                from previous versions.
                Two new planar defects models. One for modulated planar defects
                like the one you find in Kaolinite. An example will be added
                shortly. Second one is for polymers (fibers) where the chain
                ends are always a bit disordered.

(05/20/2014):   2.45. The "ESRF" release.
                Always nice to visit Grenoble.
                Just one bug preventing the crystal structure refinement wizard
                to run completely with atoms sharing the same site.

(05/20/2014):   2.44. The "Monopoly" release.
                Kids growing.
                Several bugs corrected.
                XRF working now especially XRF-XRR combination.
                Added Parrat recursive method to XRR (select in the dataset edit
                panel.
                Modified the electron dynamical correction.

(01/11/2013):   2.43. The "Typhoon" release.
                First visit to Japan. Impressed by J-Parc and I haven't seen
                Spring-8.
                Still in beta but approaching final.
                Now loading and integrating images in Tiff format is more easy.
                You load one through ImageJ inside Maud, do the integration,
                (you must define the instrument and the angular calibration
                to one of the image calibrations before). Then for all the
                others with the same detector/instrument (same calibration)
                just drag and drop the tiff files in one of the panels
                supporting drag and drop of datafiles (in Linux that does
                not support drag and drop directly in java, use the browse
                button or the load datafile menu command as for the normal
                datafiles.
                Improved support for GSAS instrument parameter file.
                Several bug fixes.

(07/08/2013):   2.42. The "Birmania" release.
                A friend I didn't see for long.
                Another step toward the release of version 2.4. Only few
                models missing (mainly for SDPD).
                A major step is the new model for fluorescence, normal
                fluorescence or coupled with reflectivity using the
                De Boer model.
                The multithreading is back, but for the moment, forcing
                the refresh of one of the plot panel (by clicking on the tab)
                during the refinement, should be avoided as it may interfere
                with the computation.
                New pole figure plotting routine.
                Now the model to work with arbitrarily inclined flat images
                in the diffraction space is included and can be used without
                plugins.
                Thanks to the fluorescence database by Giancarlo Pepponi,
                now Maud can compute scattering and absorption for every
                energy and even fine absorption edges.

(25/01/2013):   2.41. The "Back in business" release.
                Finally a real working release after long.
                The previous one was not released due to last minute bug found
                that required a major rewrite of some core routines.
                Only feature added the possibility to load D19 data
                using directly the complete _LAMP file with many images.
                Integration is done automatically based on the preferences
                set for D19.

(18/10/2012):   2.40. The "EPDIC13" release.
                Should I worry about this Tarantola Hotel I booked?
                This is a major release. Took more than a year to fix
                some relevant modifications under the hood.
                There is a new preference (analysis.saveAfterIterations) that
                if set true, will save automatically your analysis at the
                end of every refinement cycle (with the wizard analysis
                having more than one cycle will be saved more times).
                If you want to keep a history of your modifications, set
                to true also analysis.saveAlwaysIncrementalName. This will
                add a "_XXv.par" at the end of every analysis saved with XX
                a number that will increment each time. The increment is
                saved and remembered also after reloading the analysis.
                Be careful with file sizes and occupation on disk. You
                can always delete the old versions.
                The "txt" datafile format now accept also some comments
                lines at the beginning if starting with #.
                The 2theta shift is now affecting the pattern and not the
                peak positions. This is a more correct approach especially
                for "big" 2theta shifts. So if you change the 2theta shift
                you will see the pattern 2theta (or d) coordinates changing
                and not the peak positions that will just be on their
                theoretical positions.
                Many bugs corrected, also one with texture that was appearing
                when starting a refinement soon after loading an analysis
                without doing a previous computation.
                Support for preparing batch analysis from the interface (look
                in the analysis menu); also images can be added and treated in
                batch.
                If you need to work with arbitrarily inclined flat images in
                the space, you need a special plugin that can be released
                on request to: maud@ing.unitn.it
                The integration and calibration in the refinement of all
                the other kind of images treated by Maud are included:
                - flat images normal to the beam (transmission/reflection)
                - curved images over the 2theta circle
                Remember that Maud can integrate and sample them in the eta angle
                (angle along a diffraction ring) as not calibrated (not converted
                in 2theta or d) to permit a subsequent calibration inside
                the Rietveld refinement (normally centering, position and tilting
                of the images can be refined). This procedure produces a better
                calibration than any graphical routine.

(18/08/2011):   2.33. The "RSV mille" release.
                Finally is back.
                Small bugs fixing release.

(29/07/2011):   2.32. The "Flexifoil Atom" release.
                The name is promising for one like me. Hope it will
                stand up the premise.
                The LeBail extraction for structure factors has been modified
                to work better with really sharp textured (near single crystal).
                Pawley method has been moved to the extractor methods. Now you
                can switch from Le Bail to Pawley and structure factors are
                conserved.
                Corrected a bug in superflip, added an option to specify a
                preamble from a file in which you put all customization for
                superflip you need.
                Extended the raw format for Rigaku datafiles.

(20/05/2011):   2.31. The "Erice" release number 2.
                One small bug fixed regarding background peaks when it is
                extended to omega, chi, phi or eta. A modification on
                version 2.30 was not compatible with previous versions,
                now it is, there was a bug.

(17/05/2011):   2.30. The "Erice" release.
                Several modifications, quite a long time from last update.
                Fixed several bugs everywhere.
                Added constraints for bond length and angles under the energy
                computation in the edit structure panel.
                DFT energy computation (another way to add constriants) needs
                the program abinit installed in plugins with the folder HGH
                containing electron maps in hgh format.
                Rearranged the edit panel for the datafiles to make it more
                compact.
                Added the hybrid algorithm for refinement. It merge a genetic
                algorithm with the marqardt lest squares to get the best of
                both world. Tested with full pattern indexing and it improves
                by more than a factor the efficiency.
                Help buttons with no content are now disabled. So you know
                there is some help when the button is enabled.
                Some fixing (reported by Sven, thanks):
                - Plotting pole figures in Log: if the min value is <= 0, the
                program do not try to calculate the Log (stopping working) but
                take 0.01 as the minimum.
                - fixed a problem with refinable parameters in TOF bank
                calibration getting "fixed" when loading the analysis
                - fixed the import of CIF files when the Hall space group
                was specified as first entry
                - fixed the import of CIF structures with B equal to "."
                - DIFC is refined and not ZERO in TOF calibration by the
                wizard procedure
                - there is an option in the main menu (under interface) to
                show or not floating progress window. Unset it to avoid the
                popup of windows showing the computation progress.
                - no more separated output in floating window for entropy like
                refinement (like in EWIMV). Everything goes in the output panel
                in the main window.
                - added a search field in the Parameter list frame to only
                show parameters with a CIF label containing the text in
                the search field. The list is automatically expanded. The
                text is case sensitive. Free and fix on the object will be
                applied to all parameters in the object tree listed. The
                one not listed are not affected.
                

(04/06/2010):   2.26. The "Hedegaard" release.
                Quick fix on the TOF multi-banks calibration objects to avoid
                refining parameters linked to not enabled or not available
                patterns.

(03/06/2010):   2.25. The "Hedegaard" release. Too bad another god one as
                passed away. But it is aready some times.
                Bug fixing release; fixed a bug preventing to refine background
                coefficients specific to patterns. Also added a dialog to warning
                about the use of multiple simultaneous threads in the computation.
                There are still problems with that sometimes. You may try the
                feature if you have multiple cores or processors to speed up
                but the computation may stop sometimes.

(03/05/2010):   2.24. The "Cranswick" release.
                Bug fixing release; on TOF reverted behaviour to have
                automatically ZERO refined instead of DIFC.

(27/04/2010):   2.23. The "Cranswick" release.
                Bug fixing version.

(08/03/2010):   2.22. The "Cranswick" release, will be that for the entire 2.2.
                Fixed others bugs in the isotopes selection in the
                periodic table. Also a "_atom_site_calc_flag" has been added
                to atoms. So now, importing CIF structures where some atom
                sites are marked "dummy", it will set that for the flag and
                these atoms will not be used in the structure factor computation.
                Be careful they will be used for alll the rest (chemical formula
                and annexes like density, absorption etc.). So if you do not want
                these atoms, remove them, but at least by default the structure
                factor computation will be correct.
                Fixed the Pawley method by refining the structure factor instead
                of the square of it. This improve speed up in the least square
                in the early stage. Removed from the interface the Hall and
                Schoenflies conventions. This solve some issues found when
                the Hall space group was also present in the CIF file to import.
                Corrected a bug introduced in version 2.2 about importing
                D atoms.
                Now Maud also recognize "Wat" as O-2 in importing some CIF
                files coming from american mineralogist database and COD.
                Rewrited the sample displacement error treatment: now its effect
                depends on the measurement type: theta-2theta scan or 2theta scan
                only (for TOF or others is also different). For the first the
                error position is based on sin/cos(theta) (for y/z errors), for
                the second (2theta measurement) is based on sin/cos(2theta).
                Omega, chi, phi angles influence the x/y/z displacements that
                are transformed accordingly.
                The manual interpolation of the background has been made more
                robust where before was giving errors in the presence of multi-
                patterns with different range of coordinates in the same dataset.
                The superflip method has been added. But you need to donwload
                the executable for your platform and put it in the plugins
                folder. Select the superflip model in the structure solution
                models and press the info/help button for more informations.

(22/02/2010):   2.21. The "Cranswick" release, hope is high.
                Fixed the isotopes for neutrons.
                Added the Pawley method to refine structure factors. Be careful
                as if you have a lot of peaks and patterns you may end up using
                a lot of memory. Peak intensities are refined automatically
                (only those inside the computing range) based on the option
                in the options panel for the method. The method can be activated
                as a Structure Factor Model. Before to start a refinement is
                better to compute the function once (compute spectra) as Maud
                will check the peaks and set them refinable or not based on their
                position and the option in the method. With 4 Gb of RAM I am on
                the limit with around 400 refinable peaks and 300 pattern with
                9000 data point each. You need a 64bit version for that.
                Fixed the sample precession error for the DEbye-Scherrer geometry. 

(19/02/2010):   2.20. The "Lachlan" release, still hoping.
                New release from long time. There are few improvements (some did
                not get in time for this release, will be in the next).
                There is a raw fluorescence model to work simultaneously on
                fluorescence spectra; it does not implement absortion or matrix
                correction for the moment, so it is only qualitative. It should
                only be used for testing.
                There were a lot of bugs fixed, but I was to lazy not to write
                them down:
                - for TOF banks it refines now difc and not zero by the wizard
                Added the possibility to refine a z or/and y sample displacement
                for each pattern. Use it only for multiple pattern when no other
                correction for positions work (like the z/y displacement or the
                precession error in the sample. You access them with the
                additional pars button after selecting the datafile in the
                datafile list panel (sample displ. tab panel).
                There are now isotopes for neutron diffraction. Select them
                in the chemical element table where you select the oxidation
                state for atoms. If you are refining togheter neutron and
                x-ray patterns you may select for an atom both the oxidation
                and the isotope. The isotope will work for neutron and the
                oxidation state for x-ray. Same for electron scattering.

(14/08/2009):   2.15. The "Let's have a rest" release, time for the family next
                week.
                Bug fixing release: not released.

(17/07/2009):   2.14. The "Let's have a rest" release, time for the family next
                week.
                Bug fixing release:
                - corrected a bug for programmable slits option in geometry
                - a bug was preventing to load back data saved in the analysis
                file in the case of 2D images data, corrected
                - it is possible to read now Bruker gadds images (through
                the imageJ library in image manager)
                - in the Windows maud.bat launching file the memory allocated
                has been step back to 1024Mb respect to 1800Mb as it seems
                on many Windows installations this may cause the launcher
                to fail.

(03/07/2009):   2.13. The "Il n'est plus la Normandie" release, is there any AC
                at all?
                Mainly a bug correction release:
                - revised and corrected the single layer model
                - corrected a bug for loading the INEL fdt file and calibration
                - fixed the experimental background, now is finally working
                - background peaks can be defined also in eta, chi and phi, so
                you can use them for strong peaks from single crystal substrates
                - corrected a bug introduced in the 2.1 release causing an error
                when removing more than one phase at once with the fitting.
                - fixed a bug with the "equal to" parameter bound when applied
                to parameters accepting only positive values.
                - In the editing phase frame menubar there is now a new item
                under File named: Tensor homogenization. You can use it to
                homogenize with different scheme tensor of second and forth
                rank. The ODF is taken by default from the phase, or you can load
                it from a file. No check on tensor symmetries respect to the
                phase symmetry is done, also for the moment the input tensor
                is not saved when you close the frame. Will be in a future
                release.

(13/05/2009):   2.12. The "Spinnaker" release, I got a new job for the weekend.
                Mainly a bug correction release. The single layer model after
                some testing has been consolidated and moved from the Structure
                Factor models to the Planar Defects were it is more appropirate to
                stay. This is not only an aestethic problem, but this way it is
                possible to use one of the other structure factor model in
                combination with the single layer one. For example you can
                model a Montmorillonite using a MEEM electron map and the
                single layer model. Old files using the single layer model will
                not work directly. You need to go to the Microstructure panel
                in the phase edit window and select the "Ufer single layer" model
                for planar defects.
                Corrected a bug regarding extracted/computed structure factors
                outside the computing range, but with the tails inside the range.
                Previously the behavior was not consistent during the iterations
                and on the end.

(20/04/2009):   2.11. Still the "Coyote" release, I want this to stay for a while.
                Mainly a bug correction release. Revised the weighting scheme
                options. More possibilities added especially to weight in Q space.

(17/04/2009):   2.1. The "Coyote" release. To the real one.
                Corrected a bug that prevented to toggle the phases plot on windows.
                Finished the WSODF method of Popa and Balzar for Strain ODF.
                Thanks to Sebastien Merkel (http://merkel.ZoneO.net/) for providing
                his model for strain analysis. Well suited for diamond cell high
                pressure analyses. You should get a look also to his tutorial pages
                on residual strain etc.
                Added the possibility to define/edit manually the background points
                for the interpolated background. From the interpolated background
                options panel select the button to edit manually the points and
                in the appearing plot window select from the menu
                Tools->edit interpolated points manually
                the points will appears. Use the right mouse button on them to remove
                or to add a new one. Only the x coordinate matter for positioning new
                ones. If you toggle the checkbox to view the background in the plot
                options, you can see later where the background really is.
                This new release is named 2.1 for a radical change. There is a new
                way Maud treats the scale factors/phase fractions. The new way improve
                the convergence and robustness of the refinement especially in the
                early stage. In the old behaviour, you usually refine an instrument
                intensity and all phase fractions apart one (that balance to one and
                the program take care of doing it). In the new behaviour, you should
                refine not the instrument intensity, but all the phase fractions
                (this s done automatically by the wizard). So the program take care
                of normalizing the phase fractions to 1 at the end of each iteration
                and to transfer the normalization factor to the instrument intensity.
                So it is more similar to just refinening phase scale factors
                internally (like most Rietveld program do), but you end up
                dealing directly with  volume fraction like you are used in Maud.
                Another advantage is that you can fix the volume phase fraction of
                one or more phase and the program will keep that fraction constant
                (this is something not available usually in Rietveld programs).
                If you deal with multiple datasets, you keep fixed the instrument
                intensity of the first dataset, but refine all the others instrument
                intensities (or you can decide initially to refine all the instrument
                intensities; this will increase the correlation with the phase
                fractions parameters and the esd or error for them; but you can fix
                it later when you are near convergences to get better esd on the
                phase fractions).
                If you want the old behaviour (because you like it or in certain
                circustances could be better) you can force it by changing
                the preference "forcePhaseVolumeFraction.asScaleFactors" to false.
                Last but not least there is a new model to refine random disordered
                turbostratic structures like clays or graphites using the Ufer model
                as implemented in the BGMN Rietveld program (check the web site
                http://www.bgmn.de for more specific info). This is a very clever
                way to deal with this kind of structures. The model is called
                "Single Layer" as Ufer, Kleeberg, Bergman and coworkers named it
                and you select it in the advanced panel when editing a phase, under
                the structure factors models. You need a good structure definition
                for the crstal structure of the basic layer and how many layers
                you want to generate (Ufer et al. indicate 10 as a good number),
                then play with the crystallites sizes (may be you need also to
                use the Popa anisotropic model) to model the line broadening.
                You don't need to define a supercell, Maud will compute it from
                the normal cell and the number of layers you select.


(01/04/2009):   2.075. The "April Fool" release.
                Corrected and improved the switching to monoclinic 1st setting
                (or c setting) when working with texture. This is strictly necessary
                if you are using a texture model like EWIMV/WIMV, standard functions
                or harmonic based. You need to work in this setting to get the
                correct texture. It is sufficient to switch to the corresponding
                space group ending in :c or :c1 whatever and the program will
                switch cell parameters and atoms (also imported Fhkl lists).
                Added the possibility to toggle the visibility of the residuals
                and reflection positions and background in the plot options.
                Clicking with the right mouse button over the phase label or
                peak positions in the plot will show a menu that may enable or
                disable the plot of the fitting for the selected phase. Each
                phase may plot now its own computed pattern in different color.

(16/07/2008):   2.072. The "Out of the tunnel" release.
                New version to correct a bug for ODF from pole figures
                computation in EWIMV (when loading a .apf file).

(16/07/2008):   2.071. The "Chateau d'eau" release.
                New version to correct a bug on 2.07 for D19 2D detector data
                reading. Added a precession error under sample for phi
                rotations, useful for Debye-Scherrer geometries like D19.

(08/07/2008):   2.07. The "Evil release" release. As Mauro wanted to define it.
                New version to correct a bug on 2.066 causing problems to the
                least squares routine. Found a trick to reduce Choleski negative
                diagonal problems.

(08/07/2008):   2.066. The "Eleanor Rigby" release.
                Rediscovering this relatively old song.
                Finally Maud is working properly again for texture and
                structure factor extraction. Now it is possible to
                have datasets avoiding texture. Each datafile may have its own
                2theta displacement. D19 neutron images natively supported
                (data ending in "_LAMP" to be loaded directly by the
                "Browse..." command).
                Electron diffraction supported for the kinematical case,
                no absorption correction for the moment (someone has an
                electronic table?). So TEM images may be refined also
                for the structure factors (atomic parameters)
                if obtained in kinematical regime.
                Other bugs corrected, something regarding loading datafiles
                and getting the getXData exception..... Also corrected a bug
                where for multiple textured phases the last one should be enabled
                to get the extraction working.
                Fixed some problems with COD submission.

(11/06/2008):   2.065. The "Leaving for Caen" release.
                Not released to public.
                Rewrote completely the way structure
                factors and texture factors are stored and managed.
                Still there is a stopping bug on the simultaneous extraction
                of both and for datafiles of different length.

(02/04/2008):   2.064. The "After the thunderstorm" release.
                Bug fixing release. Some of the methods employing external
                libraries are moving into plugins. Progressively, everything
                except the core classes will be moved to plugins. Added a
                64 bit release for MacOSX, requires an intel mac at least
                core2duo. Do not use it if you don't need more
                than 2Gb of memory.

(24/12/2007):   2.063. The "Fighting" release.
                Internal only release. Some bug fixed. There is one
                in the previous version causing the program to loose
                phase quantities on reloading an analysis file.
                Added the superflip as a plugin, need the superflip program
                in the plugins folder.

(02/11/2007):   2.062. The "Zurich" release.
                Sorting out some Le Bail fitting bugs on the train from Zurich.
                Principal modification was the addition of the swingx.jar
                library missed in previous release for the Mac and unix/linux
                distributions. This caused the wizard not to work.
                The Le Bail extraction has been modified to work also when
                the fitted background is negative. This may happen with
                background subtracted pattern. Better not to subtract the
                background in general.

(02/10/2007):   2.061. The "tomorrow" release.
                Ok, finally tomorrow has arrived, sorry to all for the delay
                of this release.
                There are several bug fixes and actually I was too busy to
                remember what was introduced in this release.

(14/07/2007):   2.06. The "too late" release.
                Well this is me.
                Fixed a lot of bugs introduced in the previous version. From
                image integration, to output, absorption, angle corrections,
                etc.
                The TIFF image is now again recognized as a datafile.
                Integration in one spectrum is done automatically supposing
                it comes from a curved image plate in reflection (like the
                IPD3000). It uses the input from the last manual integration.
                Now Maud is compiled UTF-8 and requires Java 1.5 minimum.

(06/06/2007):   2.059. The Normandie release.
                If you don't see the Le Havre hill it's bad weather; if you see
                it, the bad weather is arriving soon.
                Internal release only. Added the new ODF plotting routines from
                Daniel Chateigner. Maud now can process also flat images out of
                the normal diffraction plane. It reads also .kcd and .syn from
                Nonius kappa goniometer, D19 images and .sis from Hypernex.
                Revised and rewritten completely the absorption correction
                for multilayers. It now support fully eta angles and all bugs
                corrected. The Le Bail extraction for structure factors was not
                working well with the interpolated background. The problem
                has been fixed.

(20/02/2007):   2.058. The walk release.
                All these babies walking around.
                The Hippo wizard now works also with an arbitrary number of
                detector panels. It may work also for other multi banks TOF
                instruments (GEM?, IPNS?). Added option to dataset so new datafiles
                added replace or not existing datafiles. Usefull for batch
                processing as you may want to start from the previous analysis
                file and replace the previous datafile with a new one. The
                CIF keyword is "_riet_meas_datafile_replace" and if set true
                datafiles are not added but replace the old one. The default
                is false and is set in the Maud preferences.

(15/12/2006):   2.057. The Van Houtte release (internal only).
                The exponential form of the harmonic is simply a brilliant idea.
                Added the Van Houtte exponential form of the harmonic method
                for texture. This ensures the positivity of the ODF. The same
                has been implemented also in bgmn.

(13/12/2006):   2.056. The Hippo final release 6.
                Back from the workshop. Ending up the story.
                Bug fixing release. Finally one good enough. The 2D plot has been
                changed.

(10/12/2006):   2.055. The Hippo final release 5.
                It's annoying.
                Bug fixing release.

(08/12/2006):   2.054. The Hippo final release 4.
                Again here.
                Bug fixing release. Not available on the web.

(04/12/2006):   2.053. The Hippo final release 3.
                Nor the second.
                Speeded up the computation using GSAS instrument profile function.

(27/11/2006):   2.052. The Hippo final release 2.
                The first wasn't.
                A couple of more bugs corrected. Mainly concerning parallel computation
                and the wizard for refinement.

(22/11/2006):   2.051. The Hippo final release.
                Hopefully this is the right one.
                Corrected a couple of nasty bugs concerning atom symbol
                when loading from certain CIF databases or files. In particular
                for CIF files coming from the COD where the atom symbol
                was missing.
                Also one in the Hippo wizard setting too much constraints.

(20/11/2006):   2.050. The Hippo release.
                Just in time for the workshop?
                The Hippo wizard has been refined and fully integrated in
                Maud, from preferences to settings. Now it may be possible
                to do a texture analysis by the Hippo wizard specifying
                the GSAS instrument parameter file, the datafiles in GSAS
                format and choosing the appropriate range. Then the phase/s
                must be loaded and the texture model selected.
                Then the refinement wizard -> texture analysis will do the
                rest. Let the program run for few hours unless it is a cubic
                samle and/or you have a quick computer.

(09/11/2006):   2.049. The Shaolin release.
                I look like a monk?
                The Hippo wizard has been included in Maud. Made by Mauro
                Bortolotti. From the principal menu, File->new analysis->
                Hippo Wizard. It needs the GSAS instrument parameter file
                and the datafiles (GSAS format also).

(22/10/2006):   2.048. The China release.
                Wonderful China visit.
                Finished with the GSAS profile function. In reality it is
                just the same as the PV function covolutted with the
                asymmetry as it was before in Maud. At least it can load
                the instrumental function from GSAS parameter file.

(13/10/2006):   2.0475. The "poor harvest" release.
                Not too much vine this year.
                First implementation of pluggable instrumental broadening.
                Added GSAS profile functions.

(15/08/2006):   2.047. The flowers release.
                The magnoglia got the flowers at end.
                Fixed the first running/create directory databases stuff.
                Fixed the inverse pole figure plotting for lower symmetries
                (monoclinic/triclinic/trigonal).

(21/07/2006):   2.046. The Wake/up release.
                It is like to wake up from a looong slep.
                Neural Network added for indexing to Maud after testing apart.
                Improved multitasking capabilities with multiprocessors
                computers, use the Preference parallel_processing.threads_maxNumber>1 to use more
                processors.

(07/05/2006):   2.045. The Waiting release.
                No news from the twin.
                Maintenance release. Corrected some bugs plus one making hard
                to choose the starting help and database folder on first run.

(12/03/2006):   2.044. The Neural release.
                Neural Network is the future for several pre-analyses.
                Some improvements, working on XGrid. Improved the Le Bail
                structure factor extraction for multiphase analysis. New Simplex
                algorithm added to the available algorithms. First release of
                the associate dev-kit to develop Maud extensions/plugins/new
                models.

(02/02/2006):   2.043. The Twin release.
                Twin is not the engine of my motorcycle......
                Corrected a bug for multidatafile loading.
                Finished the XGrid support. Task can be launched from PC also.
                Only a Mac for the controller is needed.

(02/01/2006):   2.042. The "RSV" release. Hope it will go again.
                Improved release. Added support for XGrid. The evolutionary
                algorithm can spread the population over an XGrid aggregate
                network.
                Bug fixed: UXD only intensity datafiles were not imported due
                to a bug inside, corrected.

(24/10/2005):   2.036. The "Merlot" release. Harvest finished, wine in the
                barrel.
                Bug fixing release. 2D transmission images integration fixed.
                Edit button in the main Maud window moved to the left.
                From the peak list is now possible to export an input file for
                Dicvol91 indexing.

(15/10/2005):   2.035. The "School" release. First Maud school in Riva del Garda.
                Bug fixing release. Input/output fixing. Also some plots output.

(05/09/2005):   2.03. The "CE" release. Just a "production" release ;-).
                If you want to know what CE stand for look on a manufacturing
                book.
                Bug fixing release. The system to change "live" the parameter
                value and see the immediate result on the plot a been updated.
                The object/parameters list is now at the full bottom of the
                main window. There is a preference of Maud to get it back
                just below the plot part. Not advised as the parameter fields
                will clutter too much.

(05/09/2005):   2.02. The "Firenze" release. For the IUcr congress.
                This will be the first officially released version 2.
                Bug fixing release.
                Added the MetaDynamicSearch algorithm. Based on the method
                Meta-Dynamics to explore polymorph transitions; see
                M. Iannuzzi, A. Laio and M. Parrinello, Phys. Rev. Lett.
                90, 238302 (2003). To be refined.

(22/07/2005):   2.01. The "Moving chicken" release. Running away for their life.
                This could be the first officially released version 2. Fixed a
                lot of stuff and improved compatibility with the old version.
                Still a lot of features to be completed, but at least it work
                better than previous release. The major features to fix/complete
                are:
                - printing
                - absorption (new version use a different computation for layers,
                  old parameter files will continue to use the old model for
                  compatibility; new parameter files will use a different one,
                  total intensities are not comparable)
                - indexing
                - energy computation
                - ODF plotting
                - importing non standard CIF files (some CIF files from COD do
                  not contain atom labels (only site labels). Maud requires
                  atom labels to be presents at the moment. Result is that
                  the atom types are not recognized and should be set by hand
                  in the edit phase panel.
                - COD database connection for retrieving CIF directly and
                  local mySQL database connection.

(18/11/2004):   2.0. The "Ocean 11" release. Nothing fancy, just finished
                the first working new release looking at the movie.
                Internal release only for debugging purposes.
                The new release has a completely new interface. The inner
                structure also has changed and now the root object is
                an analysis containing a sample, then the sample contains
                the phases and datasets, each datasets the instrument and
                the datafiles.
                Principal goal of the new interface is to improve usability
                of the program, simplify it and make it more intuitive
                for first time user.
                Big addition is the drag and drop possibility for datafiles
                and objects.
                Dragging datafiles from your system to the plot panel will
                add them to the selected dataset.
                Dragging CIF files to the phases list panel will add them
                automatically (if containing only one phase) or a list
                box will appear asking which ones you want to add.
                Dragging of multiple files is supported.
                This is only the first 2.0 version. Mostly an hybrid version.
                By time more part of the program will be retouched and
                improved, with the idea of masking all the features not
                used frequently, but with the possibility for the expert
                user to set and use them.
                This version can load the old analysis files, but write
                analysis files that cannot be fully readed by previous
                versions.
                If you find a bug or something not working, just submit
                it throughout the new Maud bugreporting system at the link
                from the principal Maud web site.
                You need to register with a valid e-mail that will be
                used to notify the status of your bug; then you enter
                with the password that will be sent to you (and that
                you can change) and choose: submit a bug.

(13/10/2004):   1.9993. The "Recover" release. Finally I seem to be in shape
                again.
                The harmonic object can load and convert GSAS harmonic
                coefficients directly from GSAS .exp files.
                Fitting of the harmonic coefficients on loaded pole figures
                from file (Beartex, Maud and Popla format).
                The oval multispectra import plugin from images now has more
                features expecially for the curved Image Plate.
                Revised the absorption computation for Image Plate out of
                scattering plane condition.

(31/7/2004):    1.9992. The "Accident" release.
                Internal release only. Features added:
                - Texture Standard Function (Fiber and spherical components,
                same as in Beartex). The component parameters can be optimized
                in the fitting like other parameters.
                - Crystallites and microstrains distribution fitting for LB.
                The profile function is computed by Fourier Transform from
                coefficients.
                - New fragment model
                - New library (JOGL) for OpenGL plotting. Revised all OpenGL
                plots
                - ODF 2D and 3D plotting (still in progress).
                Many bugs fixed.

(31/12/2003):   1.9991. The "Home" release.
                Never enjoyed home so much.
                Fixed a problem with the experimental texture factor saved in
                the "phase_name.apf" file. The problem was present in the case
                one or more dataset/datafiles were disabled.

(03/12/2003):   1.999. The "Felas" release.
                For Gianmarco.
                Added Cobalt radiation.
                Added output for simple tracking of some parameters. Use the
                menu item "Append results to..." or the simple results
                version. The two command save on a single line selected
                parameters and outputs separed by tabs for import in
                programs like Excel. If you save several different
                (but similar) analyses (every time you select the same file
                for the output there is a warning about overwriting, but it is
                wrong, in reality the data will be appended to the data
                already present in the file) then you can import in Excel
                the file and plot the trend for the same parameters.
                First time with a new file a first row containing the labels
                is saved. The "non simple" results are customizable. Use
                the "parameter list" window to specify the parameters in
                output. Also object can be selected.
                Improved the Warren faulting model.
                For Mac OS X (tested on Panther, Java 1.4.1) now it is
                possible to double click an "analysis" file (ending in
                .par) to open it (even if the program is not running) in
                Maud.

(15/10/2003):   1.998. The "RSV04" release.
                New life under 6000 rpm.
                Bug fixing release. Corrected some bugs plus one in the
                WIMV texture model introduced in the 1.991 release.
                Added Sietronic datafile format (.cpi).

(03/09/2003):   1.997. The "Barcellona" release.
                Again here for the usual course. Quite good to be in a Latin 
                country again.
                Bug fixing release. I discover that the free version was never
                updated by my script. Only the full version. So this will be 
                the first version from long.....
                Added computation of X-ray absortion for any wavelength.
                Started adding more threading capability to the program for
                multiprocessor machines.

(11/08/2003):   1.996. The "Leaving..." release.
                Bye Bye California.... for now.
                Revised some of the Lorentz polarization factors (for better
                compatibility with the 2D detectors in transmission).
                Added a plugin to the Image manager (ImageJ) in order to
                transform transmission images to spectra (integration
                around Laue circles, continuous or in slices).
                The new "Flat Image Transmission" angular calibration
                can calibrate and refine the center and off axis tilt of
                2D images coming from CCDs or Image Plates. Based on the 
                paper: Piltz et al., Rev. Sci. Instrum. 63, 700, 1992.
                Started the implementation and testing of the WSODF
                of Popa and Balzar.....

(23/07/2003):   1.995. The "Enzo" release.
                He got a nasty bug.....
                Only some bug fixing.

(20/07/2003):   1.994. The "Small release" release.
                A quick version to correct a bug slowing down texture 
                computation.....
 
(16/07/2003):   1.993. The "Stolen victory" release.
                You should have followed the MotoGp this year.....
                Corrected a bug in the crystallite size and microstrain with
                Pseudo-Voigt function, the crystallite size is now double
                the size it was before and the microstrain half of it. Maud
                will load old analysis files and change them to the 
                correct value.
                Plotting pole figures is now by default with the new model,
                using the edit menu you can edit each pole figure individually
                to plot or not contours, labels etc. To get the old way
                of plotting you have to change in the Maud Preferences the
                "plotPF.classicalPlot" to true (false is the default).
                Maud and the new Beartex (experimental version in the 
                downloading page) can now fully exchange ODFs back and
                forth. Only one format is used now for exporting and 
                exchanging. If in Maud the resolution is different from 5 deg
                than it will be converted to 5 deg.
                Corrected a bug for the experimental pole figure plotting
                where the x and y axes were exchanged inn the previous 
                versions.
                Corrected a nasty bug preventing the Java Web Start version
                to install correctly with the new jdk version (1.4.1 and 
                1.4.2) under Windows.

(03/07/2003):   1.992. The "Captain Stephan" release.
                Good sailing in the bay tomorrow.....
                Added the TDS model of Warren based on the elaboration
                of Stephan G. Look under Phases, edit one and check the
                Advanced models panel.

(13/06/2003):   1.991. The "Maui" release.
                Leaving for one week vacation.....
                Works on the pole figure plotting. Now the experimental pole
                figure plotting is working. Sinking the program with Beartex
                for better exchange of the ODF between the two programs.
                Remember that from a while also the inverse pole figure
                is working.

(11/05/2003):   1.99. The "Tamalpais" release.
                Beautiful view of the bay area.....
                Fixed a bug (introduced in 1.98) preventing reflectivity to
                work. Added a reflectivity example in the files list (you
                will not see it unless you install the program from scratch)
                The example is call g11maud.par and it is a SiO2 film over
                a Si wafer. EWIMV is now using weights proportional to the 
                sqrt of the peak intensity to compute the ODF. So it is
                possible to use all peaks for the analysis, as the samll
                ones will contribute proportionally less than the strong
                ones. Something missing from WIMV from years.....
                So now you can set the minimum intensity to zero.

(07/05/2003):   1.98. The "Big Dog" release.
                In Los Alamos they know what it is. I can only say it is very
                noisy......
                Finally I am starting to release the new program. And we
                are approaching the new 2.0. Fixed the COD submission to
                have a more CIF readable file to send and also a confirmation
                window were the data can be edited before it's sent.
                Maud produce now a log file for each analysis that can be
                loaded by the Refinement->results menu item. It is not too
                easy to read, but it will contain time by time all what you
                wish to know from the program ;-)

(23/04/2003):   1.97. The "Kirkwood" release.
                Too bad Kirkwood is closing. But was fun!
                Added several new things and the transition to the new
                structure is nearly completed. New Phase edit window
                with OpenGl plotting of the structure. Actually very
                slow if openGl is not hardware accelerated. Need the 
                GL4Java native libraries for that. The Genetic algorithm
                can be selected for minimization in the options, but need
                a careful setup of min and max values for each parameter.
                Fixed a problem with the Caglioti formula in the TOF case
                it was not using really d-spacing (thanks to Stephan G.)
                but a strange bugged result. Actually for old versions
                will still use the same. If you want to go to the real
                d trend, deselect the "force Tan(theta)" button in the 
                Caglioti formula panel under instrument.
                Consistent speed up of the program by optimizing object
                and computation refresh.

(xx/03/2003):   1.96. The "Hole" release.
                The only hole is in my mind, I forgot what this release was
                for.

(25/03/2003):   1.95. The "Armel" release.
                I want to dedicate this to A. Le Bail for all the work
                is doing, I hope it will be a very good one like he is.
                This one is released to public. It is also a final
                release for the ESQUI project and it could be considered
                a version 2. But I would like to see how it works for
                a while.

(15/03/2003):   1.94. The "Trees release" release.
                Why trees are moving when I surf?
                The transition is nearly completed. From this version old
                Maud programs reading the new analysis file will loose the
                atoms probably (actually not sure at all). This is the last
                version not going public. The next one will be probably
                sufficiently good to go public. Seems everything is working
                again ok or better.

(01/01/2003):   1.93. The "Transition" release.
                Starting migration to version 2.0.....
                Most of the program is going to have a revision.
                Introduced the JNIAltivec for native computation support
                to speed up certain routines (for the Mac OS X and the G4
                it will use the vector processor).
                Revised the Phase and Structure. Now the atomic structure
                becomes a child of Phase. This permits the introduction of
                fragment etc. and it is going to have an energy computation
                counterpart for structure solution. Mauro Bortolotti is
                working on that.
                The phase editing panel has changed completely.
                The sample shape absorption model has been revised and
                corrected. It works only for Debye-Scherrer geometries and
                subclasses of it (IPNS/LANSCE TOF, LANSCE/GSAS TOF,
                Transmission Flat Image, Dubna/SKAT TOF, Generic TOF). For
                neutron the velocity absorption dependency can be included
                but this requires to change/refine completely the incident
                intensity function for LANSCE/IPNS TOF neutron.
                The Transmission Flat Image is a new geometry class under
                progress intended to manage spectra from flat 2D detectors in
                transmission.
                Now the printing should work everywhere. For all the graphics
                windows (only Java 1.4 or better) now it is possible to use
                the copy function from the menu and paste the graph or image
                in your preferred program. For Mac works beautifully, if it
                does not for Windows complain to Sun/Microsoft.
                The program can upload/download a structure from the COD
                (Crystallographic Open Database) network database (requires
                internet connection).
                It is possible to choose (see options panel) which function
                to minimize (Weighted Sum of Squares or Rwp). The second
                could be useful when dealing with different datasets of which
                one has very big intensities but is not not so reliable due
                to other reasons (high background, poor resolution etc.). So
                the minimization routine weigth the different spectra on the
                Rwp base.
                Corrected a bug for the IPNS/LANSCE incident intensity function
                (where you load the GSAS instrument parameter file) for the
                case with multiple banks having at the same time different
                function type (before only one was effectively used for all).
                It is advised for both angular and incident intensity
                calibration to use the multibank variant model and if it
                is a old analysis, to reload the GSAS instrument parameter
                file for both the angular and incident intensity calibration.
                The parameter list window now includes the button to free/fix
                /bound special parameters at once. They will be removed from
                the overcrowded wizard panel in the future. The "bound To"
                Frame (from a previous version) now stay open unless you
                manually close it, and remember the last parameter choosed,
                so it is more quick now to bound several parameters by just
                selecting "bound to" and in the "bound to" frame select the
                reference one and press "Bound". It is advised to use the
                Parameter list window for that.
                Lot of other changes I forgot.....

(13/12/2002):   1.92. The "Normandy Village" release.
                Well, is like to live in a fairly tales house..... 
                EWIMV can normalize pole figures now to be used not only
                inside the Rietveld. 
                Started the process for speed optimization.
                Some bugs worked out.

(22/10/2002):   1.91. The "Ant" release.
                Ant is not exactly an insect..... 
                Corrected a nasty bug in the cell volume computation affecting
                some space groups with angles far from 90 deg. The bug 
                affect only quantitative analysis.
                Added support for multispectra in the Bruker/Siemens datafile
                format (*.UXD).

(11/10/2002):   1.90. The "BulkPathGEO" release.
                Siegfried is here..... 
                Finally the BulkPathGEO residual Stress model is working
                (only full release). Several bug fixes and small improvement.
                Now in nearly every list box you have multiple non contiguous
                selection support. For example you can select some datafiles
                in the datafiles list and every action you do will be
                performed for all the selected (for example the view plot).
                The list box when importing from the database, now list
                the object sorted by their name, multiple selection is also
                possible there. Also for browsing datafiles to load the 
                file dialog box support multiple selection.
                Still some problem when changing some option in the plot
                window, if the window is not displaying correctly, just
                close it and reopen it.
                The "Live parameters" box now works also for the multiplot
                window.
                The program now recognize and don't try refine a refinable 
                parameter associated to an object that it is not active or 
                enabled, so you don't need to fix it.
                A full set of residual stress model has been added under the
                name of "Moment Pole Residual Stress". You can choose in
                there Voigt, Reuss, Hill, PathGEO or BulkPathGEO.
                Added a precession error for the sample position, it works
                with the IPNS/LANSCE geometry and it was developed for the 
                new Hippo spectrometer. In that geometry also the x, y and z
                shift also work. Also for the Bragg-Brentano geometry these
                shifts are working (x shift has no effect for the moment).
                The Web Start version now try to install the native 
                GL4Java library to use the OpenGL acceleration for some 3D
                plottings. The program try to download the last version
                available from the original JauSoft site, except for the
                MacOS X that is mirrored on my site.
                I added the Matrix reflectivity model to the public version
                of Maud. So you can try to use the reflectivity. Remember
                to select a log model for the statistics (from refinement->
                options) otherwise the fitting will never be sufficiently
                good.
                Added the Rwpb and Rpb (Rwp and Rp factors with the background
                subtracted). Also singles R factors are displayed in the
                result window for each datafile when working with multiple
                spectra.
                Added the multiple spectra generator in the plug-ins of the
                image manager. On the Trento IPDiffractometer diffraction
                images it permits to obtain spectra at different eta angle.
                The eta angle is the angle along the diffraction cone for 
                a single diffraction plane. In the classical Laue image
                from powder in transmission the eta angle goes around
                one of the diffraction cones. So its axis coincides with 
                the incident beam, the positive rotation is for a right
                handed rotation towards the source. The zero is in the 
                positive 2theta plane.
                The "Equal To" window when bounding parameters or objects
                in the parameter list window, now remains open, so it is
                more quick to bound several similar parameter, you don't
                have to open the window and scroll down to the correct
                position every time.
                Fixed the printing on the graphic windows. You can now print
                all the plot. Only the native 3D OpenGL plots cannot be printed.
                You have to capture the screen or window to print it.

(06-09/2002):   1.88-1.89. The "Corean" release.
                Never go out to eat without a corean..... 
                Bug fixing releases. 

(18/06/2002):   1.87. The "via delle Bocchette" release.
                Too much late snow this year..... 
                Maud run now as a WebStart application.
                Added support for Hippo. 

(09/05/2002):   1.86. The "SMARTS" release.
                Los Alamos. These neutrons are faster than we think :-)
                Bug fixed release. The IPNS/LANSCE calibration intensity was 
                never using the last 2 coeffs in type 1 and 2 function. This 
                has been fixed. Corrected a bug causing the Rombohedral cell
                setting (:R in the space group) to work not properly.
                Fixed bug for D1B datafiles.
                Fixed the printing of graphic windows. For the moment only one
                page is printed.
                Fixed the refreshing problem for some parameters from the
                live update panel. 

(15/03/2002):   1.85. The "Office and view" release.
                Changed office......
                Bug fixed release. The IPNS/LANSCE calibration options panel
                has been fixed. It was causing a corruption of the calibration
                coefficients. Just reload the GSAS parameter file to restore
                the correct parameters if they result screwed up.
                Fixed an annoying bug preventing a good refinement of a 
                angular calibration function when a restricted range of data
                was active.
                There is a new preference to set up the maximum miller index
                usable by the program. Before it was not set, but this was
                generating incredible long lists (and the program running
                forever) of peaks with some TOF spectra with a very small
                minimum d-space.
                Other smaller bugs corrected...
                Revised the EsquiClient/Server part inside the ESQUI project.

(13/02/2002):   1.84. The "Banana republic" release.
                Just looking to the italian politics......
                Changing the printing system to Java 2. Revised the interpolated
                background method. Added an angular calibration for 
                Debye-Scherrer camera to refine radius etc. (for Imaging Plate
                Diffractometer).
                Added a Synth for fun to be used when tired at work. Need a 
                sound output, better if you can connect the computer to your 
                home stereo. Actually it is based on my US keybord of the Apple
                powerbook. On request I can work to add the possibility to use
                other keyboards or define a custom one.

(15/01/2002):   1.83. The "Chomolungma" release.
                Still waiting for the snow and reading......
                This should be a version that will go on line.
                Adapted the Siemens datafile format .uxd to the new one (version
                2).

(26/12/2001):   1.82. The "S.Stefano" release.
                No rest this year......
                Bug fixing release. New installation and build, simplifying
                the program structure for future update and different releases.
                Some frame rearranged for better consistency.

(09/12/2001):   1.81. The "Oetzi" release.
                Fitting the Oetzi axe data......
                Revised the sample shape harmonic model to fit the absorption
                correction for Debye-Scherrer geometries.
                Modified the texture/strain pole figure plot; also the 
                absorption correction can be plotted now if a sample shape model
                is active.
                The experimental pole figures plotting still not finished.
                Strange background: if the background is very bad (and changing
                with tilting) there is a new choice, check the box 
                "background interpolated" in the panel for background parameters
                under the "Datafile Set" edit frame, and remove all the other
                background parameters and backgrounds peaks, leaving only one
                parameter in the polinomial background. No extra parameters
                also associated to single diffraction datafiles. Starting with 0
                value for the only background parameter, the background will
                magically fitted. Not to be used for amorphous fitting.

(22/11/2001):   1.80. The "Haydin" release.
                Added MEM Electron Map computation.
                Some bugs introduced in the last version has been corrected.

(20/10/2001):   1.79. The "Running for good" release.
                Added genetic algorithm support to reflectivity (in Le Mans).
                Added .fdt datafile format for INEL detector.

(21/09/2001):   1.78. The "5G64" release.
                Added full support for sdpd (from peak location, exporting
                file for dicvol91...). In the view plot window look for the
                "tools" menu.
                Corrected a nasty bug introduced in latest build affecting
                convergencies of iteration when texture or structure factor
                extraction/recalculation was active.

(01/09/2001):   1.77. The "sdpd" release.
                Discovering SDPD could be also something fun to do...
                Added support for ab initio structure solution (only in test
                release, not available to public). Two method implemented:
                - reverse Montecarlo (similar to Espoir of A. Le Bail)
                - Genetic algorithm
                It works also in iterations with refinement.
                Plot windows remember their position and size...

(31/07/2001):   1.76. The "Guests" release.
                This one is good enough to update the web version. It could be
                the last free version.....
                The new release is a bug fixing release. Discovered a bug
                on loading the .f1b files from D1B at ILL. Improved the
                2D pole figure plots with a new color model. ODF (WIMV) and
                extracted structure factors are saved inside the analysis 
                file from now on and not separately. The old version is 
                in any case supported.

(02/07/2001):   1.75. The "Mouse" release.
                Casalino, looking for a mouse somewhere eating my kiwis...
                Completed the Structure Factor model and extractor. It is
                now possible to extract indipendently Structure Factors and
                Texture Factors, provided that for at list one of the two
                a model to recalculate them is used and not arbitrary models
                for both. So now extracting Structure Factors from textured
                sample becomes a "trivial" task.
                Incorporated the SGT graphic package from D. W. Denbo at
                www.epic.noaa.gov/java/sgt for 2D rendering of multi-datafiles
                (only for Java 2.0 or later, so not for MacOS <= 9.x).
                
(20/06/2001):   1.74. The "Gaudi" release.
                Barcellona. Finally the E-WIMV is working for all cases.
                Starting production release.
                
(01/05/2001):   1.73. The "No work team" release.
                E-WIMV texture method released. It is essentially the old
                entropy fixed and cleaned. This method got a speed-up by
                implementing the sample symmetries in the ODF instead
                of generating more experimental points. The ODF is saved 
                as a full ODF in any case.
                Layered model revisited. It works with the Bragg-Brentano
                geometry. Refining layer thickness, the convergences requires
                a lot of iterations. Better to start from best know values
                and keep B-factors fixed on some good estimates.
                
(01/04/2001):   1.72. The "April Fool" release.
                Some hold releases were not saving the parameter bounds
                correctly (see also notes in the 1.70 release). It was not 
                possible to track down the bugged release, so from this 
                release loading a parameter file from an old release (< 1.70) 
                will not load the bounds. If you were having some bounds in 
                it you have to restore them in the program. The LANSCE-GSAS
                geometry is now exactly the same as the IPNS-LANSCE apart
                that the pole figures angles phi is 360 - phi respect to
                the IPNS-LANSCE. This way the texture angles in output
                (ODF, pole figures) use the same convention as GSAS.
                GL4Java (openGl rendering) updated to version 2.6.0.0 (see
                www.jausoft.com).
                Added preference to plot the x-axis as d-space or default.
                
(04/03/2001):   1.71. The late snow release (only tested internally).
                Well, it's still snowing here......
                Fixed the entropy method for texture; now it's working good.
                Actually it need a lot of memory to ensure coverage. The
                "Use all hkl" switch increase the coverage when it is on,
                but slow down computation a lot and requires a lot of memory
                (256 Mb or more). Actually computing with that switch off
                is not recommended, because if the coverage don't result
                100%, than you may have a not correct normalization of the
                ODF. The tube projection can be used, but for the cubic
                symmetry the symmetrization of the ODF cannot be enforced
                and may slightly deviate when tube projection is on.
                Added a Preferences... item to the menu to edit inside Maud
                the preferences (what it is saved in the preferences.Maud
                file) for the program. Some of them require an exit and
                restart of the program to take effect. Some also are 
                changed automatically inside the program and there are no
                reasons to change them manually. More informations will go
                in the manual. Some testing models not finished and so not
                working has been masked in the program to avoid selection
                of them. They will be available when completed and tested.
                Fixed some bugs introduced in the "Hippo release".
                Fixed a bug in the triangular interpolation that was
                removing the more external circle in certain cases or not
                interpolating in the center when no point was in there.
                
(11/02/2001):   1.70. The Hippo release (prepared for the Hippo workshop). 
                Corrected a nusty bug standing from the beginning that was
                messing up in certain cases (with major new Maud releases)
                the parameter binding when reading from old version files.
                The present version should be ok for reading all previous
                analysis files.
                Added the microabsorption model structure and the Brindley
                and Vien models. Any phase may have his own model. The
                volume and weight phase fractions in layer/sample must be
                considered has the real phase fractions and not the
                apparent phase fraction due to the present of micro-
                absorption.
                Changed and simplified the overall program structure. Now
                also a Phase or Instrument can be subclassed.
                Added some others buttons to the toolbar for frequent
                commands.
                Revised the texture plot panel and others panels for
                consistency.
                Only for ESQUI: added the EsquiClient by Leonardo to 
                launch special measurements on the ESQUI diffractometer.
                
(26/01/2001):   1.69. Not released (used only internally, due to bugs). 
                Added the ESQUIGo data formats for the ESQUI instrument.
                Added absorption correction for particular sample shape
                in Debye-Scherrer geometries and similar. Under the Sample
                edit panel there is an harmonic model to describe a sample
                shape based on some symmetries (size in mm).
                Pure Java version of OpenGl included. It's slower, but the
                program will use it only if the gl4Java shared library is
                not found in the path, otherwise the hardware accelerated
                OpenGl is used.
                Corrected a bug in input of Philips UDF files.
                Added a toolbar to the refinement output window with buttons
                to stop/resume computation/iterations or change the number
                of iterations on the fly.
                Added by Leonardo Cont a routine to automaticly arrange in 
                alphabetical order the phases in the list box for importing
                from database.
                New data format for ESQUI machine has been added.
                Added an agular polynomial calibration to calibrate angular
                data with a simple polynomial function.
                Improved support for refining calibration (both intensity
                and angular/d-spacing) of TOF machines using the standard 
                already implemented GSAS/LANSCE/IPNS calibration.
                
(31/12/2000):   1.68. The Zurich/Christmas release. 
                Added the ETH data format (3 columns: 2Theta Intensity 
                Standard_deviation). Positions of peaks in the plotting are
                now corrected for aberrations and strains. Added help in the
                DataFile dialog about the different datafile formats 
                recognized by the program.
                WIMV works now for different resolutions than 5.0 degrees;
                it is adviced to use a multiple of 5.0 or 5.0/n where n is
                an integer. From the WIMV texture panel is is possible to
                load some standard pole figures (in Beartex format) and perform
                an ODF computation. If the resolution is different from 5
                it is assumed that the pole intensity is in any case in rows
                of 18 values like for 5. The cell parameters, laue group etc.
                are retrieved from the phase parameters and not from the pole
                figures file. At the end of the computation, three files are 
                saved with the name of the phase and ending in .xpe (exp pole
                figures), .xpx (exp + recalculated pole figures) and .odf
                (odf in Maud format). The ODF is maintained internally by
                the program, so it can be used to correct spectra for the
                texture (automatically) or to plot the reconstructed pole
                figures with the texture plot routines.
                A new tutorial has been added to the Maud pages for texture.
                
(06/12/2000):   1.67. The Rain release. 
                Never seen a year with all this rain like today....
                Bug fixing release. 
                Spectra plotting and printing improved. Added the peak detail
                starting picture. Workarounds for MacOSX and Unix added.
                
(01/11/2000):   1.66. The Open release. 
                Removed the password system. Now Maud is completely open to
                users. Preparing to open source the project. Linear
                Interpolation has been introduced to substitute the triangular
                interpolator in WIMV (texture) to solve the problem of the 90
                degrees missing interpolation. Corrected a bug introduced in
                last release for texture angle computation on position
                sensitive detectors. For application the starting class is
                now it.unitn.ing.rista.Maud
                There are the first two tutorials on line by L. Cont (on the
                Maud web page) and L. Cranswick (on the CCP14 suite under 
                tutorials). Regarding to the Cranswick tutorial, in this 
                release the menu item mentioned for Area Image with the name
                "Plot profile" is no more the correct one to use. Instead
                the menu item "Spectra profile" under the menu plug-ins should
                be used. This because ImageJ has been updated to version 1.19.
                Rewrited the 2D plotting for pole figures. Multi-PF can be
                plotted in full colors. Several enhancements for datafiles
                and added a warning dialog on exiting.
                Corrected a bug on importing non standard space group C-1.
                
(01/10/2000):   1.65. The Periodic Table release. 
                Bug fixing release + change the ugly long list of atom with
                a periodic table for selection. 
                
(14/09/2000):   1.64. The Dr. Gibaud release. 
                Alain just left few days ago.....
                Added support to set all parameters of an object equal to
                the parameters of another similar object; in the parameter
                list dialog, selecting an object instead of a parameter,
                on the last column you can free or fix all parameters of
                the object as well as use the set equal to dialog/tree to
                set this object equal to another (only for parameters).
                
(09/08/2000):   1.63. Billy the Kid release. 
                Watching the movie..... 
                Added buttons to append phases, instruments etc. to the
                database. Revised the Entropy method for texture and some
                equation in the planar defect (Warren) model.

(26/06/2000):   1.62. Film release. 
                Added support to import images (from the ImageJ package)
                of films and transform them in spectra. Spectra can be
                added and fitted by the program. Intensity calibration
                for film non linearity and saturation has been added.
                Example: film.par. The datafile has been obtained from
                the film1.jpg scannerized image of a Debye-Sherrer film.

(18/05/2000):   1.61. Bug fixed release. 
                Fixed some bugs and added specific features to help routinely 
                work. Added a user specific preference file that store some
                of the settings (like last analysis file used or folder
                visited).

(26/03/2000):   1.6. Major release. Reflectivity has been added. Only ESQUI/Pro
                version. The ESQUI/Pro version contains also the Strain/Stress
                ODF method. No informations here because under testing and 
                due a publication.
                - For reflectivity you need to choose the reflectivity matrix
                method in the dataset edit panel and to load the proper
                datafiles. Datafiles are a double column of q and intensities.
                Before to load a datafile you have to specify the instrument
                and the radiation used. Datafiles for reflectivity and with:
                .dcq : Log plot, weigths as 1/Intensity
                .dlq : Log Plot, WSS as (Log10(Ic) - Log10(Ie))^2, no weights
                .ddq : Normal sqrt plot and weigths as 1/sqrt(intensity).
                You specify the layered structure in Sample->layers.
                Remember to put the air as a first layer, with all zero's
                and the substrate as the last (thickness = 0).
                An example file is included (rrfil.par) in which the three
                different data formats are included (same internal file, only 
                extension change) and you can use one or the other by changing
                the enabled or not checkbox.
                No phases are needed for pure reflectivity analysis.
                
(21/03/2000):   1.59. Improvement release. One step away from a major release.
                - Added a button to force immediately the data range setting in 
                the dataset edit dialog. There is something more: if you have
                selected the option "keep lower memory occupation" (in the 
                options panel) then the data out of the selected range will be 
                removed from memory (not from the stored files!). In general
                this will happen also just before a computation and it save
                some precious memory for computation. (Saving and) Reloading
                the analysis file will force Maud reload the datafiles and
                restore the original range (until a computation or the button
                is pressed again). This permit you to select a wider range.
                - Intensity extractor life has been made easier. You don't have
                to worry anymore to select none or Le Bail depending if
                you work on texture or not. Just leave Le Bail selected in the
                dataset dialog (it's now the default when creating a dataset;
                if you have an old analysis file you can turn on manually) and
                the program will use it or not depending on what you select
                as texture model in the phase edit dialog (actually only
                WIMV, Entrophy and Arbitrary Text. use an intensity extractor).
                The intensities will not extracted for phases not having these
                models enabled.
                - Memory footprint has been decreased consistently expecially
                for the case where multiple files are loaded. Some bugs on
                memory release hopefully are gone.
                
(18/03/2000):   1.58. Improvement release.
                - Added a button to sum spectra based on rules like same Chi
                angle or Omega.
                - Added multiple selection support in list. Now it is possible
                to select more object and on remove all the selected objects
                are deleted. For the sum spectra button in the datafiles list
                only the selected files are summed (but not with the "based
                on rules" button, in that case all the enabled files are used).
                
(28/02/2000):   1.57. Bug fixing and improvement release.
                - Fixed some bugs for non standard space group recognition.
                - Layer absorption and correction (Bragg-Brentano geometry only)
                implemented for thin films.
                - Revised the tilting angle convention. Chi and Phi zeros should
                correspond to polar and azimuthal zeros in the pole figure.
                Omega zero is for a cradle with the rotational axis parallel
                to the incident beam. Only for Bragg-Brentano geometry in 
                theta-twoTheta measurement omega is the theta off-axis angle.
                - Fixed a bug preventing to load multiple datafiles if the first
                9 are deleted.
                
(28/02/2000):   1.56. Bug fixing release.
                - Fixed a bug for space group info about different origin 
                choices.
                - Revised the datafile list panel to improve readibility.
                
(13/02/2000):   1.55. Speed bump release.
                - Speed increase of 40% in the reference test (3 iterations
                of the alzrc.par analysis file).
                - Fixed a bug for import of CIF datafiles.
                
(09/02/2000):   1.54. Bug fixing release.
                - Added Arbitrary Texture model. This model will use directly
                the peak intensities extracted by the intensity extractor 
                model (if any enabled) to correct the spectrum. Works only
                if texture computation is enabled in the refinement options
                panel.
                - Added a button in the dataset panel (under the spectra
                tabPanel) to create and save a datafile containing a 
                spectrum that is computed as a mean of all the enabled spectra
                in the dataset. If an hexagonal grid is used (equal area
                coverage in the pole figure) and the entire hemisphere of
                a pole figure is covered by the spectra this ensure that 
                the resulting spectrum will be randomized.
                
(02/02/2000):   1.53. Bug fixing release.
                - Space group 230 is accepted now.
                - Generic TOF geometry corrected for texture angle computation.
                - .cif and .fit datafile are recognized in input (these data
                format correspond to the fitting output format).
                
(19/01/2000):   1.52. Bug fixing release.
                - Fixed the OpenGl rendering on different plattforms; update to
                gl4java 2.1.3.0 version. On windows only the M$ opengl32.dll 
                and glu32.dll libraries are supported.
                - Fixed some bugs in the Space Group manipulation and on input
                from CIF files. Now more non standard groups are recognized
                as well as all monoclinic groups.
                
(04/12/1999):   1.51. Bug fixing release.
                - Added sharpness of texture computation;
                - Added SODF by Harmonic with texture (not for public release
                for the moment). 
                
(28/11/1999):   1.50. Major version release.
                - SgInfo is pure java now; the SgInfo library is no more needed; 
                so there is a speed bump on part of the code. It is important 
                to remove the old version before to install this new one. Maud 
                actually may run on any java supporting computer.
                - Strain PF and DF in testing and also the parallel computing. 
                Will be enabled to public in the next release.
                
(04/11/1999):   1.40. The Berkeley release. Many improvements so far.
                - Speed bump by 50% during refinement; due to some code 
                optimizations.
                - Entropy method for texture corrected and proofed. Now the not 
                covered by data values in the ODF are negative but 
                interpolated using the available positive ones around. In the 
                export these are transformed to positive.
                - Harmonic implementation for texture and crystallite 
                reimplemented to correct all bugs. Now the S. Matthies 
                convention is in use not too much far from the Popa variant. 
                Cubic groups only up to l = 22 for now.
                - Pole figures in 3D are colored now from blu (lower values) to
                red higher values. I encountered some problem to run the 
                OpenGL in the latest version of NT. If the program shows some 
                error windows on a ddraw.dll library and openGl, try to trash 
                the opengl.dll and glu.dll. If still the program fails in 
                plotting with openGL use the java variant in the Popa 
                crystallites and the 2D plotting for the Pole figures instead 
                of 3D. I will try to upgrade the OpenGL implementation to 
                solve the problem in a future. For Pole Figures I recommend to
                export them and use the excellent Beartex package to 
                manipulate them. On Macintosh there is no OpenGL problem.
                - Double values are formatted now and the number of digits in 
                output has been reduced.
                - Some extra options in the wizard to fix all backgrounds pars
                or others.
                - Possibility to export pole figures in Beartex format from harm.
                or WIMV or Entropy. It is possible to export also the harmonic
                coefficients for ODF computations in Beartex.
                - Eliminated the atomLib native library, pure java now.
                - The registration method has been changed and may be someone
                (but I hope not) will need a new password. Sorry for that. 
                Actually under Windows NT and Mac OS 9 (and Unix in general) 
                different users can use the same program but need a different 
                registration.
                - The relative path from analysis to datafiles under Windows has
                been fixed. In some cases a much longer path was generated, in
                others (different disk) it was not working.
                - Changed the angular conversion routine for different geometry
                configurations.
                - I forgot what else was changed. Surpassed the 60000 lines for
                the core package (maud.jar).
                - First Linux version available. I tested on a LinuxPPC and on a 
                Linux x86. No OpenGL support for the moment.
                - Available general Unix release, by compiling a native C 
                library, SgInfo, (sources and instructions provided).
                About compatibility: the old values for crystallites in the Popa 
                anisotropic model are no more compatible with the new one. Due to
                reimplementation and bug correction. You may need to re-iterate
                the values to adjust them.
                
(20/10/1999):   1.35. Introduced the theoretical weights as an alternative to the
                experimental weights for a better background fitting. More
                information to be published.
                
(20/9/1999):    1.34. Corrected a bug preventing to change some instrumental data
                if the instrument is removed and added again from database.
                Maximum Entropy L is completed. This method for texture modelling
                is similar to WIMV except it doesn't need the interpolation step
                (the measured points are used) and use a more strictly entropy 
                formulation. Include an option for tube projections like the ADC
                method. The complete ODF coverage is not guarantee with this
                method.
                More experimental points and more coverage. Using sample
                symmetries and tube projections increases the coverage. Part of 
                the ODF not covered is saved as -1.0 in the MAUD ODF file format 
                and as 1.0 in the Beartex format. Actually the Beartex export is 
                not completed and simply a single column of data is saved instead,
                containing the ODF reduced for the crystal symmetries as normal in
                the Beartex format. Tube projection require more memory. The 
                texture output (pole figure plotting and coverage) is available
                in the principal menu of the main window instead of the old 
                locations. Choose the menu graphics->texture to open the window
                for texture plotting.
                Actually: 240 files and 55868 lines of code.
                
(10/9/1999):    1.33. Completed the GSAS incident intensity function 3, 4 etc.
                Harmonic texture implementation finished. For cubic symmetries
                only l up to 22 is implemented, don't use over that.
                For the harmonic method you don't have to set-up the intensity
                extraction as for the WIMV; choose the none intensity extraction 
                method in the dataset edit panel.
                There are 54705 lines of code (240 files) only for the maud core 
                library.
                Added more choices in the wizard panel.
                
(13/8/1999):    1.32. ICOTOM12 release. GSAS files with TIME_MAP working now.
                Corrected a bug in the plotting of recalculated pole figures.
                
(24/7/1999):    1.31. Bug correction release.

(20/7/1999):    1.30. Added some options in the data file plotting window.
                Corrected a bug for the intensity calibration by data file
                preventing to load the datafile during the computing.
                Added a size strain model allowing microstrain to cause
                Voigt broadening (Voigt microstrain model). Changed the 
                close button label to OK. Implemented the R-factor
                computation.
                
(25/6/1999):    1.29. Bug fixing release.

(18/5/1999):    1.28. Added support for cut/copy/paste for either strings 
                and objects in lists. Bug correction release.
                A wizard panel has been added under the Sample edit panel
                to allow the user to enforce an hexagonal grid coverage
                of the pole figure removing or disabling all datafiles that
                are not on the grid. The panel is under the plot Pole figure
                coverage panel.
                
(14/4/1999):    1.27. Added the Dubna/Skat instrument. Dubna data for now are 
                recognized if containing "swra" in the name (in the future should
                change). Data file are single column of intensities.
                
(06/4/1999):    1.26. Bug correction release. Corrected a bug for the P63, P63cm 
                and P63mc for some index and multiplicity computation.
                Space group with blank in between are interpreted now.
                Corrected a bug in the WIMV routine for the initial exponent
                RFANF optimization.
                Added the EntrophyL method for intensities extraction. Using this
                algorithm the speed for extraction is at least double the speed
                of the Le Bail algorithm. Three iterations are sufficient to 
                obtain the correct texture weights. Swing 1.1.1beta2 introduced.
                
(23/3/1999):    1.25. Bug correction release. Path to datafile corrected.
                Added a scale propertiy to the crystallite rendering.
                Added printing support for some windows. To be tested.
                Corrected a bug influencing quantitative phase analysis. The 
                number Z of molecules per Cell was used uncorrectly. Check
                your results; they are corrects only if Z was 1 or the same
                value for all phases. Now Z is no more used (not needed in
                reality).
                Some more tuning for cosmetic and small bugs. Changed behaviour
                for the lists displaying parameters that sometimes where
                loosing changes introduced.
                Introduced the Berkeley format for datafiles (*.spc) and a simple
                one (*.prn) containing only two columns of data 
                (2-theta Intensity).
                
(07/2/1999):    1.24. Bug correction release. Updated the plot of datafiles.
                Updated some help files and databases.
                
(26/1/1999):    1.23. Bug correction release. Now the title and analysis operator
                are correctly saved in the analysis file. Remain a bug discovered
                in the cubic symmetrization of the Popa anisotropic model.
                Added color chooser support to the crystallite rendering.
                Correction of a bug for the chi and omega broadening.
                
(18/1/1999):    1.22. Builded the faults model for broadening following the 
                Warren book. It is implemented only for FCC, BCC and HCP like
                structures (intermetallics included). Corrected a bug for the 
                restore command introduced with the path modification of
                version 1.2.
                
(2/1/1999):     1.21. Added instrumental broadening for Omega and Chi tilting.
                Corrected a bug in the openGL rendering of the crystallite
                under Windows.
                
(25/12/1998):   1.2. Christmas edition. This is the first beta release. The
                wizard option panel for refinement has been added. It permits to
                choose which kind of analysis you want to perform and the
                program will perform the refinement for that analysis in
                automatic mode. See the help on the panel for more. Console 
                window visible for bug reporting.
                Path to datafile is now relative to the analysis file (the *.par
                file). Previous version used to be relative to the program
                folder; the program recognize in any case the old setting also.
                Now you can move the analysis file and the datafiles togheter
                out of the program without the need to reload the datafile.
                Only the file name is visible in the datafile panel, not the 
                path. If you mantain the same path between analysis file and
                datafiles, you can move them to any disk or computer.
                Added a button to visualize the reconstructed pole figure
                (only contour plot, experimental pole figure button not active).
                To use it, select a reflection in the "hkl list" panel from
                the phase edit frame, and press the corresponding button. Wait
                10-20 secs to see the plot.

(20/12/1998):   1.11. Added support for crystal structure plotting (in the Phase
                edit dialog). The crystallite plot has speed up changing the 3D
                rendering to support the OpenGL rendering (OpenGL libraries are
                included with the program). The crystal structure plotting
                instead is pure java.
                Errors are computed for the parameters and saved in the analysis
                file also. The CIF convention of using the error on the last 
                digit of the parameter value is not respected. The absolute error
                value is saved.
                Added support for backup (automatic) and restore of the last 
                analysis. The last analysis is saved after any computation and
                can be restore using the restore menu item or the restore button
                in the toolbar.

(28/11/1998):   1.10. Completed the WIMV and the interpolation by arbitrary grid
                translation in Java. Now the program is completed for the texture
                part. Correction of many little bugs.

(1/11/1998):    1.08. The Popa model has been corrected completely and verified.
                The cubic branch coefficient implemented are up to l = 22. For
                this reason the maximum l selectable is 22.
                On Mac using MRJ 2.1beta1 there is a bug preventing to display
                correctly the "Save As" or "Open Analysis" dialogs; but they
                works using the toolbar instead. For this reason a "Save as" 
                toolbar button as been added.
                Added support for the database of structures. In the phase
                tabpanel now using the "Add (database)" button it is possible to
                select a database or a CIF file containing crystal structures 
                and to load the selected one. It works with any CIF compliant 
                file. The database of Rietquan can be used as well (be sure to 
                load the *.PDB file and not the *.PIN).
                A first database (structures.pdb) has been included in the 
                distribution. Not all the phases there has been verified for
                correctness.
                The structure of the analysis file as been changed a little bit
                for a better support of children objects. In any case the old
                analysis files are compatibles and can be loaded.
                
(1/11/1998):    1.07. Corrected monoclinic bug in the Popa model.
                
(28/10/1998):   1.06. Finished the anisotropic Popa model (J. Appl. Cryst. 31[2])
                for size-strain. Implemented new analytical Legrende functions
                for the spherical harmonics. There is a bug in the cubic branch
                for Lmax > 4 (it will be corrected a.s.a.p.). Implemented a 3D
                rendering of the crystallite to watch the shape.
                Implemented a contextual help; see the dialog box for the Popa
                symmetry in microstructure for an example.
                Fixed a bug in the sample dialog that was corrupting the dataset 
                dialog.
                New installation building.
                
(13/10/1998):   1.05. The Microstructure dialog has changed on a more simple
                structure. Fixed a bug that was preventing to reload a data file
                from a previous saved analysis file. Popa model for anisotropic
                crystallites and microstrain implemented. Added the Rigaku and
                Siemens file formats. Corrected a bug in the Philips one. Added a
                new interface (in progress). Using jdk version 1.1.7 on Windows.
                Added better support for versioning.
                
(06/10/1998):   1.0ea4. Corrected a bug that prevent loading data files on 
                windows.
                Fixed the Le-Bail method.
                Changed the structure for the Line-Broadening method; now has
                the plug-in structure.
                Updated the swing to 1.1beta3.

(23/09/1998):   1.0ea3. Save As dialog now works correctly on windows.
                Introduced a workaround to permit the refine under Windows 95.
                Corrected a bug to properly display the popup menu in the 
                graphical view of the diffraction spectra. It can be 
                instantiated by pressing the right mouse button (option+mouse 
                click on the Mac).
                The file save or saveas now produce a complete file that can be 
                load. It is not fully compatible with the Rietquan program.
                Added the window icon on windows.
                Added this readme file.
                
(15/09/1998):   1.0ea2. Added the ParameterList to change the value of parameters
                and set their status: refinable/not refinable/equal to.

(09/09/1998):   1.0ea1. Only one (the Delft) model for line broadening is actually
                usable.
                Saving parameters in a file doesn't work.
                
--------------------------------------------------------------------------------
By Luca Lutterotti

Luca.Lutterotti@ing.unitn.it
