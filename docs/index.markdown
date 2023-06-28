---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults

layout: home
keywords: Homepage, home, news, tutorial, examples, MAUD, Rietveld method, diffraction, fluorescence, x-ray, xray, neutron, electron, documentation, latest publications, requirements, licensing
---

<b>MAUD</b> is a free software to analyse diffraction data using a combined Rietveld method. Its capabilities extend beyond diffraction and include fluorescence and reflectivity. It can analyse data from X-ray sources as well as neutrons, TOF, electrons from TEM.

<br>
[![](https://badgen.net/badge/icon/github?icon=github&label)](https://github.com/luttero/maud)&nbsp;
[![](https://img.shields.io/badge/Research-Gate-9cf)](https://www.researchgate.net/lab/X-Ray-Lab-UNITN-Luca-Lutterotti)&nbsp;
[![](https://img.shields.io/twitter/follow/MaudProgram)](https://twitter.com/MaudProgram)&nbsp;
[![](https://img.shields.io/badge/Youtube-MAUD)](https://www.youtube.com/@MaudRietveldProgram)&nbsp;
![](https://img.shields.io/github/languages/top/luttero/maud)&nbsp;
![](https://img.shields.io/github/downloads/luttero/maud/total)&nbsp;
![](https://img.shields.io/github/contributors/luttero/maud)&nbsp;

<div class="row">
    <div id="home-news" class="col-md-6">
        <div class="panel panel-default">
            <div class="panel-heading">
                <i class="fa fa-newspaper-o"></i>
                News
                <i class="fa fa-angle-double-right"></i>
                {% include reference.html link="changelog" content="Complete&nbsp;changelog" %}
            </div>
            <div class="panel-body">
                <ul class="no_bullet">
					<li>
                        June 28, 2023 &minus;
							{% include reference.html link="https://github.com/luttero/maud/releases/tag/v2.998" content="Download MAUD latest version" download=true %}
                        released:
                        {% include reference.html link="changelog#1" content="Release Notes" %}
                    </li>
					<li>
                        June 28, 2023 &minus;
							{% include reference.html link="http://www.ecole.ensicaen.fr/~chateign/formation/flyers/2023-Flyer-13th-workshop.pdf" content="Combined Analysis School 2023 in Caen" download=true %}
                    </li>
                    <li>
                        June 28, 2023 &minus; new homepage on Github.io
                    </li>
                 </ul>
            </div>
        </div>
    </div>
    <div id="home-community" class="col-md-6">
        <div class="panel panel-default">
            <div class="panel-heading">
                <i class="fa fa-comments-o"></i>
                Community
            </div>
            <div class="panel-body">
                <ul>
                    <li>
                        {% include reference.html link="https://github.com/luttero/maud/discussions" content="Forum" %}
                        - Do you have a question or something to share about MAUD and analyses?
                    </li>
                    <li>
                        {% include reference.html link="https://github.com/luttero/maud/issues" content="Bugs" %}
                        - Report bugs if you like them being fixed.
                    </li>
                </ul>
            </div>
        </div>
    </div>
</div>

<div class="row">
    <div id="home-documentation" class="col-md-12">
        <div class="panel panel-default">
            <div class="panel-heading">
                <i class="fa fa-book"></i>
                Documentation
                <i class="fa fa-angle-double-right"></i>
                {% include reference.html link="Documentation" content="Documentation" %}
                | {% include reference.html link="tutorials" content="Tutorials" %}
                | {% include reference.html link="download#installation" content="Installation" no_icon=true %}
                | {% include reference.html link="examples" content="Examples" %}
                | {% include reference.html link="videos" content="Videos" %}
            </div>
        </div>
    </div>
</div>

<!--div class="row">
    <div class="col-md-4">
        <div class="panel panel-default text-center">
            <div class="panel-heading">
                <img src="images/thumbnails/GrainTutorial.jpg">
            </div>
            <div class="panel-body">
                <h4>Grain Tutorial</h4>
                <p>A quick guide through the grain reconstruction capabilities of MTEX.</p>
                {% include reference.html link="GrainTutorial.html" content="Learn More" class="btn btn-primary" %}
            </div>
        </div>
    </div>
    <div class="col-md-4">
        <div class="panel panel-default text-center">
            <div class="panel-heading">
                <img src="images/thumbnails/EBSDDenoising.jpg">
            </div>
            <div class="panel-body">
                <h4>Denoising Orientation Maps</h4>
                <p>In this section we demonstrate how random errors can be significantly reduced using denoising techniques.</p>
                {% include reference.html link="EBSDDenoising.html" content="Learn More" class="btn btn-primary" %}
            </div>
        </div>
    </div>
    <div class="col-md-4">
        <div class="panel panel-default text-center">
            <div class="panel-heading">
                <img src="images/thumbnails/MaParentGrainReconstruction.jpg">
            </div>
            <div class="panel-body">
                <h4>Martensite Parent Grain Reconstruction</h4>
                <p>Here we demonstrate the tools MTEX offers to reconstruct a parent austenite phase from a measured martensite phase.</p>
                {% include reference.html link="MaParentGrainReconstruction.html" content="Learn More" class="btn btn-primary" %}
            </div>
        </div>
    </div>
</div-->

<!--div class="row">
    <div id="home-latest-publications" class="col-md-6">
        <div class="panel panel-default">
            <div class="panel-heading">
                <i class="fa fa-file-pdf-o"></i>
                Latest&nbsp;Publications
                <i class="fa fa-angle-double-right"></i>
                {% include reference.html link="publications" content="All&nbsp;publications" %}
            </div>
            <div class="panel-body">
                <ul>
                    <li>{% include reference.html link="https://arxiv.org/pdf/2201.02103.pdf" content="The variant graph approach to improved parent grain reconstruction" %}</li>
                    <li>{% include reference.html link="https://www-user.tu-chemnitz.de/~rahi/paper/parentGrain.pdf" content="Parent grain reconstruction from partially or fully transformed microstructures in MTEX" %}</li>
                    <li>{% include reference.html link="https://www-user.tu-chemnitz.de/~rahi/paper/gazingAtCrystalBalls.pdf" content="Gazing at crystal balls - electron backscatter diffraction indexing and cross correlation on the sphere" %}</li>
                    <li>{% include reference.html link="https://www-user.tu-chemnitz.de/~rahi/paper/denoising.pdf" content="Denoising of Crystal Orientation Maps" %}</li>
                </ul>
            </div>
        </div>
    </div>
    <div id="home-addons" class="col-md-6">
        <div class="panel panel-default">
            <div class="panel-heading">
                <i class="fa fa-code-fork"></i>
                Featured&nbsp;Addons
                <i class="fa fa-angle-double-right"></i>
                {% include reference.html link="addons" content="All&nbsp;Toolboxes" %}
            </div>
            <div class="panel-body">
                <ul>
                    <li>{% include reference.html link="addons#addons-mtex-gui" content="MTEX GUI" %}</li>
                    <li>{% include reference.html link="addons#addons-mtex2gmsh" content="MTEX2Gmsh" %}</li>
                    <li>{% include reference.html link="addons#addons-stabix" content="Stabix" %}</li>
                    <li>{% include reference.html link="addons#addons-crystal-aligner" content="crystalAligner" %}</li>
                    <li>{% include reference.html link="addons#addons-or-tools" content="ORTools" %}</li>
                    <li>{% include reference.html link="addons#addons-phase-segmenter" content="phaseSegmenter" %}</li>
                </ul>
            </div>
        </div>
    </div>
</div-->

<div class="row">
    <div id="home-requirements-and-licensing" class="col-md-12">
        <div class="panel panel-default">
            <div class="panel-heading">
                <i class="fa fa-copyright"></i>
                Licensing
            </div>
            <div class="panel-body">
                <ul>
                    <li>
                        MAUD is a free software running in a Java Virtual Machine (included). Is a standalone software and can be used as it is. 
                    </li>
                    <li>
                        You are permitted to use it for the purposes it was build (scientific analyses of materials and compounds) and the author is not responsible for different or improper use of it. You are not permitted to re-distribute or sell the MAUD software, share instead the link to its home page here.
                    </li>
                </ul>
            </div>
        </div>
    </div>
</div>
