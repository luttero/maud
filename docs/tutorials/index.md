---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults

layout: post

---

{% include bar.md %}

<div class="row">
    <div id="maud2_tutorial" class="col-md-6">
        <div class="panel panel-default">
            <div class="panel-heading">
                <i class="fa fa-tutorial2-o"></i>
                Tutorials - MAUD v.2.x
                <i class="fa fa-angle-double-right"></i>
            </div>
            <div class="panel-body">
                <ul class="no_bullet">
					<li>
                        {% include reference.html link="QPA2/main.html" content="Performing an x-ray quantitative analysis in five easy steps." %}
                    </li>
					<li>
                        {% include reference.html link="sizestrain/instrumentalbroadening.html" content="Determining the instrumental broadening of your instrument (for size-strain analyses)" %}
                    </li>
					<li>
                        {% include reference.html link="imagesToSpectra2/index.html" content="Converting image data to spectra and performing a refinement with an old film camera" %}
                    </li>
					<li>
                        {% include reference.html link="ODFfromPF2/index.html" content="Texture analysis from traditional pole figures with EWIMV" %}
                    </li>
					<li>
                        {% include reference.html link="HippoWizard/hippowizard.html" content="Hippo Texture Analysis Wizard" %}
                    </li>
                 </ul>
            </div>
        </div>
    </div>
    <div id="maud1-tutorial" class="col-md-6">
        <div class="panel panel-default">
            <div class="panel-heading">
                <i class="fa fa-tutorial1-o"></i>
                Tutorials - MAUD v.2.x
            </div>
            <div class="panel-body">
                <ul>
					<li>
                        {% include reference.html link="QPA/main.html" content="Hippo Texture Analysis Wizard" %}
                    </li>
					<li>
                        {% include reference.html link="sizestrain/instrumentalbroadening.html" content="Hippo Texture Analysis Wizard" %}
                    </li>
					<li>
                        {% include reference.html link="imagesToSpectra/index.html" content="Hippo Texture Analysis Wizard" %}
                    </li>
					<li>
                        {% include reference.html link="ODFfromPF/index.html" content="Hippo Texture Analysis Wizard" %}
                    </li>
                </ul>
            </div>
        </div>
    </div>
</div>


{% include nav.md %}

{% include license.md %}
