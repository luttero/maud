---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults

layout: post

---

{% include bar.md %}

<div class="row">
    <div id="maud1-examples" class="col-md-6">
        <div class="panel panel-default">
            <div class="panel-heading">
                <i class="fa fa-tutorial2-o"></i>
                Examples of Maud analysis
                <i class="fa fa-angle-double-right"></i>
            </div>
            <div class="panel-body">
                <ul class="no_bullet">
                     <li>
                        {% include reference.html link="texture/montmorillonite.html" content="Example on texture analysis on Montmorillonite" %}
                    </li>
                 </ul>
            </div>
        </div>
    </div>
    <div id="maud2-examples" class="col-md-6">
        <div class="panel panel-default">
            <div class="panel-heading">
                <i class="fa fa-tutorial1-o"></i>
                Maud default examples
            </div>
            <div class="panel-body">
                <ul>
                    <li>
                        {% include reference.html link="examples.zip" content="Download the Maud default examples. These should be already available in your "Home/Documents/Maud" directory after you run first time the program." %}
                    </li>
                </ul>
            </div>
        </div>
    </div>
</div>


{% include nav.md %}

{% include license.md %}
