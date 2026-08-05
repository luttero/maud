---
layout: post
title:  "New Version 2.99997"
date:   2026-08-05 12:30:00 -0600
categories: Maud release
---

A new MAUD version (2.99997) is available in the releases section.
The "End of life" release.
This will be the last version 2.x release before version 3.0 that will follow shortly.
So it is advice to keep this version for old analyses as version 3 is not guaranteed to work with everything from past versions and you can expect some new bugs or models not working initially.
This is mainly a bugs correction release. The standard function texture model has been changed a bit. 
Now it is using HWHM for fiber and spherical components. A bug concerning the normalisation not always applied has been fixed. ODF output from standard function has been fixed also. 
Kearns factors are calculated with the sharpness index computation in texture. Output to the console. 
Fixed the refinement when using some weights modifiers that were not properly applied. 
Fixed the batch launch computation and added some methods to generate AI training patterns. 
Some exports from the plot window has been fixed also.

Download links:

Download from [Goggle drive][gdrive]

To download here on github the program and source check on [MAUD download][maud-download]. 
Older versions are available only on github.

To start tutorials are probably the best and can be found at [MAUD tutorials][maud-tutorials]. 

[maud-docs]: /maud/documents/
[maud-tutorials]: /maud/tutorials/
[maud-download]: https://github.com/luttero/maud/releases/tag/v2.99997
[gdrive]: https://drive.google.com/drive/folders/1EQw0XPx6QPwE-VN7OpTvI8DtrvsW-V4i?usp=sharing