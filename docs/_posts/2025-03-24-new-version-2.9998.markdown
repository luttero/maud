---
layout: post
title:  "New Version 2.9998"
date:   2025-03-24 11:49:00 +0200
categories: Maud release
---

A new MAUD version (2.9998) is available in the releases section.
Fixed a bug introduced in version 2.9997 affecting CIF importing of structures.
If the CIF structure file contains atom sites with no specification of the atom element type it enter an infinite loop and the atom type remain unspecified.
So, an analysis file with such unspecified type, create again the loop on loading. The phase should be removed and the CIF reloaded with the new version.

Other download links:

Download from [DropBox][dropbox]

Download from [Goggle drive][gdrive]

To download here on github the program and source check on [MAUD download][maud-download]. 
Also older versions are available on github.

To start tutorials are probably the best and can be found at [MAUD tutorials][maud-tutorials]. 

[maud-docs]: /maud/documents/
[maud-tutorials]: /maud/tutorials/
[maud-download]: https://github.com/luttero/maud/releases/tag/v2.9998
[dropbox]: https://www.dropbox.com/sh/3l4jpjw7mkc3cfo/AAAtzz-9__TMmUdaxlolX68xa?dl=0
[gdrive]: https://drive.google.com/drive/folders/1EQw0XPx6QPwE-VN7OpTvI8DtrvsW-V4i?usp=sharing