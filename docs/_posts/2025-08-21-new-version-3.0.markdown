---
layout: post
title:  "New Version 3.0"
date:   2026-08-27 13:10:00 -0600
categories: Maud release
---

A new MAUD version (3.0) is available in the releases section.
The "Hippo scattering" release.
You have to check the glorious Hippo scattering.
This version is quite a big step from previous. It should be in general better and more robust.
But a word of cation is necessary because with all the changes, especially in the core, it may be advisable to keep a copy of your previous Maud version, just in case.
This version include the first DiffaX model (for dhcp structures) but a more general will follow.
The DiffaX computation in Maud includes crystallite sizes, microstrains and texture. No strains. For texture only models not using a texture extraction engine can be used for refining texture. For the other only texture computation from a fixed ODF.
It is also the first version including the angle-energy maps computation, even if is better to wait for a full debug of them before using. The computation has been extensively changed to include texture and stresses and still it is not everything complete.
Some more additional notes can be found in the help->readme of Maud.

Please notice that the downloading directory structures have been changed in the gdrive.
More Maud releases has been added for the arm type of CPUs for both Linux and Windows.
The java jdk included has been updated to the Java 25 (LTS). If you notice something not working properly, please report the problem with the OS you are using and Maud version.
Older versions of the Maud program can be downloaded from Github.

Download links:

Download from [Goggle drive][gdrive]
The versions in the directories with "..._x64..." are for Intel/amd cpus, while "..._aarch64..." are for arm type of cpus (Mx processors for Apple Macs).

To download here on github the program and source check on [MAUD download][maud-download]. 
Older versions are available only on github.

To start tutorials are probably the best and can be found at [MAUD tutorials][maud-tutorials]. 

[maud-docs]: /maud/documents/
[maud-tutorials]: /maud/tutorials/
[maud-download]: https://github.com/luttero/maud/releases/tag/v3.0
[gdrive]: https://drive.google.com/drive/folders/1EQw0XPx6QPwE-VN7OpTvI8DtrvsW-V4i?usp=sharing