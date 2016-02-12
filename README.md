#ADF Tools

<b>Author:</b>        Wei-ju Wu<br>
<b>Creation Date:</b> February 11th, 2011<br>
<b>Description:</b>   Tools for accessing Amiga file system images in Java

##About this project

This is a collection of software tools to access Amiga file systems.

At the core is a file system library written in Scala.
I started to write this because I wanted to use my Mac to prepare
a couple of ADF files for testing on UAE.

##Usage

Since Github does not support downloads anymore, a prebuilt
binary can be downloaded from

https://dl.dropboxusercontent.com/u/1361536/amiga/adftools-1.0.jar

On most systems, a double click on the jar file's icon should
work. Alternatively, the browser can be started with

    java -jar adftools-1.0.jar

from the command line.

## Building
The project can be built with sbt.
The library is in the adf-core module, the application is in the app module.

$ sbt
> project app
> run

For building an assembly jar file that contains all dependencies, run

sbt> assembly

##Status

- supports Double Density disks
- supports OFS and FFS
- can create disks
- can add files to a disk
- rename files/directories/disk, move files between directories
- delete files (non-destructive)
- can copy files from a disk
- view icons from .info files
- stretch icons to display adapt aspect ratio
- 1.3 and 2.0 palettes for icon display

##Background

The ADF C library does not compile correctly on my machine and I needed
a Java library to access ADF files anyways, so I just started to work
on it.

The file system specifications were taken from

Ralph Babel's Amiga Guru Book, chapters 9, 10 and 15
Data Becker Amiga Intern, chapter 3.8

http://lclevy.free.fr/adflib/adf_info.html
