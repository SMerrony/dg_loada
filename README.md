# loada

`loada` loads (restores) legacy Data General AOS/VS DUMP_II, and maybe DUMP_III, files on any modern Unix-like system supported by GNAT Ada.

It can be used to rescue data from legacy AOS/VS systems if the dumps are accessible on a modern system.  

The current version handles at least versions 15 and 16 of the DUMP format.

This software is a port of the LoadG utility from my https://github.com/SMerrony/aosvs-tools programs.
As of March 2021 it is believed to be functionally equivalent to that program and shares the same SemVer.

## Build

Use the `gprbuild` command in the directory where you unpacked or cloned this software.

## Usage
```
./loada
Usage of loada:
  -dumpfile <file>  DUMP_II or DUMP_III file to read/load (required)
  -extract          extract the files from the DUMP_II/III into the current directory
  -ignoreErrors     do not exit if a file or link cannot be created
  -list             list the contents of the DUMP_II/III file
  -summary          concise summary of the DUMP_II/III file contents (default true)
  -verbose          be rather wordy about what loada is doing
  -version          show the version number of loada and exit
  ```

To list the contents of a dump: `./loada -dumpfile ACK.DMP -summary`

To extract the contents of a dump `./loada -dumpfile ACK.DMP -extract` 

### Notes
 * All files and directories will be created using upper-case names
(the AOS/VS file system was case insensitive) 
 * Subdirectories found in the dump are created as needed
 * Link files will be created as symbolic links
 * ACLs (file permissions) are ignored
 * `loada` will not traverse above the current directory (this was legal in AOS/VS dumps) 
 * Will not build on Windows due to lack of symlinks
