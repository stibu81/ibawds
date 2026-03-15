# Get Files for File Reading Exercise

Copy the files for an exercise for reading files to a directory.

## Usage

``` r
get_reading_exercise_files(path, unzip = TRUE)
```

## Arguments

- path:

  path where the files should be copied to.

- unzip:

  logical indicating whether the files should be unzipped. Set this to
  `FALSE` if unzipping fails.

## Value

Logical indicating the success of the copy operation.

## Details

There are 8 files in total. Apart from a few errors that were introduced
for the purpose of the exercise, they all contain the same data:
information about 100 randomly selected Swiss municipalities. The full
file can be downloaded from
<https://www.bfs.admin.ch/bfsstatic/dam/assets/7786544/master>.
