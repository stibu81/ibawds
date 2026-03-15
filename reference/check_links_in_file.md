# Check All Links in a Text File

Find and check all http(s) URLs in an text file. Only links starting
with `http://` or `https://` are found and checked.

## Usage

``` r
check_links_in_file(file)
```

## Arguments

- file:

  the path to the file to be checked.

## Value

a tibble with two columns:

- `url`: the URL that was found and checked

- `reachable`: whether the URL could be reached
