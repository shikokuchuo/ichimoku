# Read/write Objects \<\> Archive Files with Data Verification

Read and write objects to/from archival storage in the native RData
format, with verification of data integrity.

## Usage

``` r
archive(..., object, file)
```

## Arguments

- ...:

  unnamed arguments will be parsed as 'file' if there is only one
  argument, 'object' and 'file' if there are two arguments.

- object:

  (for write operations) an object.

- file:

  the name of the file or a connection where the object is saved to or
  read from.

## Value

For read operations: the object originally archived.

For write operations: the filename supplied. 'object' is written to
'file'.

## Details

For read operations: specify only 'file', or alternatively if no
arguments are specified, a system dialog will be opened allowing a file
to be chosen interactively. 'file' is read and the return value may be
assigned to an object. A confirmation message is issued if the file read
operation has been successful.

For write operations: specify both 'object' and 'file'. If only 'object'
is specified and 'file' is left empty (see examples), a system dialog
will be opened allowing the file save location to be chosen
interactively. 'object' will be written to 'file'. A confirmation
message is issued if the file write operation has been successful.

## Data Verification

A SHA256 hash of the original object is written to the archive. This
allows the data integrity of the restored object to be verified when the
archive is read back.

For write operations: confirmation of the SHA256 hash written to file is
displayed.

For read operations: a 'data verified' message is issued if the SHA256
hash found within the data file has been authenticated.

## Further Details

Please refer to the reference vignette by calling:
[`vignette("reference", package = "ichimoku")`](https://shikokuchuo.net/ichimoku/dev/articles/reference.md)

## Examples

``` r
cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
file <- tempfile()

archive(cloud, file)
#> Archive written to '/tmp/RtmpdW84iQ/file1d044c58c050'
#> SHA256: f04ccb6b8869372733acb7ad07d468624e58b1b8ee9d6a715ab13e698fd1373b

restored <- archive(file)
#> Archive read from '/tmp/RtmpdW84iQ/file1d044c58c050'
#> Data verified by SHA256: f04ccb6b8869372733acb7ad07d468624e58b1b8ee9d6a715ab13e698fd1373b

unlink(file)

if (interactive()) {
# Only run examples in interactive R sessions
# Read file to 'object' using system dialog:
object <- archive()

# Write 'cloud' to file using system dialog:
archive(cloud, )
}
```
