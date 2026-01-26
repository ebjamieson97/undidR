# Shows valid date formats

The `undid_date_formats()` function returns a list of all valid date
formats that can be used within the `undidR` package.

## Usage

``` r
undid_date_formats()
```

## Value

A named list containing valid date formats:

- `General_Formats`: General date formats compatible with the package.

- `R_Specific_Formats`: Date formats specific to R.

- `Other_Formats`: Formats seen sometimes in Stata.

## Details

The date formats returned by this function are used to ensure
consistency in date processing within the `undidR` package.

## Examples

``` r
undid_date_formats()
#> $General_Formats
#>  [1] "yyyy/mm/dd" "yyyy-mm-dd" "yyyymmdd"   "yyyy/dd/mm" "yyyy-dd-mm"
#>  [6] "yyyyddmm"   "dd/mm/yyyy" "dd-mm-yyyy" "ddmmyyyy"   "mm/dd/yyyy"
#> [11] "mm-dd-yyyy" "mmddyyyy"   "yyyy"      
#> 
#> $R_Specific_Formats
#>  [1] "%Y/%m/%d" "%Y-%m-%d" "%Y%m%d"   "%Y/%d/%m" "%Y-%d-%m" "%Y%d%m"  
#>  [7] "%d/%m/%Y" "%d-%m-%Y" "%d%m%Y"   "%m/%d/%Y" "%m-%d-%Y" "%m%d%Y"  
#> [13] "%Y"      
#> 
#> $Other_Formats
#> [1] "ddmonyyyy" "yyyym00"  
#> 
```
