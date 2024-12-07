data clean & EDA
================

## Clean data

``` r
tornado_data <- read.csv("./data/1950-2023_actual_tornadoes.csv", na.strings = c("NA", "N/A", " "))

cleaned <- tornado_data |>
  filter(yr >= 2000) |>
  select(-tz, -stf, -ns, -sn, -sg, -f1, -f2, -f3, -f4) |>
  relocate(fc, .after = mag)|>
  drop_na()
  
# Optional
write.csv(cleaned, "cleaned_df.csv", row.names = FALSE)
```

``` r
summary(cleaned)
```

    ##        om               yr             mo               dy       
    ##  Min.   :     1   Min.   :2000   Min.   : 1.000   Min.   : 1.00  
    ##  1st Qu.:   671   1st Qu.:2005   1st Qu.: 4.000   1st Qu.: 9.00  
    ##  Median :302789   Median :2011   Median : 5.000   Median :16.00  
    ##  Mean   :290793   Mean   :2011   Mean   : 5.966   Mean   :16.37  
    ##  3rd Qu.:616025   3rd Qu.:2018   3rd Qu.: 8.000   3rd Qu.:24.00  
    ##  Max.   :623401   Max.   :2023   Max.   :12.000   Max.   :31.00  
    ##      date               time                st                 stn        
    ##  Length:29507       Length:29507       Length:29507       Min.   :  0.00  
    ##  Class :character   Class :character   Class :character   1st Qu.:  0.00  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :  0.00  
    ##                                                           Mean   : 15.19  
    ##                                                           3rd Qu.: 20.00  
    ##                                                           Max.   :198.00  
    ##       mag                fc         inj                 fat           
    ##  Min.   :-9.0000   Min.   :0   Min.   :   0.0000   Min.   :  0.00000  
    ##  1st Qu.: 0.0000   1st Qu.:0   1st Qu.:   0.0000   1st Qu.:  0.00000  
    ##  Median : 0.0000   Median :0   Median :   0.0000   Median :  0.00000  
    ##  Mean   : 0.2391   Mean   :0   Mean   :   0.7458   Mean   :  0.05958  
    ##  3rd Qu.: 1.0000   3rd Qu.:0   3rd Qu.:   0.0000   3rd Qu.:  0.00000  
    ##  Max.   : 5.0000   Max.   :0   Max.   :1500.0000   Max.   :158.00000  
    ##       loss               closs               slat            slon        
    ##  Min.   :0.000e+00   Min.   :       0   Min.   :17.72   Min.   :-163.53  
    ##  1st Qu.:0.000e+00   1st Qu.:       0   1st Qu.:33.32   1st Qu.: -97.79  
    ##  Median :0.000e+00   Median :       0   Median :37.09   Median : -92.40  
    ##  Mean   :3.181e+05   Mean   :    2376   Mean   :37.19   Mean   : -92.18  
    ##  3rd Qu.:0.000e+00   3rd Qu.:       0   3rd Qu.:40.77   3rd Qu.: -86.55  
    ##  Max.   :1.550e+09   Max.   :12250000   Max.   :61.02   Max.   : -64.72  
    ##       elat            elon              len               wid        
    ##  Min.   : 0.00   Min.   :-163.53   Min.   :  0.000   Min.   :   0.0  
    ##  1st Qu.:33.24   1st Qu.: -97.70   1st Qu.:  0.300   1st Qu.:  30.0  
    ##  Median :37.03   Median : -92.18   Median :  1.160   Median :  50.0  
    ##  Mean   :36.85   Mean   : -91.24   Mean   :  3.343   Mean   : 142.3  
    ##  3rd Qu.:40.75   3rd Qu.: -86.28   3rd Qu.:  3.870   3rd Qu.: 150.0  
    ##  Max.   :61.02   Max.   :   0.00   Max.   :168.530   Max.   :4576.0

``` r
skimr::skim(cleaned)  
```

|                                                  |         |
|:-------------------------------------------------|:--------|
| Name                                             | cleaned |
| Number of rows                                   | 29507   |
| Number of columns                                | 20      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |         |
| Column type frequency:                           |         |
| character                                        | 3       |
| numeric                                          | 17      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |         |
| Group variables                                  | None    |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| date          |         0 |             1 |  10 |  10 |     0 |     4182 |          0 |
| time          |         0 |             1 |   8 |   8 |     0 |     1436 |          0 |
| st            |         0 |             1 |   2 |   2 |     0 |       53 |          0 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |      mean |          sd |      p0 |     p25 |       p50 |       p75 |         p100 | hist  |
|:--------------|----------:|--------------:|----------:|------------:|--------:|--------:|----------:|----------:|-------------:|:------|
| om            |         0 |             1 | 290793.12 |   286698.59 |    1.00 |  671.00 | 302789.00 | 616024.50 |  6.23401e+05 | ▇▁▁▁▇ |
| yr            |         0 |             1 |   2011.43 |        6.93 | 2000.00 | 2005.00 |   2011.00 |   2018.00 |  2.02300e+03 | ▇▇▆▇▇ |
| mo            |         0 |             1 |      5.97 |        2.62 |    1.00 |    4.00 |      5.00 |      8.00 |  1.20000e+01 | ▃▇▅▂▃ |
| dy            |         0 |             1 |     16.37 |        8.78 |    1.00 |    9.00 |     16.00 |     24.00 |  3.10000e+01 | ▇▇▇▇▇ |
| stn           |         0 |             1 |     15.19 |       27.80 |    0.00 |    0.00 |      0.00 |     20.00 |  1.98000e+02 | ▇▁▁▁▁ |
| mag           |         0 |             1 |      0.24 |        1.91 |   -9.00 |    0.00 |      0.00 |      1.00 |  5.00000e+00 | ▁▁▁▇▁ |
| fc            |         0 |             1 |      0.00 |        0.00 |    0.00 |    0.00 |      0.00 |      0.00 |  0.00000e+00 | ▁▁▇▁▁ |
| inj           |         0 |             1 |      0.75 |       13.50 |    0.00 |    0.00 |      0.00 |      0.00 |  1.50000e+03 | ▇▁▁▁▁ |
| fat           |         0 |             1 |      0.06 |        1.29 |    0.00 |    0.00 |      0.00 |      0.00 |  1.58000e+02 | ▇▁▁▁▁ |
| loss          |         0 |             1 | 318136.65 | 13912569.73 |    0.00 |    0.00 |      0.00 |      0.20 |  1.55000e+09 | ▇▁▁▁▁ |
| closs         |         0 |             1 |   2376.39 |   108141.81 |    0.00 |    0.00 |      0.00 |      0.00 |  1.22500e+07 | ▇▁▁▁▁ |
| slat          |         0 |             1 |     37.19 |        4.94 |   17.72 |   33.32 |     37.09 |     40.77 |  6.10200e+01 | ▁▆▇▂▁ |
| slon          |         0 |             1 |    -92.18 |        8.47 | -163.53 |  -97.79 |    -92.40 |    -86.55 | -6.47200e+01 | ▁▁▁▇▂ |
| elat          |         0 |             1 |     36.85 |        6.10 |    0.00 |   33.24 |     37.03 |     40.75 |  6.10200e+01 | ▁▁▇▇▁ |
| elon          |         0 |             1 |    -91.24 |       12.29 | -163.53 |  -97.70 |    -92.18 |    -86.28 |  0.00000e+00 | ▁▂▇▁▁ |
| len           |         0 |             1 |      3.34 |        6.35 |    0.00 |    0.30 |      1.16 |      3.87 |  1.68530e+02 | ▇▁▁▁▁ |
| wid           |         0 |             1 |    142.33 |      238.84 |    0.00 |   30.00 |     50.00 |    150.00 |  4.57600e+03 | ▇▁▁▁▁ |
