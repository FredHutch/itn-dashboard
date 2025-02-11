# `itn-dashboard` Maintenance

## Courses

### My plot doesn't look how I expect it too....what do I do?

#### Coloring

#### Missing Data

#### A website/course is there that shouldn't be



## Workshops

## CRAN

### If you want to add a new package....

1. Add the package to the `.github/workflows/get_itn_data.R` script, within the `cran_downloads()` function and its `packages` argument vector specifically. 

2. Add a `geom_vline` with a new color e.g., `geom_vline(aes(xintercept="2024-02"), linetype="dashed", color = '#fde725') + #package published date`

3. Add that corresponding color to ...

### If you want to add a package that has been around and we contributed to it

1.

2. 