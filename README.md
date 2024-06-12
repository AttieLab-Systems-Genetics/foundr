# foundr: Multiparent Founder Study Tools

This package can be used on its own for analysis and visualization of founder data,
and is part of a (planned) collection of packages. See
[Foundr App Developer Guide](https://docs.google.com/presentation/d/171HEopFlSTtf_AbrA28YIAJxJHvkzihB4_lcV6Ct-eI)
for an overview of package(s) use and components.

- [foundr](https://github.com/byandell/foundr): data analysis and visualization
- [foundrShiny](https://github.com/byandell/foundrShiny): interactive shiny app
- foundrHarmony: harmonize data from multiple sources (to be written)
- [modulr](https://github.com/byandell/modulr): harmonize WGCNA module objects

There is a default app ([inst/shinyApp/app.R](https://github.com/byandell/foundr/blob/main/inst/shinyApp/app.R))
and shiny code in the
[foundr](https://github.com/byandell/foundr)
package, which will be removed soon. See [foundrShiny](https://github.com/byandell/foundrShiny)
for the revised shiny code.

The foundrHarmony repo is currently in two places. The
[foundr](https://github.com/byandell/foundr)
package has generic functions for harmonizing data. The
[FounderDietStudy](https://github.com/byandell/FounderDietStudy)
repository has code for harmonizing specific datasets. This needs to be organized better. See

- [DataHarmony.Rmd](https://github.com/byandell/FounderDietStudy/blob/main/DataHarmony.Rmd)
- [WGCNA.Rmd](https://github.com/byandell/FounderDietStudy/blob/main/WGCNA.Rmd)
- [WGCNAmixed_Data.Rmd](https://github.com/byandell/FounderDietStudy/blob/main/WGCNAmixed_Data.Rmd)

Specialized instances are being developed for particular studies. See code in

- <https://github.com/byandell/FounderCalciumStudy>
- <https://github.com/byandell/FounderDietStudy>

See data visualizations in web apps

- <https://connect.doit.wisc.edu/FounderCalciumStudy/>
- <https://connect.doit.wisc.edu/FounderLiverDietStudy/> (need password)
- <https://connect.doit.wisc.edu/FounderDietStudy/> (need password)

