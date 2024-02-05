# Shiny Modules Hierarchy

## Shiny Modules

This shiny app [app.R](app.R) is composed of shiny module, in the package as `R/shiny*.R`.
Modules are tested in `inst/shinyTest/app*.R`.
Functions with `()` are shiny reactives.
Arguments as `input$*` and shiny inputs.

- [TraitPanel](appTraitPanel.R)
  - [TraitOrder](appTraitOrder.R)
  - [TraitNames](appTraitNames.R)
  - [CorTable](appCorTable.R)
  - [CorPlot](appCorPlot.R)
  - [TraitTable](appTraitTableNames.R)
  - [TraitSolos](appTraitSolosNames.R)
  - [TraitPairs](appTraitPairsNames.R)
  - Downloads
- [TimePanel](appTraitPanel.R)
  - [TimeTable](appTimeTable.R)
    - [TraitOrder](appTraitOrder.R)
    - [TimeTraits](appTimeTraits.R)
  - [TimePlot](appTimePlot.R)
    - Downloads
- [StatsPanel](appStatsPanel.R)
  - [ContrastPlot](appContrastPlot.R)
    - Downloads
- [ContrastPanel](appContrastPanel.R)
  - [ContrastTable](appContrastTable.R)
  - [ContrastSex](appContrastSex.R)
    - [ContrastPlot](appContrastPlot.R)
      - Downloads
  - [ContrastTime](appContrastTime.R);
    - [TimeTraits](appTimeTraits.R)
  - [TimePlot](appTimePlot.R)
    - Downloads
  - [ContrastModule](appContrastModule.R)
    - [ContrastPlot](appContrastPlot.R)
      - Downloads
      
ContrastPanel calls ContrastTime and TimePlot, and seem to be blocked.
Seems to work OK for TimePanel with TimeTable and TimePlot.

### Helper Modules

- **TraitNames**
- traitStatsArranged() probably should be a module **TraitStats**
- data input (see below)

## Code Dependencies

- **TraitNames**
  + traitDataSelectType()
  + traitNamesArranged()
  + datasets_selected()
  + input$response
  + input$order
- traitNamesArranged()
  + traitStatsArranged()

- traitStatsArranged() probably should be a module **TraitStats**
  + traitStatsSelectType()
  + traitSignalSelectType()
  + datasets_selected()
  + input$order

- **TraitSolos**
  + traitDataSelectType()
  + traitSignalSelectType()
  + trait_selection()
  + input$facet
  + input$butresp
  + input$strains

- **TraitPairs**
  + traitDataSelectTrait()
  + trait_selection()
  + input$pair
  + input$facet

- **Volcano**
  + traitStatsSelectType()
  + traitStatsArranged()

- **Effects**
  + traitStatsSelectType()
  + trait_selection()
  + corobject()
  
- **Correlation**
  + traitStatsSelectType()
  + trait_selection()
  + input$corterm

### Data Input

Data are input via `traitdata`, `traitsignal` and `traitstats`, which
could be entered as an list object (to `traitdata`) with elements
`Data`, `Signal`, `Stats`, and possibly additional items (say for `Modules`).
These are parsed in `trait*Param()` and then joined with `newtraitdata()`
in the trait*Input()` reactives. 
All of this might later become its own shiny helper module.

Data input uses the optional `customSettings` list,
which contains the following elements:

- help: markdown for About tab
- dataset: data frame with short and long dataset names
- condition: name to use for `condition` column (e.g. "diet")

The `customSettings` element `dataset` is used in various places
for naming things. This could be more coherent.

- traitDataSelectType()
  + traitDataInput()
  + datasets_selected()
- traitStatsSelectType()
  + traitStatsInput()
  + datasets_selected()
- traitSignalSelectType()
  + traitSignalInput()
  + datasets_selected()

- traitDataSelectTrait()
  + **TraitSolos**
  
The `traitStatsArranged()` reactive is used to construct the
`traitNamesArranges()` reactive used in the **TraitNames** module.
It is also used for **Volcano** output.

- traitStatsArranged()
  + traitStatsSelectType()
  + traitSignalSelectType()
  + datasets_selected()
  
### Trait and Dataset Selection

Trait selection occurs in the **TraitNames** module.
This vector of trait names is used in multiple places.
The `datasets()` is the list of all datasets, while
`datasets_selected()` is the subset of datasets selected.
  
- trait_selection()
  + **TraitNames**
- datasets_selected()
  + input$dataset
  + customSettings$dataset
- datasets()
  + traitDataInput()
  + customSettings$dataset

This is a bit tricky taking care of the situation where
traits are ordered by correlation. For all other situations,
the ordering can be done without regard to the trait_selection().
However, when ordering by correlation, one uses either the first trait (planned)
or all selected traits (current) to determine the order of possible next
traits. Further, one might want to pick one trait from one dataset as
proband, and then pick other traits from a different dataset(s).
That is not easy right now, as there is no process (yet) to keep the
part of the dataset for proband in place. There is some code in bestcor
for part of this. I may need to rethink this. Makes sense in terms of what
researchers are asking for.

A slight variant on the current shinyTraitNames can be used with option
on `multiple` as FALSE for proband might work.
Need to see if traitStatsArranged() needs to have all terms or only
the cellmean/signal term. It is used in Volcano.
Also need to see what to pass for traitStatsArranged for proband;
probably want a version of traitStatsArranged() without correlation.