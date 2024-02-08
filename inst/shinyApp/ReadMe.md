# Shiny Modules Hierarchy

## Shiny Modules

This shiny app [app.R](app.R) is composed of shiny module, in the package as `R/shiny*.R`.
Modules are tested in `inst/shinyTest/app*.R`.
Functions with `()` are shiny reactives.
Arguments as `input$*` and shiny inputs.

- [TraitPanel](appTraitPanel.R)
  - `input$strains`
  - `input$facet`
  - [TraitOrder](appTraitOrder.R)
  - [TraitNames](appTraitNames.R)
  - [CorTable](appCorTable.R)
  - [CorPlot](appCorPlot.R)
  - [TraitTable](appTraitTableNames.R)
    - `panel_par$strains`
  - [TraitSolos](appTraitSolosNames.R)
  - [TraitPairs](appTraitPairsNames.R)
  - Downloads
- [TimePanel](appTraitPanel.R)
  - `input$strains`
  - `input$facet`
  - [TimeTable](appTimeTable.R)
    - [TraitOrder](appTraitOrder.R)
    - [TimeTraits](appTimeTraits.R)
  - [TimePlot](appTimePlot.R)
    - Downloads
- [StatsPanel](appStatsPanel.R)
  - [ContrastPlot](appContrastPlot.R)
    - Downloads
- [ContrastPanel](appContrastPanel.R)
  - `input$contrast`: Sex, Time, Module
  - `input$strains`
  - `input$facet`
  - contrastOutput <- [ContrastTable](appContrastTable.R)
    - [TraitOrder](appTraitOrder.R)
      - `input$order`
      - `main_par$dataset`
  - [ContrastSex](appContrastSex.R) <- contrastOutput
    - [ContrastPlot](appContrastPlot.R)
      - `input$butshow`: Plots, Tables
      - `input$interact`
      - `input$ordername`: Order by
      - `input$rownames`
      - Downloads
  - contrastTimeOutput <- [ContrastTable](appContrastTable.R)
    - `panel_par$strains`
    - [TraitOrder](appTraitOrder.R)
      - `input$order`
      - `main_par$dataset`
  - timeOutput <- [ContrastTime](appContrastTime.R) <- contrastTimeOutput
    - `panel_par$strains`
    - [TimeTraits](appTimeTraits.R)
      - `input$time`
      - `input$response`
      - `input$traits`
      - `main_par$tabpanel`
      - `panel_par$contrast`
  -  [TimePlot](appTimePlot.R) <- timeOutput
    - `input$butshow`
    - `input$buttable`
    - `main_par$height`
    - `panel_par$strains`
    - `panel_par$facet`
    - Downloads
  - [ContrastModule](appContrastModule.R) <- contrastOutput
    - [ContrastPlot](appContrastPlot.R)
      - `input$butshow`: Plots, Tables
      - `input$interact`
      - `input$ordername`: Order by
      - `input$rownames`
      - Downloads
      
ContrastPanel calls ContrastTime and TimePlot, and seem to be blocked.
Seems to work OK for TimePanel with TimeTable and TimePlot.
Need to put input$strains and input$facet in ContrastPanel to match TimePanel.

I seem to have fixed this problem. However, ContrastPanel subpanels Sex and Module rely on strains and facet within them. This may connect to ContrastPlot so watch out.

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