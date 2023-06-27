# Shiny Modules Hierarchy

## Shiny Modules

Module names in **bold** below are `R/shiny*.R`.
Modules are tested in `inst/shinyTest/app*.R`.
Functions with `()` are shiny reactives.
Arguments as `input$*` and shiny inputs.

- **TraitSolos**
- **TraitPairs**
- **Correlation**
- **Volcano**
- **Effects**
- **Times**
- **Modules**

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
