This release is mostly about IgBLAST parsing improvements. It's based on 0.9.3 and adds more tools to work with the parsed IgBLAST data. There are also some code rearrangements, so it's not source-compatible with 0.9.3. See #60.

* Removed extra `clonotypes` namespace
* Added `readsCount` field to `Clonotype` which is calculated during parsing
* Added `onlyProductive` filtering
* Added `Totals` summary parsing
* Added DSV/TSV headers
