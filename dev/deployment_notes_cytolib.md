### Brief Note on De-Bugging Deployment with`cytolib` Package

Author: Stephanie Vartany

After a bunch of messing around with package versions, repos, etc. I appear to have fixed the package-related issues with `cmapR` and deployment of the app the R Studio Connect! Figured I should probably share what I did in case it is ever relevant again.

**The problem:** `cmapR` has a dependency to `flowCore` which is dependent on `cytolib`. This `cytolib` package has a lot of compilation issues. Most of the issues are related to the version of `cytolib` being somehow mismatched with the system's boost installation using the R package `BH`. Even though I could get installation to work locally on my mac (installed older version from binary), I was struggling to get it to work on R Studio Connect's server. And since I didn't have any real way of debugging the compilation issues that were popping up on the server, this was particularly challenging.

**The current solution:** I remembered that somehow I had deployed the multi-ome data viewer app also using `cmapR`, so some older version of `cytolib` and `BH` must have actually worked on the server. After a lot of playing around with this, I discovered that versions `BH_1.78.0-0` and `cytolib_2.10.0` (from BiocVersion_3.16.0) worked.....somehow? At least, it convinced the server to pull from a cached version of `cytolib`.

**How to actually do this:** On my own computer installing `BH_1.78.0-0` and then trying to install `cytolib_2.10.0` from the source doesn't actually work! So my work-around was to install `cytolib_2.10.0` using whatever I could get to work, and then uninstalling my newer version of `BH` and re-installing the older version `BH_1.78.0-0`. That made the R Studio Connect deployment pull those versions of the packages.
