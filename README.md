# Generating Monthly Paleostreamflow Reconstructions

This repository contains code designed to disaggregate and reconstruct monthly streamflows directly from annual streamflow reconstructions, reconstructions of global circulation, and potential differences among regional tree-ring chronologies related to tree species and geographic location. It is presented here to replicate the results in [Stagge et al. (2017)](http://) and to provide a reference for others wishing to use these methods to reconstruct streamflow records.  

An subset of the reconstruction is shown below as an example. Users can interact with the entire reconstruction at the [Monthly Paleostreamflow Explorer](https://jstagge.shinyapps.io/paleo_flow)

<figure>
 <img src="reconst_example.png" alt="Monthly reconstruction example" />
 <figcaption>
 Reconstructed flows at the Logan river site for subsets of the (a) historical and (b) observed periods. Reproduces from Stagge et al. (2017).
 </figcaption>
</figure>

## Getting Started

These instructions will allow you to fit monthly streamflow reconstruction models on your local machine for testing purposes. All code is written in R. See Prerequisites and Running sections below for detailed instructions.

### Prerequisites

In order to run this code, you must install:
* [R for Statistical Computing](https://www.r-project.org/)

All necesary R packages will be installed automatically in the first file.

### Running the Code (All Scripts)

Code is numbered in order of operations.  If you would like to simply recreate the results of [Stagge et al. (2017)](http://), you may run the following from any command line after installing R. For more detailed information about each file, see below:

```
Rscript 00_prepare_file_system.R
Rscript 01_process_streamflows.R
Rscript 02_mf_model.R
Rscript 03_ap_model_fit.R
Rscript 04_ap_model_reconstruct.R
Rscript 05_apr_model_fit.R
Rscript 06_apr_model_reconstruct.R
Rscript 07_pca_tree_ring.R
Rscript 08_apr_model_predictors_fit.R
Rscript 09_apr_model_predictors_reconstruct.R
Rscript 10_plot_gof_results.R
```

### Running the Code (Step by Step)
The following file prepares the file system, installing any necesary packages and creating folders for model output.

```
Rscript 00_prepare_file_system.R
```
The next script downloads and processes USGS streamflow for the relevant sites. In this case, the relevant stream gauges are 10109001 and 10011500, located on the Logan and Bear rivers of Utah, respectively.
```
Rscript 01_process_streamflows.R
```
The following scripts each fit a model described in [Stagge et al. (2017)](http://) and then reconstruct flow. For all models, except the MF model, these steps are separated into "_fitting" and "_reconstruct" files.
```
Rscript 02_mf_model.R
Rscript 03_ap_model_fit.R
Rscript 04_ap_model_reconstruct.R
Rscript 05_apr_model_fit.R
Rscript 06_apr_model_reconstruct.R
```
The following files run a PCA analysis on regional tree-ring chronologies and then use these, along with global circulation indices as predictors. File naming follows the same sceme:
```
Rscript 07_pca_tree_ring.R
Rscript 08_apr_model_predictors_fit.R
Rscript 09_apr_model_predictors_reconstruct.R
```
Finally, a series of goodness of fit tests are run, in addition to several plots used to validate the model results:
```
Rscript 10_plot_gof_results.R
```

## Reference and How to Cite

For any description of this methodology, please use the following citation:

* Stagge, J.H., Rosenberg, D.E., DeRose, R.J., and Rittenour, T.M. (2017) "Monthly paleostream-flow reconstruction from annual tree-ring chronologies." Journal of Hydrology.

For any use of this code, please cite the above paper and the following:

* I will apply to zenodo to get a DOI for this.

## Authors

* **James H. Stagge** - *Owner* - [jstagge](https://github.com/jstagge)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Thank you to Justin DeRose for providing several tree-ring chronologies.
* Additional thanks to the International Tree-Ring Data Bank for providing further chronologies and global climate reconstructions. 