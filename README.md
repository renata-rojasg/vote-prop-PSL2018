# 📊 Vote proportions in the Brazilian presidential elections in 2018

## 📁 About This Repository
This repository contains the R implementations and data sets used in the paper entitled "Unit regression models to explain vote proportions in the Brazilian presidential elections in 2018," published at the Revista Colombiana de Estadística, 47(2), 2024.

## 🔧 Additional Code Base

Some of the unit regression models used in this analysis rely on custom distribution implementations. These were developed based on code provided in the [UnitDistsForGAMLSS](https://github.com/renata-rojasg/UnitDistsForGAMLSS) GitHub repository. It contains R functions that extend the `gamlss.family` to include unit distributions not currently available in the official GAMLSS package. The unit gamma and unit Lindley were fitted using this repository.

**Note:** The paper relies on a previous release of the UnitDistForGAMLSS, which was deployed at [`figshare`](https://figshare.com/articles/software/UnitDistForGAMLSS/25328575/1) and has its own DOI. Later versions of the repository include additional distributions and provide documentation of the implemented functions.

## 🔗 Data Availability

The data supporting this research is publicly available and their sources are described in the paper. The final data set is provided here at the [`data_elections_lat_long.csv`](data_elections_lat_long.csv) file. 

## 📚 References

If you use this repository or its contents, please cite:

@article{guerra2024unit,
  title={Unit regression models to explain vote proportions in the Brazilian presidential elections in 2018},
  author={Guerra, Renata Rojas and Pe{\~n}a-Ram{\'\i}rez, Fernando Arturo and Ribeiro, Tatiane Fontana and Cordeiro, Gauss Moutinho and Mafalda, Charles},
  journal={Revista Colombiana de Estad{\'\i}stica},
  volume={47},
  number={2},
  pages={},
  year={2024}
}

If you use the later version of the [UnitDistsForGAMLSS](https://github.com/renata-rojasg/UnitDistsForGAMLSS) repository or its contents, please cite:

@misc{Guerra2025,
  author = {Guerra, Renata Rojas},
  title = {UnitDistForGAMLSS},
  year = {2025},
  note = {Available online: \url{https://github.com/renata-rojasg/UnitDistForGAMLSS} (released on 7 May 2025). Repository: \textit{Figshare}. DOI: \href{https://doi.org/10.6084/m9.figshare.25328575.v2}{10.6084/m9.figshare.25328575.v2}},
}

