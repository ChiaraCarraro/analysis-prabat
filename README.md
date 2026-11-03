# PraBat: An item response theory-based test battery to assess individual differences in pragmatic abilities in preschool children from different cultural backgrounds.

Data and analysis scripts associated with the following paper:  Carraro, C., Waffen, P., Küçükerdoğan, P., Yiğit, E. A., Odedra, M. J., Zeidler, H., Küntay, A., Haun D., & Bohn M., (2026). PraBat: An item response theory-based test battery to assess individual differences in pragmatic abilities in preschool children from different cultural backgrounds. (link to preprint: https://osf.io/preprints/psyarxiv/db6u4_v1)

Link to task (constantly updated): https://devpsy.web.leuphana.de/prabat

### Structure

```

├── data                                <-- data files
    ├── data_4IRT.csv                   <-- data from Germany used for item selection
    ├── data_kenya.csv                  <-- pilot data from Kenya
    ├── data_reliability.csv            <-- data collected for reliability study
    ├── data_turkiye.csv                <-- pilot data from Türkiye
    ├── data_validation.csv             <-- data from the item-selection dataset of the children who also completed the vocabulary test
    └── orev_data.csv                   <-- oREV data (vocabulary task for validation)
├── figures                             <-- figures in paper
├── paper                               <-- reproducible manuscript file
├── saves                               <-- saved model outputs
└──scripts
    ├── analysis.Rmd                    <-- all analysis code, including item selection and validity study
    ├── selected_model_comparison.R     <-- script for model comparison
    ├── simulated_annealing.R           <-- simulated annealing script (called in selected_model_comparison.R)
    └── irt2PL.R                        <-- script for 2PL IRT model

```
