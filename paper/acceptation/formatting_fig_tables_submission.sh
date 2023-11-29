
mkdir -p figures
mkdir -p extended_data

# Figure 1-3
cp ../fig_paper/p_trends.pdf figures/Danet_Fig1.pdf
cp ../fig_paper/clust_figure.pdf figures/Danet_Fig2.pdf
cp ../fig_paper/p_effect.pdf figures/Danet_Fig3.pdf

# Table (included in the main text)

# Extended data
cp ../fig_paper/timeseries_dist.pdf extended_data/Danet_ED_Fig1.pdf
cp ../fig_paper/p_site_trends_ts_caracteristics.pdf extended_data/Danet_ED_Fig2.pdf
cp ../fig_paper/p_trends_supp.pdf extended_data/Danet_ED_Fig3.pdf
cp ../fig_paper/clust_figure_na.pdf extended_data/Danet_ED_Fig4.pdf
cp ../fig_paper/p_pred_hft.pdf extended_data/Danet_ED_Fig5.pdf
cp ../fig_paper/p_drivers_ts_caracteristic.pdf extended_data/Danet_ED_Fig6.pdf
cp ../fig_paper/p_pca_riv_str.pdf extended_data/Danet_ED_Fig7.pdf
cp ../fig_paper/p_effect_supp.pdf extended_data/Danet_ED_Fig8.pdf
cp ../fig_paper/tab_rand_extended_data_table1.jpg extended_data/Danet_ED_Table1.jpg

# Supplementary Information
cp ../supplementary_figures.pdf Danet_SI.pdf

# Supplementary Data
zip -r Danet_Supplementary_Software_1.zip ../fig_paper/SupplementaryMap

# Reporting summary
cp nr-reporting-summary_filed.pdf Danet_RS.pdf

# Zip figures and table folder for NEE upload
zip -r figures.zip figures
zip -r extended_data.zip extended_data
