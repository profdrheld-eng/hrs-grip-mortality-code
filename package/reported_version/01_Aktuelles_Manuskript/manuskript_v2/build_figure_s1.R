# Rebuild Figure S1 from the previously approved exact flow counts.
# This script uses aggregate counts only and does not read participant data or fit models.
script_arg <- sub('^--file=', '', grep('^--file=', commandArgs(), value=TRUE)[1])
package_dir <- dirname(normalizePath(script_arg))
project_dir <- normalizePath(file.path(package_dir, '../..'))
source(file.path(project_dir, '02_Analyse/scripts/hrs_paper_figures.R'))

counts <- c(valid_grip_pairs=6485L, tracker_matched=6484L,
            age_and_overlap_eligible=6339L, ordered_interview_months=6339L,
            followup_eligible=6316L, analyzed=6307L)
plot_data <- data.frame(grip10=20, grip14=19)
agreement <- data.frame(statistic=c('bias', 'lower_loa', 'upper_loa'),
                        estimate=c(0, 0, 0))
pages <- paper_pages(plot_data, 0.1, 0.2, list(), agreement, counts)

asset_dir <- file.path(package_dir, 'assets')
dir.create(asset_dir, recursive=TRUE, showWarnings=FALSE)
png(file.path(asset_dir, 'Figure_S1.png'), width=4500, height=3000,
    res=600, type='cairo', bg='white')
paper_style()
pages$S01_cohort_flow()
dev.off()
