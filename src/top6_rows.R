library(ProjectTemplate)
load.project()

for (dataset in project.info$data)
{
  message(paste('Showing top 5 rows of', dataset))
  print(head(get(dataset)))
}

head(train_hp_preprocessed[, namesTrain_id])
head(train_hp_preprocessed[, namesTrain_fac])
head(train_hp_preprocessed[, namesTrain_date])
head(train_hp_preprocessed[, namesTrain_char])
head(train_hp_preprocessed[, namesTrain_ind])
head(train_hp_preprocessed[, namesTrain_int])
head(train_hp_preprocessed[, namesTrain_num])
