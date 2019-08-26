# Preamble =============================================================
memory.size(2^17)
options(java.parameters = "-Xmx80g")

# Packages -------------------------------------------------------------
# Tools
library(tidyverse)
library(SparseM)
library(slam)
library(here)
library(ROCR)
library(Matrix)

# Text management
library(RWeka)
library(SnowballC)
library(tm)

# Machine Learning
library(caret)
library(caTools)
library(glmnet)
library(maxent)


# Utilities ------------------------------------------------------------
here('R') %>%
    list.files('\\.R$', full.names = TRUE) %>%
    walk(source)


# Global variables -----------------------------------------------------
pathdata   <- here("../data")
set.seed(1234)
stat_used <- list(
    acc = 'acc', sens = 'sens', spec = 'spec',
    ppv = 'ppv', npv = 'npv', f1 = 'f'
)





# Data Import ==========================================================
load(file.path(pathdata, "varicella_data.rda"))

## Data imported are:
# varic_train, positive_gold_train                              # Veneto
# varic_vali,  positive_gold_vali                              # Sicilia





# Pre-processing =======================================================
corpus_train_pp <- preproc(varic_train)
corpus_vali_pp  <- preproc(varic_vali)





# DTM ==================================================================
# Train ----------------------------------------------------------------
dtm_train_tf <- atom_dtm(corpus_train_pp) %>%
    removeSparseTerms(.99)

dtm_train  <- tfidf4stm(dtm_train_tf)
dns_train  <- stm_dns(dtm_train)
mtrx_train <- Matrix(dns_train, sparse = TRUE)
csr_train  <- as.matrix.csr_train(dns_train)

classes_train <- as.integer(
    dtm_train$dimnames$Docs %in%
        as.character(positive_gold_train$nPaz)
)

classes_train_table <- table(classes_train)
classes_train_table


# Validation -----------------------------------------------------------
dtm_adjust_vali <- atom_dtm(corpus_vali_pp) %>%
    as.DocumentTermMatrix(weighting = weightTf) %>%
    dtm_lfilter(dtm_train) %>%
    reweights_test(dtm_train_tf)

dns_validation  <- stm_dns(dtm_adjust_vali)
mtrx_validation <- Matrix(dns_validation, sparse = TRUE)
csr_validation  <- as.matrix.csr(dns_validation)

classes_validation <- as.integer(
    dtm_adjust_vali$dimnames$Docs %in%
        as.character(positive_gold_vali$nPaz)
)

table(classes_validation)




# Training =============================================================
# Setup ----------------------------------------------------------------
wt <- (
        (classes_train_table[1] * classes_train) +
        (classes_train_table[2] * (1 - classes_train))
    ) /
    sum(classes_train_table)

train_indeces <- createFolds(classes_train, k = 5, returnTrain = TRUE)


# LogitBoost -----------------------------------------------------------
model_lb <- map(train_indeces,
    ~ LogitBoost(dns_train[., ], classes_train[.])
)

pred_lb_train <- map2(model_lb, train_indeces,
    ~ predict(.x, dns_train[-.y, ])
)

perf_lb_train <- cv_perf(
    map2(pred_lb_train, train_indeces,
        ~ prediction(.x, classes_train[-.y])
    ),
    stat_used
)


# GLMNet ---------------------------------------------------------------
model_glm <- map(train_indeces,
    ~ glmnet(mtrx_train[., ], classes_train[.], weights = wt[.])
)

pred_glm_train <- map2(model_glm, train_indeces,
    ~ as.numeric(
        predict(.x, mtrx_train[-.y, ], s = 0.01, type = 'response') > .5
    )
)

perf_glm_train <- cv_perf(
    map2(pred_glm_train, train_indeces,
        ~ prediction(.x, classes_train[-.y])
    ),
    stat_used
)


# Maxent ---------------------------------------------------------------
model_maxent <- map(train_indeces,
    ~ maxent(csr_train[., ], classes_train[.])
)

pred_maxent_train <- map2(model_maxent, train_indeces,
    ~ predict(.x, csr_train[-.y, ])
)

perf_maxent_train <- cv_perf(
    map2(pred_maxent_train, train_indeces,
        ~ prediction(as.integer(.x[, 1]), classes_train[-.y])
    ),
    stat_used
)





# Validation ===========================================================
# LogitBoost -----------------------------------------------------------
pred_lb_validation <- map(model_lb, ~ predict(., dns_validation))

perf_lb_validation <- cv_perf(
    map(pred_lb_validation, ~ prediction(.x, classes_validation))
    , stat_used
)


# GLMNet --------------------------------------------------------------
pred_glm_validation <- map(model_glm,
    ~ as.numeric(
        predict(., mtrx_validation, s = 0.01, type = 'response') > .5
    )
)

perf_glm_validation <- cv_perf(
    map(pred_glm_validation, ~ prediction(.x, classes_validation)),
    stat_used
)


# Maxent --------------------------------------------------------------
pred_maxent_validation <- map(model_maxent,
    ~ predict(., csr_validation)
)

perf_maxent_validation <- cv_perf(
    map(pred_maxent_validation,
        ~ prediction(as.integer(.x[, 1L]), classes_validation)
    ),
    stat_used
)






# Results ==============================================================
# Training -------------------------------------------------------------
perf_train <- list(
    glmnet = perf_glm_train,
    maxent = perf_maxent_train,
    lb = perf_lb_train
)
map(perf_train, ci)

agreement(pred_glm_train,    pred_maxent_train, classes_train)
agreement(pred_glm_train,    pred_lb_train,     classes_train)
agreement(pred_maxent_train, pred_lb_train,     classes_train)


# Validation -----------------------------------------------------------
perf_validation <- list(
    glmnet = perf_glm_validation,
    maxent = perf_maxent_validation,
    lb = perf_lb_validation
)
map(perf_validation, ci)

agreement(pred_lb_validation, pred_glm_validation,
    classes_validation
)
agreement(pred_lb_validation, pred_maxent_validation,
    classes_validation
)
agreement(pred_glm_validation, pred_maxent_validation,
    classes_validation
)

