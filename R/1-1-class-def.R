#################################
#### TrtDataObj class
#' An S4 class containing treatment, covariates and response
#'
#' @slot trtResp A numeric vector, the response under assigned treatment
#' @slot trtLabl A factor vector, the assigned treatment label
#' @slot X A numeric matrix. Containing all the variables.
#' @slot trtLevl A character vector of length two, two treatment labels
#' @slot trtTrue A factor vector, true the better treatment
#' @slot sample.weight A numeric vector, indicating the weight of a sample in performance evaluation
#' @slot sample.inclsn A logical vector, indicating whether a sample is included in performance evaluation
#'
#' @seealso
#' \code{\link{TrtDataObj-class}}\cr
#' \code{\link{TrtDataIdeal-class}}\cr
#' \code{\link{wClsObj-class}}\cr
#' \code{\link{RegObj-class}}\cr
#' \code{\link{ModelObj-class}}\cr
#' \code{\link{ModelEnsembleObj-class}}\cr
#' \code{\link{cvGrid-class}}\cr
#' \code{\link{cvKfold-class}}\cr
#' \code{\link{workflow-class}}\cr
#' @export
setClass(
  "TrtDataObj",
  slots = list(
    trtResp = "numeric",
    trtLabl = "factor",
    X = "data.frame",
    trtLevl = "character",
    trtTrue = "ANY",
    sample.weight = "numeric",
    sample.inclsn = "logical"
  )
)

#################################
#### TrtDataIdeal class
#' An S4 class, an ideal dataset that contains covariates and responses under both treatments.
#' It can be used to generate a \code{TrtDataObj} randomly.
#'
#' @slot trtResp1 A numeric vector, the response under the first treatment
#' @slot trtResp2 A numeric vector, the response under the second treatment
#' @slot X A numeric matrix. Containing all the variables.
#' @slot label A character vector of length two, two treatment labels.
#' The first one corresponds to \code{trtResp1} and the second one corresponds to \code{trtResp2}.
#'
#' @seealso
#' \code{\link{TrtDataIdeal-class}}\cr
#' \code{\link{TrtDataObj-class}}\cr
#' \code{\link{generateTrtData}}\cr
#' @export
setClass(
  "TrtDataIdeal",
  slots = list(
    trtResp1 = "numeric",
    trtResp2 = "numeric",
    X = "data.frame",
    label = "character"
  )
)

#################################
#### wClsObj class
#' An S4 class, a data object to conduct a weighted classification.
#'
#' @slot weight A numeric vector, the sample weight
#' @slot Y A factor vector, the response label
#' @slot X A numeric matrix. Containing all the variables.
#' @slot Y.lvl A character vector of length two, two response labels.
#'
#' @seealso
#' \code{\link{wclskSVM}}\cr
#' \code{\link{wclslSVM}}\cr
#' \code{\link{wclsLASSO}}\cr
#' \code{\link{wclsANN}}\cr
#' \code{\link{wclsDNN}}\cr
#' @export
setClass(
  "wClsObj",
  slots = list(
    weight = "numeric",
    Y = "factor",
    X = "data.frame",
    Y.lvl = "character" # ,
    # Y.true = "factor"
  )
)

#################################
#### RegObj class
#' An S4 class, a data object to conduct a regression.
#'
#' @slot Y A numeric vector, the response
#' @slot X A numeric matrix. Containing all the variables.
#'
#' @seealso
#' \code{\link{regLASSO}}\cr
#' \code{\link{regSVR}}\cr
#' \code{\link{regRF}}\cr
#' \code{\link{regANN}}\cr
#' \code{\link{regDNN}}\cr
#' @export
setClass(
  "RegObj",
  slots = list(
    Y = "numeric",
    X = "data.frame"
  )
)

#################################
#### ITRObj class
#' An S4 class, an individualized treatment rule object
#'
#' @slot label A character vector of length two, treatment labels
#' @slot model A list of regression or classification models related to the indivifualized treatment rule.
#' @slot model.type Type of individualized treatment rule, including \code{itrDOWL}, \code{itrOWL}, \code{itrVT}, \code{itrSimple}
#'
#' @seealso
#' \code{\link{TrtDataObj-class}}\cr
#' \code{\link{itrDOWL}}\cr
#' \code{\link{itrOWL}}\cr
#' \code{\link{itrVT}}\cr
#' \code{\link{itrSimple}}\cr
#' @export
setClass(
  "ITRObj",
  slots = list(
    label = "character",
    model = "ANY",
    model.type = "character"
  )
)

#################################
#### ModelObj class
#' An S4 class, a regression or classification model object
#'
#' @slot model A regression or classification model
#' @slot model.type Type of the model, either "Continuous" or "Binary"
#'
#' @seealso
#' \code{\link{wClsObj-class}}\cr
#' \code{\link{RegObj-class}}\cr
#' \code{\link{wclskSVM}}\cr
#' \code{\link{wclslSVM}}\cr
#' \code{\link{wclsLASSO}}\cr
#' \code{\link{wclsANN}}\cr
#' \code{\link{wclsDNN}}\cr
#' \code{\link{regLASSO}}\cr
#' \code{\link{regSVR}}\cr
#' \code{\link{regRF}}\cr
#' \code{\link{regANN}}\cr
#' \code{\link{regDNN}}\cr
#' \code{\link{ModelEnsembleObj-class}}\cr
#' @export
setClass(
  "ModelObj",
  slots = list(
    model = "ANY",
    model.type = "character"
  )
)

#################################
#### ModelEnsembleObj class
#' An S4 class, an ensemble model object
#'
#' @slot model.list A list of regression or classification models in the ensemble
#' @slot model.type Type of the model, either "Continuous" or "Binary"
#' @slot loss A numeric vector, the loss of each model
#' @slot keep A numeric vector or matrix, indicating whether each model is included when aggregating the predictions
#'
#' @seealso
#' \code{\link{wClsObj-class}}\cr
#' \code{\link{RegObj-class}}\cr
#' \code{\link{ModelObj-class}}\cr
#' @export
setClass("ModelEnsembleObj",
         slots = list(
           model.list = "ANY",
           model.type = "character",
           loss = "numeric",
           keep = "ANY"
         )
)

#################################
#### PredObj class
#' An S4 class containing the prediction
#'
#' @slot pred Prediction of new data from some model
#' @slot v.score Potential variable importance score, not appicable in this version
#'
#' @seealso
#' \code{\link{ModelObj-class}}\cr
#' \code{\link{ModelEnsembleObj-class}}\cr
#' \code{\link{ITRObj-class}}\cr
#' \code{\link{cvGrid-class}}\cr
#' \code{\link{cvKfold-class}}\cr
#' \code{\link{workflow-class}}\cr
#' @export
setClass("PredObj",
         slots = list(
           pred = "ANY",
           v.score = "ANY"
         )
)

#' An S4 class containing the prediction
#'
#' An \code{PredObj-class} sub-class for model dealing with a continuous response.
#' @export
setClass("PredContinuousObj",
         contains = "PredObj"
)

#' An S4 class containing the prediction
#'
#' An \code{PredObj-class} sub-class for model dealing with a discrete response.
#' @slot prob The matrix for estimated probability for each class.
#' @export
setClass("PredDiscreteObj",
         contains = "PredObj",
         slots = list(
           prob = "ANY"
         )
)

#################################
#### ListPredObj class
#' An S4 class containing the prediction
#'
#' @slot listPred A list of \code{PredObj} objects.
#'
#' @seealso
#' \code{\link{PredObj-class}}\cr
#' @export
setClass("ListPredObj",
         slots = list(
           listPred = "list"
         )
)

#################################
#### cvGrid class
#' An S4 class containing the grid search results
#'
#' @slot best.model The best model fitted with the grid that has an optimal performance
#' @slot performances The performance table for all grids
#' @slot cutoff.list Cutoff estimated fro all grids
#' @slot models A list of models for all grids
#'
#' @seealso
#' \code{\link{ModelObj-class}}\cr
#' \code{\link{ModelEnsembleObj-class}}\cr
#' \code{\link{ITRObj-class}}\cr
#' \code{\link{PredObj-class}}\cr
#' \code{\link{ListPredObj-class}}\cr
#' \code{\link{cvGrid-class}}\cr
#' \code{\link{cvKfold-class}}\cr
#' \code{\link{workflow-class}}\cr
#' @export
setClass("cvGrid",
         slots = list(
           best.model = "ANY",
           performances = "data.frame",
           cutoff.list = "ANY",
           models = "ANY"
         )
)

#################################
#### cvKfold class
#' An S4 class containing the K-fold criss validation results
#'
#' @slot pred.table A table for all cross-validation predictions from all models
#' @slot prob.table An array with all estimated probability tables
#' @slot models The final models returned
#' @slot cutoff.list A list of cutoff values
#' @slot performances The performance table for all models
#' @slot v.score A table of variables scores averaged on all k folds from all models
#' @slot pred.error A table of corss-validation prediction error for all models
#'
#' @seealso
#' \code{\link{ModelObj-class}}\cr
#' \code{\link{ModelEnsembleObj-class}}\cr
#' \code{\link{ITRObj-class}}\cr
#' \code{\link{PredObj-class}}\cr
#' \code{\link{ListPredObj-class}}\cr
#' \code{\link{cvGrid-class}}\cr
#' \code{\link{cvKfold-class}}\cr
#' \code{\link{workflow-class}}\cr
#' @export
setClass("cvKfold",
         slots = list(
           pred.table = "matrix",
           prob.table = "array",
           models = "ANY",
           cutoff.list = "ANY",
           performances = "data.frame",
           v.score = "ANY",
           pred.error = "ANY"
         )
)

#################################
#### workflow class
#' An S4 class containing the workflow results
#'
#' @slot pred.array An array for all iterations of all cross-validation predictions from all models
#' @slot prob.array An array with all estimated probability tables
#' @slot performance.array An array storing the performance score for all iterations
#' @slot performances The aggregated performance table for all models
#' @slot cutoff.list A list of cutoff values
#' @slot v.score A table of variables scores averaged on all k folds from all models
#' @slot pred.error A table of corss-validation prediction error for all models
#' @slot models The final models returned
#'
#' @seealso
#' \code{\link{ModelObj-class}}\cr
#' \code{\link{ModelEnsembleObj-class}}\cr
#' \code{\link{ITRObj-class}}\cr
#' \code{\link{PredObj-class}}\cr
#' \code{\link{ListPredObj-class}}\cr
#' \code{\link{cvGrid-class}}\cr
#' \code{\link{cvKfold-class}}\cr
#' \code{\link{workflow-class}}\cr
#' @export
setClass("workflow",
         slots = list(
           pred.array = "array",
           prob.array = "array",
           performance.array = "array",
           performances = "data.frame",
           cutoff.list = "ANY",
           v.score = "ANY",
           pred.error = "ANY",
           models = "ANY"
         )
)
