% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RVI.R
\name{align_with_index}
\alias{align_with_index}
\title{Align an object with a target index
使对象与目标索引对齐}
\usage{
align_with_index(x, target_index, fill_value = NA)
}
\arguments{
\item{x}{The input object (numeric vector or xts).
输入对象(数值向量或xts)。}

\item{target_index}{The target index to align with.
要对齐的目标索引。}

\item{fill_value}{The value to use for filling missing positions, default is NA.
用于填充缺失位置的值，默认值NA。}
}
\value{
An xts object aligned with the target index.
        与目标索引对齐的xts对象。
}
\description{
Ensures that the input object is aligned with the specified target index,
filling missing values with NA if necessary.
确保输入对象与指定的目标索引对齐，必要时用NA填充缺失值。
}
\keyword{internal}
