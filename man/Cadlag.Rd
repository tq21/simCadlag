% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Cadlag.R
\name{Cadlag}
\alias{Cadlag}
\title{Cadlag function class}
\description{
An `R6` class for randomly generating a Cadlag function.
Currently the class is designed to generate a Cadlag function of the form
finite linear combination of 0-order basis functions, that is, indicator
functions.
}
\examples{
set.seed(123)
random_function <- Cadlag$new(n_vars = 3)
random_function$gen_formula()
df <- random_function$gen_samples(500)
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{order}}{Order of each basis function.}

\item{\code{total_var_norm}}{Total variation norm of the function.}

\item{\code{formula}}{Formula of the function.}

\item{\code{n_vars}}{Number of variables in the function.}

\item{\code{coefs}}{Coefficients of the basis functions.}

\item{\code{rv_list}}{RV (random variable) objects.}

\item{\code{param_list}}{Parameters for RV objects.}

\item{\code{basis_list}}{Basis functions.}

\item{\code{max_degree}}{Maximum degree of interaction of the basis functions.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Cadlag-new}{\code{Cadlag$new()}}
\item \href{#method-Cadlag-gen_param_list}{\code{Cadlag$gen_param_list()}}
\item \href{#method-Cadlag-gen_rv_list}{\code{Cadlag$gen_rv_list()}}
\item \href{#method-Cadlag-gen_coefs}{\code{Cadlag$gen_coefs()}}
\item \href{#method-Cadlag-gen_formula}{\code{Cadlag$gen_formula()}}
\item \href{#method-Cadlag-gen_samples}{\code{Cadlag$gen_samples()}}
\item \href{#method-Cadlag-clone}{\code{Cadlag$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Cadlag-new"></a>}}
\if{latex}{\out{\hypertarget{method-Cadlag-new}{}}}
\subsection{Method \code{new()}}{
Initialize a new instance of the Cadlag class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cadlag$new(n_vars)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{n_vars}}{An integer for the number of variables in the function.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Cadlag-gen_param_list"></a>}}
\if{latex}{\out{\hypertarget{method-Cadlag-gen_param_list}{}}}
\subsection{Method \code{gen_param_list()}}{
Generate parameters for RV objects
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cadlag$gen_param_list(samp_dists)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{samp_dists}}{A character vector for the sampled distributions.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Cadlag-gen_rv_list"></a>}}
\if{latex}{\out{\hypertarget{method-Cadlag-gen_rv_list}{}}}
\subsection{Method \code{gen_rv_list()}}{
Generate a list of RV objects
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cadlag$gen_rv_list()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Cadlag-gen_coefs"></a>}}
\if{latex}{\out{\hypertarget{method-Cadlag-gen_coefs}{}}}
\subsection{Method \code{gen_coefs()}}{
Generate intercept and coefficients given the total variation norm
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cadlag$gen_coefs()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Cadlag-gen_formula"></a>}}
\if{latex}{\out{\hypertarget{method-Cadlag-gen_formula}{}}}
\subsection{Method \code{gen_formula()}}{
Generate a formula for Y ~ X
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cadlag$gen_formula()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Cadlag-gen_samples"></a>}}
\if{latex}{\out{\hypertarget{method-Cadlag-gen_samples}{}}}
\subsection{Method \code{gen_samples()}}{
Sample n copies of the data
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cadlag$gen_samples(n)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{n}}{An integer for the number of samples to generate.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Cadlag-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Cadlag-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cadlag$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
