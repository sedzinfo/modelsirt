##########################################################################################
# IRT MODELS
##########################################################################################
#' @title Explore the response curves of dichotomous, polytomous and multidimensional IRT models
#' @description Launches a Shiny app that draws the item response functions of the main item
#' response theory (IRT) models in interactive plots, so that you can see how each item
#' parameter shapes them: the probability of each possible response is plotted against the
#' latent trait and redrawn as soon as a parameter is changed. The app covers the Rasch model
#' and the one- to four-parameter logistic models for dichotomous items; the partial credit,
#' generalized partial credit, graded response, rating scale, generalized rating scale and
#' nominal response models for polytomous items; the probit response functions of Thurstonian
#' IRT; and the response surface, contour plot and information surface of a dichotomous item
#' that measures two abilities (multidimensional IRT).
#' @details Each tab plots, for a single item, the probability of each possible response as a
#' function of the person's position on the latent trait (ability), written
#' \eqn{\theta}{theta} (\eqn{\eta}{eta} in the TIRT tab). The basic building block is the
#' four-parameter logistic function
#' \deqn{P(X = 1 \mid \theta) = c + \frac{d - c}{1 + \exp(-D a (\theta - b))}}{P(X = 1 | theta) = c + (d - c) / (1 + exp(-D * a * (theta - b)))}
#' the probability of a correct (or keyed) response, \eqn{X = 1}, where \eqn{a} is the
#' discrimination (how steeply the probability rises with ability), \eqn{b} the difficulty
#' (the ability at which the probability is halfway between its lower and upper limits),
#' \eqn{c} the lower asymptote (the probability at very low abilities, as with guessing) and
#' \eqn{d} the upper asymptote (the probability at very high abilities). \eqn{D} is the
#' scaling constant: 1 keeps the logistic metric, and 1.702 makes the logistic curve almost
#' coincide with the normal ogive, the cumulative normal distribution. The Rasch-4PL tab plots
#' this function itself, the GRM tab uses it with \eqn{c = 0} and \eqn{d = 1} for the
#' boundaries between categories, and the MIRT Dichotomous tab extends it to two abilities;
#' the other models are given in their sections.
#'
#' The five tabs on a single trait, Rasch-4PL to NRM, share a layout. A Scaling Constant
#' choice sets \eqn{D} to 1 (the default) or 1.702. The ability axis runs from -6 to 6, so a
#' curve centred beyond that range, which most location sliders allow (they reach -10 and 10),
#' moves out of view. The plots are interactive plotly charts with a marker at every 0.1 of
#' ability: hovering shows the values, clicking a legend entry hides or shows its curve, and
#' the plot can be zoomed and panned. A horizontal gray line marks a probability of 0.5, and
#' the vertical line at 0 is the zero line of the axis. Every plot is redrawn as soon as a
#' control changes.
#'
#' The app has seven tabs, described in the sections below.
#' @section Rasch-4PL:
#' The item characteristic curve of a dichotomous item under the four-parameter logistic model
#' (4PL) and the models nested in it, which fix some of its parameters.
#' \itemize{
#'   \item \strong{Controls:} the scaling constant; the discrimination \eqn{a} (-10 to 10,
#'     default 1); the difficulty \eqn{b} (-10 to 10, default 0); the guessing parameter
#'     \eqn{c} (0 to 1, default 0); the inattentiveness parameter \eqn{d} (0 to 1, default 1);
#'     and the probability to plot: \eqn{P(X = 1 \mid \theta)}{P(X = 1 | theta)} alone, or
#'     \eqn{P(X \mid \theta)}{P(X | theta)} (the default), which adds the probability of an
#'     incorrect response,
#'     \eqn{P(X = 0 \mid \theta) = 1 - P(X = 1 \mid \theta)}{P(X = 0 | theta) = 1 - P(X = 1 | theta)}.
#'   \item \strong{Plot:} the chosen curves across ability, with a legend. The title names the
#'     model that the current values define: Rasch Model when \eqn{a = 1}, \eqn{c = 0} and
#'     \eqn{d = 1}; 1PL or 2PL Model when \eqn{c = 0} and \eqn{d = 1} but \eqn{a} is not 1
#'     (with a single item, a discrimination shared by all items, as in the 1PL, cannot be told
#'     apart from one specific to the item, as in the 2PL); 3PL Model when \eqn{c > 0} and
#'     \eqn{d = 1}; and 4PL Model when \eqn{c > 0} and \eqn{d < 1}.
#'   \item \strong{Text:} below the controls, the name of the model followed by the values of
#'     \eqn{a}, \eqn{b}, \eqn{c} and \eqn{d}. A lowered upper asymptote without a raised lower
#'     one (\eqn{c = 0} and \eqn{d < 1}) is not given a name: the text then reads "No such
#'     model!".
#' }
#' Raising \eqn{b} moves the curve to the right, so that a higher ability is needed for the
#' same probability. The discrimination sets the steepness: at \eqn{\theta = b}{theta = b} the
#' slope is \eqn{D a (d - c) / 4}{D * a * (d - c) / 4}, a negative \eqn{a} makes the
#' probability fall as ability rises, and \eqn{a = 0} makes the curve flat. Raising \eqn{c}
#' lifts the lower end of the curve, so that even the least able answer correctly with
#' probability \eqn{c}, as when guessing; lowering \eqn{d} pulls the upper end down, so that
#' even the most able miss with probability \eqn{1 - d}, as through inattention. With
#' \eqn{c = 0} and \eqn{d = 1} the curve crosses the gray line at \eqn{\theta = b}{theta = b},
#' where the curves of \eqn{P(X = 1 \mid \theta)}{P(X = 1 | theta)} and
#' \eqn{P(X = 0 \mid \theta)}{P(X = 0 | theta)} also cross; in general it reaches the midpoint
#' \eqn{(c + d) / 2} there. Choosing \eqn{D = 1.702} makes the curve steeper by that factor.
#' @section PCM-GPCM:
#' Category response curves of an item scored in five ordered categories, 1 to 5, under the
#' partial credit model (PCM) and the generalized partial credit model (GPCM):
#' \deqn{P(X = k \mid \theta) = \frac{\exp\left(\sum_{v=1}^{k-1} D a (\theta - \delta_v)\right)}{\sum_{h=1}^{5} \exp\left(\sum_{v=1}^{h-1} D a (\theta - \delta_v)\right)}}{P(X = k | theta) = exp(sum over v = 1, ..., k - 1 of D * a * (theta - delta_v)) / (sum over h = 1, ..., 5 of exp(sum over v = 1, ..., h - 1 of D * a * (theta - delta_v)))}
#' for \eqn{k = 1, \ldots, 5}{k = 1, ..., 5}, where \eqn{a} is the discrimination,
#' \eqn{\delta_1, \ldots, \delta_4}{delta_1, ..., delta_4} are the step parameters (one for
#' each step from a category to the next) and an empty sum is 0, so that category 1 has the
#' numerator 1.
#' \itemize{
#'   \item \strong{Controls:} the scaling constant; the discrimination \eqn{a} (-10 to 10 in
#'     steps of 0.5, default 1); and the four step parameters (-10 to 10; defaults -2, -0.7,
#'     0.8 and 2, because the code asks for -0.75 and 0.75, which the sliders round to their
#'     steps of 0.1).
#'   \item \strong{Plot:} the probabilities of the five categories,
#'     \eqn{P(X = 1 \mid \theta)}{P(X = 1 | theta)} to
#'     \eqn{P(X = 5 \mid \theta)}{P(X = 5 | theta)}, one coloured curve each. The title reads
#'     Partial Credit Model (Masters, 1982) when \eqn{a = 1} and Generalized Partial Credit
#'     Model (Muraki, 1992) otherwise. A discrimination of 0 is rejected with an error message
#'     in place of the plot.
#' }
#' The curves of adjacent categories \eqn{k} and \eqn{k + 1} cross exactly at
#' \eqn{\theta = \delta_k}{theta = delta_k}, so each step parameter moves one crossing point.
#' With the step parameters in increasing order, each category in turn is the most likely
#' response as ability rises; if a step parameter is lower than the one before it (reversed
#' steps), the category between them is never the most likely. A larger \eqn{a} makes the
#' curves narrower and more peaked, and a negative \eqn{a} reverses the order of the
#' categories along the ability axis.
#' @section GRM:
#' Category response curves of an item with five ordered categories under the graded response
#' model (GRM). The model first gives, for each of the four boundaries between adjacent
#' categories, the probability of a response in a category above the boundary,
#' \deqn{P^*_k(\theta) = \frac{1}{1 + \exp(-D a (\theta - \delta_k))}, \quad k = 1, \ldots, 4,}{P*_k(theta) = 1 / (1 + exp(-D * a * (theta - delta_k))), k = 1, ..., 4,}
#' and then the probability of each category as the difference between successive boundary
#' curves,
#' \eqn{P(X = m \mid \theta) = P^*_{m-1}(\theta) - P^*_m(\theta)}{P(X = m | theta) = P*_(m-1)(theta) - P*_m(theta)}
#' for \eqn{m = 1, \ldots, 5}{m = 1, ..., 5}, with \eqn{P^*_0 = 1}{P*_0 = 1} and
#' \eqn{P^*_5 = 0}{P*_5 = 0}.
#' \itemize{
#'   \item \strong{Controls:} the scaling constant; the discrimination \eqn{a} (-10 to 10,
#'     default 1.5), shared by all boundaries; and the four difficulties,
#'     \eqn{\delta_1}{delta_1} (-2.5 to -1.5), \eqn{\delta_2}{delta_2} (-1.25 to -0.25),
#'     \eqn{\delta_3}{delta_3} (0 to 1.25) and \eqn{\delta_4}{delta_4} (1.5 to 2.5), with
#'     defaults -2, -0.8, 0.8 and 2 (the code asks for -0.75 and 0.75, which the sliders round
#'     to one decimal). The slider ranges do not overlap, which keeps the difficulties in
#'     increasing order, as the model requires: otherwise some category probabilities would
#'     be negative.
#'   \item \strong{Plot:} \eqn{P(X = 1 \mid \theta)}{P(X = 1 | theta)} to
#'     \eqn{P(X = 5 \mid \theta)}{P(X = 5 | theta)}, one coloured curve each, titled Graded
#'     Response Model.
#' }
#' Each difficulty \eqn{\delta_k}{delta_k} is the ability at which a response above category
#' \eqn{k} becomes as likely as not. Only the first and the last can be read directly from the
#' plot: the curve of category 1 crosses the gray line at \eqn{\delta_1}{delta_1}, and that of
#' category 5 at \eqn{\delta_4}{delta_4}. Unlike the step parameters of the PCM, the
#' difficulties are not the points where adjacent curves cross. Because the GRM gives the item
#' a single discrimination, changing \eqn{a} makes all curves steeper or flatter together.
#' @section RSM-GRSM:
#' Category response curves of an item with five ordered categories under the rating scale
#' model (RSM) and its generalized form with a discrimination parameter (GRSM). Both are the
#' partial credit model with each step parameter split into the difficulty of the item and a
#' threshold:
#' \deqn{P(X = k \mid \theta) = \frac{\exp\left(\sum_{v=1}^{k-1} D a (\theta - (\delta + \tau_v))\right)}{\sum_{h=1}^{5} \exp\left(\sum_{v=1}^{h-1} D a (\theta - (\delta + \tau_v))\right)}}{P(X = k | theta) = exp(sum over v = 1, ..., k - 1 of D * a * (theta - (delta + tau_v))) / (sum over h = 1, ..., 5 of exp(sum over v = 1, ..., h - 1 of D * a * (theta - (delta + tau_v))))}
#' where \eqn{\delta}{delta} is the difficulty (location) of the item and
#' \eqn{\tau_1, \ldots, \tau_4}{tau_1, ..., tau_4} are the thresholds, which in a rating
#' scale model are the same for all items that share a response scale.
#' \itemize{
#'   \item \strong{Controls:} the scaling constant; the discrimination \eqn{a} (-10 to 10,
#'     default 1); the difficulty \eqn{\delta}{delta} (-10 to 10, default 0); and the four
#'     thresholds (-10 to 10; defaults -1, -0.4, 0.4 and 1).
#'   \item \strong{Plot:} \eqn{P(X = 1 \mid \theta)}{P(X = 1 | theta)} to
#'     \eqn{P(X = 5 \mid \theta)}{P(X = 5 | theta)}, one coloured curve each. The title reads
#'     Rating Scale Model (Andrich, 1978) when \eqn{a = 1} and Generalized Rating Scale Model
#'     (Muraki, 1990) otherwise. A discrimination of 0 is rejected with an error message in
#'     place of the plot.
#' }
#' Adjacent curves \eqn{k} and \eqn{k + 1} cross at
#' \eqn{\theta = \delta + \tau_k}{theta = delta + tau_k}. Changing the difficulty shifts all
#' five curves along the ability axis together without changing their shape, the thresholds
#' set the spacing of the crossings around the difficulty, and the discrimination sets how
#' peaked the curves are.
#' @section NRM:
#' Category response curves of an item with five unordered categories, labelled a to e, under
#' the nominal response model (NRM). Category a is the reference category, whose exponent is
#' fixed at 0, and each of the other four categories has its own discrimination
#' \eqn{\alpha_k}{alpha_k} (written with a Greek letter here, to avoid confusion with
#' category a) and difficulty \eqn{\delta_k}{delta_k}:
#' \deqn{P(X = k \mid \theta) = \frac{\exp(z_k)}{\sum_{h=a}^{e} \exp(z_h)}, \quad z_a = 0, \quad z_k = D \alpha_k (\theta - \delta_k) \quad (k = b, \ldots, e).}{P(X = k | theta) = exp(z_k) / (exp(z_a) + exp(z_b) + ... + exp(z_e)), with z_a = 0 and z_k = D * alpha_k * (theta - delta_k) for k = b, ..., e.}
#' Following Baker (1992), each exponent is written with a difficulty rather than an
#' intercept.
#' \itemize{
#'   \item \strong{Controls:} the scaling constant; four discriminations (-10 to 10; defaults
#'     -4, -2, 2 and 4) and four difficulties (-10 to 10; defaults -4, -2, 2 and 4).
#'     Discrimination \eqn{j} and difficulty \eqn{j} belong to the \eqn{j}-th category after
#'     a: 1 to b, 2 to c, 3 to d and 4 to e.
#'   \item \strong{Plot:} \eqn{P(X = a \mid \theta)}{P(X = a | theta)} to
#'     \eqn{P(X = e \mid \theta)}{P(X = e | theta)}, one coloured curve each, titled Nominal
#'     Response Model.
#' }
#' Category \eqn{k} is exactly as likely as category a at
#' \eqn{\theta = \delta_k}{theta = delta_k}, because the ratio of their probabilities is
#' \eqn{\exp(z_k)}{exp(z_k)}. The discriminations order the categories along the ability
#' axis: the category with the largest discrimination (counting 0 for category a) becomes the
#' most likely at high abilities, the one with the smallest at low abilities, and the others
#' can be the most likely in between. Nothing forces an order on the categories, which is what
#' sets the NRM apart from the models for ordered categories. With the default values,
#' categories c, a and d are in turn the most likely across the plotted range, while b and e
#' would take over only beyond -6 and 6.
#' @section TIRT:
#' Probit response functions of Thurstonian item response theory (TIRT), in which a binary
#' outcome, such as the choice between two items in a forced-choice questionnaire, depends on
#' the latent trait \eqn{\eta}{eta} through \eqn{\Phi}{Phi}, the cumulative distribution
#' function of the standard normal distribution. The tab has three sub-tabs, each named by a
#' formula, with these functions and controls:
#' \itemize{
#'   \item \strong{First sub-tab:} a single binary outcome, written with the parameters of
#'     factor analysis,
#'     \deqn{P(\eta) = \Phi\left(\frac{-\gamma + \lambda \eta}{\sqrt{\psi}}\right),}{P(eta) = Phi((-gamma + lambda * eta) / sqrt(psi)),}
#'     with the threshold \eqn{\gamma}{gamma} (-10 to 10, default 0), the factor loading
#'     \eqn{\lambda}{lambda} (-10 to 10, default 0.5) and the error (unique) variance
#'     \eqn{\psi}{psi} (0 to 10, default 0.5).
#'   \item \strong{Second sub-tab:} the choice of item \eqn{i} over item \eqn{k} in a pair,
#'     \deqn{P(\eta) = \Phi\left(\frac{-\gamma + \lambda_i \eta - \lambda_k \eta}{\sqrt{\psi_i + \psi_k}}\right),}{P(eta) = Phi((-gamma + lambda_i * eta - lambda_k * eta) / sqrt(psi_i + psi_k)),}
#'     with the threshold of the pair \eqn{\gamma}{gamma} (-10 to 10, default 0), the
#'     loadings of the two items \eqn{\lambda_i}{lambda_i} and \eqn{\lambda_k}{lambda_k} (-10
#'     to 10; defaults 0.5 and 0) and their error variances \eqn{\psi_i}{psi_i} and
#'     \eqn{\psi_k}{psi_k} (0 to 10, default 0.5 each). The formula on the sub-tab gives each
#'     item its own trait, but the plot uses the same \eqn{\eta}{eta} for both, so only the
#'     difference \eqn{\lambda_i - \lambda_k}{lambda_i - lambda_k} matters: with equal
#'     loadings the curve is flat, and the choice tells nothing about the trait.
#'   \item \strong{Third sub-tab:} the same comparison in intercept and slope form,
#'     \deqn{P(\eta) = \Phi(\alpha + (\beta_i - \beta_k) \eta),}{P(eta) = Phi(alpha + (beta_i - beta_k) * eta),}
#'     with the intercept \eqn{\alpha}{alpha} (-10 to 10, default 0) and the slopes
#'     \eqn{\beta_i}{beta_i} and \eqn{\beta_k}{beta_k} (-10 to 10; defaults 0.5 and 0). It
#'     draws the curve of the second sub-tab when
#'     \eqn{\alpha = -\gamma / \sqrt{\psi_i + \psi_k}}{alpha = -gamma / sqrt(psi_i + psi_k)}
#'     and each \eqn{\beta = \lambda / \sqrt{\psi_i + \psi_k}}{beta = lambda / sqrt(psi_i + psi_k)};
#'     with their defaults, both sub-tabs show \eqn{\Phi(0.5 \eta)}{Phi(0.5 * eta)}.
#'   \item \strong{Range:} each sub-tab also has a slider labelled \eqn{\eta}{eta} (1 to 100,
#'     default 6) that sets no parameter: it sets the range of the horizontal axis, from
#'     \eqn{-\eta}{-eta} to \eqn{\eta}{eta}.
#'   \item \strong{Plot:} the probability, from 0 to 1, against \eqn{\eta}{eta}: a single
#'     curve with a marker at every 0.1, and the gray line at 0.5.
#' }
#' Each curve crosses 0.5 where the argument of \eqn{\Phi}{Phi} is 0, for example at
#' \eqn{\eta = \gamma / \lambda}{eta = gamma / lambda} in the first sub-tab. The loadings or
#' slopes set the steepness and direction of the curve, and larger error variances make it
#' flatter. There is no scaling constant in this tab, since the curves are normal ogives.
#' @section MIRT Dichotomous:
#' The response surface of a dichotomous item that measures two abilities,
#' \eqn{\theta_1}{theta1} and \eqn{\theta_2}{theta2} (multidimensional IRT, MIRT), on a grid
#' of 50 by 50 points from -4 to 4 on each dimension. In the compensatory model (the default)
#' the probability is a logistic function of a weighted sum of the two abilities,
#' \deqn{P(\theta_1, \theta_2) = c + \frac{d - c}{1 + \exp(-D (a_1 (\theta_1 - \delta_1) + a_2 (\theta_2 - \delta_2)))},}{P(theta1, theta2) = c + (d - c) / (1 + exp(-D * (a_1 * (theta1 - delta_1) + a_2 * (theta2 - delta_2)))),}
#' so that a high ability on one dimension can make up for a low ability on the other. In the
#' non-compensatory model it is the product of one logistic function for each ability,
#' \deqn{P(\theta_1, \theta_2) = c + (d - c) \frac{1}{1 + \exp(-D a_1 (\theta_1 - \delta_1))} \cdot \frac{1}{1 + \exp(-D a_2 (\theta_2 - \delta_2))},}{P(theta1, theta2) = c + (d - c) / ((1 + exp(-D * a_1 * (theta1 - delta_1))) * (1 + exp(-D * a_2 * (theta2 - delta_2)))),}
#' so that a low ability on either dimension keeps the probability low, whatever the other.
#' Here \eqn{a_1} and \eqn{a_2} are the discriminations, \eqn{\delta_1}{delta_1} and
#' \eqn{\delta_2}{delta_2} the locations on the two dimensions, and \eqn{c} and \eqn{d} the
#' lower and upper asymptotes.
#' \itemize{
#'   \item \strong{Controls:} a choice labelled Item Response Function, which only sets the
#'     scaling constant of the logistic function: Normal Ogive gives \eqn{D = 1} and Logistic
#'     (the default) gives \eqn{D = 1.702}; whether the model is compensatory (Yes, the
#'     default, or No); locations 1 and 2 (-10 to 10, default 0); discriminations 1 and 2
#'     (-10 to 10, default 1; a value of 0 is rejected with an error message); the lower
#'     asymptote \eqn{c} (0 to 0.4 in steps of 0.1, default 0) and the upper asymptote
#'     \eqn{d} (0 to 1, default 1); and names for the two dimensions (default Math and
#'     Reading), used as axis titles.
#'   \item \strong{Item Response Surface sub-tab:} \eqn{P(\theta_1, \theta_2)}{P(theta1, theta2)}
#'     as an interactive 3-D surface, coloured from purple (low) to yellow (high probability),
#'     with P(Y = 1) from 0 to 1 on the vertical axis; drag it to rotate it.
#'   \item \strong{Contour Plot sub-tab:} the same surface seen from above, as contour lines of
#'     equal probability, each labelled with its probability; a slider sets the approximate
#'     number of contour levels (10 to 100, default 10). Black lines mark
#'     \eqn{\theta_1 = 0}{theta1 = 0} and \eqn{\theta_2 = 0}{theta2 = 0}. A thick line marks
#'     the contour halfway between the asymptotes: 0.5 when \eqn{c = 0} and \eqn{d = 1},
#'     \eqn{(c + 1) / 2} when \eqn{c > 0} and \eqn{d = 1}, and \eqn{(c + d) / 2} when
#'     \eqn{c > 0} and \eqn{d < 1} (no thick line is drawn when \eqn{c = 0} and \eqn{d < 1}).
#'     For the compensatory model a blue arrow shows the item vector. It starts on the line
#'     where the probability is halfway between the asymptotes, at the point of that line
#'     closest to the origin; it points in the direction in which the probability rises
#'     fastest, at the angle \eqn{\arccos(a_1 / A)}{arccos(a_1 / A)} to the
#'     \eqn{\theta_1}{theta1} axis; and its length is the multidimensional discrimination
#'     \eqn{A = \sqrt{a_1^2 + a_2^2}}{A = sqrt(a_1^2 + a_2^2)}. Its starting point lies at the
#'     signed distance \eqn{(a_1 \delta_1 + a_2 \delta_2) / A}{(a_1 * delta_1 + a_2 * delta_2) / A},
#'     the multidimensional difficulty, from the origin along the direction of the arrow.
#'   \item \strong{Information Plot sub-tab:} a 3-D surface, seen from the opposite corner, of
#'     the item information at each point of the grid, computed for both models from the
#'     probability \eqn{P} at that point as
#'     \deqn{I(\theta_1, \theta_2) = A^2 \frac{(P - c)^2 (d - P)^2}{(d - c)^2} \cdot \frac{1 - P}{P}.}{I(theta1, theta2) = A^2 * (P - c)^2 * (d - P)^2 / (d - c)^2 * (1 - P) / P.}
#' }
#' In the compensatory model the contour lines are straight and parallel, at right angles to
#' the item vector, because the probability depends only on the weighted sum of the abilities;
#' in the non-compensatory model they bend, since both abilities must be high for a high
#' probability. Making one discrimination larger than the other turns the item vector towards
#' the axis of that dimension, which means that the item measures mostly that ability, and
#' moving a location shifts the surface along its dimension.
#' @section Acknowledgments:
#' modelsirt is a fork and substantial extension of the irtDemo package by Wes Bonifay and
#' Metin Bulus, which is available on CRAN (\url{https://cran.r-project.org/package=irtDemo});
#' the title of the app, IRT Models using IRTDemo, reflects this origin. Dimitrios Zacharatos
#' modified and expanded it: he consolidated the IRT models into a single interface, rebuilt
#' the visualisations with Plotly for interactivity, added further IRT models and improved the
#' user interface. Special thanks to Kostas Maistrelis (\url{https://github.com/maisk}) for
#' contributing the implementation of the Thurstonian IRT curves. The tabs keep the credits
#' shown in the app itself: the Rasch-4PL, PCM-GPCM and RSM-GRSM tabs are credited to Metin
#' Bulus, and the GRM, NRM and MIRT Dichotomous tabs to an unknown contributor.
#' @return A Shiny app object (\code{shiny::shinyApp()}). Printing it, which is what happens
#'   when you call \code{modelsirt()} at the console, starts the app in the browser or the
#'   RStudio viewer; you can also pass it to \code{shiny::runApp()}.
#' @references Andrich, D. (1978). A rating formulation for ordered response categories.
#'   \emph{Psychometrika}, 43(4), 561-573.
#'
#'   Baker, F. B. (1992). \emph{Item Response Theory: Parameter Estimation Techniques}. New
#'   York: Marcel Dekker.
#'
#'   Bulus, M., and Bonifay, W. (2022). irtDemo R Package: Pedagogical Interactive Web
#'   Applications for Estimation, Scoring, and Multi Dimensionality in Item Response Theory.
#'   \emph{Anadolu University Journal of Education Faculty}, 6(1), 92-108.
#'   \doi{10.34056/aujef.913781}
#'
#'   Masters, G. N. (1982). A Rasch model for partial credit scoring. \emph{Psychometrika},
#'   47(2), 149-174.
#'
#'   Muraki, E. (1990). Fitting a polytomous item response model to Likert-type data.
#'   \emph{Applied Psychological Measurement}, 14(1), 59-71.
#'
#'   Muraki, E. (1992). A generalized partial credit model: Application of an EM algorithm.
#'   \emph{Applied Psychological Measurement}, 16(2), 159-176.
#' @import magrittr
#' @importFrom shiny shinyApp tagList navbarPage tabPanel titlePanel h4 sidebarPanel selectInput sliderInput textOutput mainPanel tabsetPanel withMathJax textInput inputPanel plotOutput renderText reactive renderPlot
#' @importFrom plotly plotlyOutput renderPlotly plot_ly add_segments layout
#' @importFrom grDevices contourLines
#' @importFrom graphics abline arrows layout lines
#' @importFrom stats pnorm
#' @importFrom utils install.packages
#' @keywords irt models
#' @export
#' @examples
#' if (interactive()) modelsirt()
modelsirt<-function() {
  ##########################################################################################
  # RACH
  ##########################################################################################
  la_rach<-seq(from=0.05,to=0.6,by=0.1)
  ua_rach<-seq(from=0.05,to=0.6,by=0.1)
  thetas_rach<-seq(from=-6,to=6,by=0.1)
  deltas_rach<-seq(from=-6,to=6,by=0.1)
  alphas_rach<-seq(from=-6,to=6,by=0.1)
  N_rach=length(thetas_rach)
  p1_rach<-matrix(NA,nrow=N_rach,1)
  Pfun_rach<-function(la=0,ua=1,alpha=1,theta,delta,D=1) {
    e_func<-exp(-(D*alpha*(theta-delta)))
    renum<-ua-la
    result<-la+((renum)/(1+e_func))
    return(result)
  }
  ##########################################################################################
  # PARTIAL CREDIT
  ##########################################################################################
  thetas_pcm<-seq(from=-6,to=6,by=0.1)
  deltas_pcm<-seq(from=-6,to=6,by=0.1)
  alphas_pcm<-seq(from=-6,to=6,by=0.1)
  N_pcm=length(thetas_pcm)
  p1num_pcm<-matrix(NA,nrow=N_pcm,ncol=1)
  p2num_pcm<-matrix(NA,nrow=N_pcm,ncol=1)
  p3num_pcm<-matrix(NA,nrow=N_pcm,ncol=1)
  p4num_pcm<-matrix(NA,nrow=N_pcm,ncol=1)
  p5num_pcm<-matrix(NA,nrow=N_pcm,ncol=1)
  p2fun_pcm<-function(alpha,theta,delta1,D){
    z1<-D*alpha*(theta-delta1)
    return(exp(0+z1))
  }
  p3fun_pcm<-function(alpha,theta,delta1,delta2,D){
    z1<-D*alpha*(theta-delta1)
    z2<-D*alpha*(theta-delta2)
    return(exp(0+z1+z2))
  }
  p4fun_pcm<-function(alpha,theta,delta1,delta2,delta3,D){
    z1<-D*alpha*(theta-delta1)
    z2<-D*alpha*(theta-delta2)
    z3<-D*alpha*(theta-delta3)
    return(exp(0+z1+z2+z3))
  }
  p5fun_pcm<-function(alpha,theta,delta1,delta2,delta3,delta4,D){
    z1<-D*alpha*(theta-delta1)
    z2<-D*alpha*(theta-delta2)
    z3<-D*alpha*(theta-delta3)
    z4<-D*alpha*(theta-delta4)
    return(exp(0+z1+z2+z3+z4))
  }
  ##########################################################################################
  # GRADED RESPONSE
  ##########################################################################################
  thetas_grm<-seq(from=-6,to=6,by=0.1)
  N_grm=length(thetas_grm)
  Pfun_grm<-function(alpha=1,theta,delta,D=1){
    z=D*alpha*(theta-delta)
    return(1/(1+exp(-z)))
  }
  ##########################################################################################
  # RATING SCALE RESPONSE
  ##########################################################################################
  thetas_rsm<-seq(from=-6,to=6,by=0.1)
  deltas_rsm<-seq(from=-6,to=6,by=0.1)
  alphas_rsm<-seq(from=-6,to=6,by=0.1)
  N_rsm=length(thetas_rsm)
  p1num_rsm<-matrix(NA,nrow=N_rsm,ncol=1)
  p2num_rsm<-matrix(NA,nrow=N_rsm,ncol=1)
  p3num_rsm<-matrix(NA,nrow=N_rsm,ncol=1)
  p4num_rsm<-matrix(NA,nrow=N_rsm,ncol=1)
  p5num_rsm<-matrix(NA,nrow=N_rsm,ncol=1)
  p2fun_rsm<-function(alpha,theta,delta,tau1,D){
    z1<-D*alpha*(theta-(delta+tau1))
    return(exp(0+z1))
  }
  p3fun_rsm<-function(alpha,theta,delta,tau1,tau2,D){
    z1<-D*alpha*(theta-(delta+tau1))
    z2<-D*alpha*(theta-(delta+tau2))
    return(exp(0+z1+z2))
  }
  p4fun_rsm<-function(alpha,theta,delta,tau1,tau2,tau3,D){
    z1<-D*alpha*(theta-(delta+tau1))
    z2<-D*alpha*(theta-(delta+tau2))
    z3<-D*alpha*(theta-(delta+tau3))
    return(exp(0+z1+z2+z3))
  }
  p5fun_rsm<-function(alpha,theta,delta,tau1,tau2,tau3,tau4,D){
    z1<-D*alpha*(theta-(delta+tau1))
    z2<-D*alpha*(theta-(delta+tau2))
    z3<-D*alpha*(theta-(delta+tau3))
    z4<-D*alpha*(theta-(delta+tau4))
    return(exp(0+z1+z2+z3+z4))
  }
  ##########################################################################################
  # NOMINAL RESPONSE
  ##########################################################################################
  thetas_nrm<-seq(from=-6,to=6,by=0.1)
  deltas_nrm<-seq(from=-6,to=6,by=0.1)
  alphas_nrm<-seq(from=-6,to=6,by=0.1)
  N_nrm=length(thetas_nrm)
  p2num_nrm<-matrix(NA,nrow=N_nrm,ncol=1)
  p3num_nrm<-matrix(NA,nrow=N_nrm,ncol=1)
  p4num_nrm<-matrix(NA,nrow=N_nrm,ncol=1)
  p5num_nrm<-matrix(NA,nrow=N_nrm,ncol=1)
  p2fun_nrm<-function(alpha1,theta,delta1,D){return(exp(D*alpha1*(theta-delta1)))}
  p3fun_nrm<-function(alpha2,theta,delta2,D){return(exp(D*alpha2*(theta-delta2)))}
  p4fun_nrm<-function(alpha3,theta,delta3,D){return(exp(D*alpha3*(theta-delta3)))}
  p5fun_nrm<-function(alpha4,theta,delta4,D){return(exp(D*alpha4*(theta-delta4)))}
  ##########################################################################################
  # DICHOTOMOUS MULTIDIMENSIONAL
  ##########################################################################################
  N_mirtd<-50
  thetas1<-seq(from=-4,to=4,length.out=N_mirtd)
  thetas2<-seq(from=-4,to=4,length.out=N_mirtd)
  pjj<-matrix(NA,nrow=N_mirtd,ncol=N_mirtd)
  Pfun_mirtd<-function(theta1,theta2,c,d,alpha1,alpha2,delta1,delta2,D){
    gamma<--(alpha1*delta1+alpha2*delta2)
    alpha<-matrix(c(alpha1,alpha2),nrow=2,ncol=1)
    theta<-matrix(c(theta1,theta2),nrow=2,ncol=1)
    z<-D*(t(alpha)%*%theta+gamma)
    return(c+(d-c)*(1/(1+exp(-z))))
  }
  Pfun_mirtd2<-function(theta1,theta2,c,d,alpha1,alpha2,delta1,delta2,D){
    gamma1<--(alpha1*delta1)
    gamma2<--(alpha2*delta2)
    z1<-D*(alpha1*theta1+gamma1)
    z2<-D*(alpha2*theta2+gamma2)
    return(c+(d-c)*(1/(1+exp(-z1)))*(1/(1+exp(-z2))))
  }
  ##########################################################################################
  # THURSTONIAN
  ##########################################################################################
  pn_gamma<-seq(from=0.05,to=0.6,by=0.1)
  pn_lambda<-seq(from=0.05,to=0.6,by=0.1)
  pn_eta<-seq(from=-6,to=6,by=0.1)
  pn_psi<-seq(from=-6,to=6,by=0.1)
  icc_cfa_thurstonian<-function(eta,gamma,lambda,psi) {
    return(pnorm((-gamma+lambda*eta)/(sqrt(psi))))
  }
  icc_cfa_thurstonian_bf<-function(eta,gamma,lambda_i,lambda_k,psi_i,psi_k) {
    return(pnorm((-gamma+lambda_i*eta-lambda_k*eta)/(sqrt(psi_i+psi_k))))
  }
  icc_cfa_thurstonian_l<-function(eta,alpha,beta_i,beta_k) {
    return(pnorm(alpha+(beta_i-beta_k)*eta))
  }
  shiny::shinyApp(ui=shiny::tagList(
    shiny::navbarPage("IRT Models using IRTDemo",
                      ##########################################################################################
                      # RACH
                      ##########################################################################################
                      shiny::tabPanel("Rasch-4PL",shiny::titlePanel(shiny::h4("by Metin Bulus")),
                                      shiny::sidebarPanel(shiny::selectInput("D_rach",label="Scaling Constant",list("1"=1,"1.702"=2),selected=1),
                                                          shiny::sliderInput("alpha_rach",label="Discrimination (a)",min=-10,max=10,value=1,step=.1),
                                                          shiny::sliderInput("delta_rach",label="Difficulty (b)",min=-10,max=10,value=0,step=.1),
                                                          shiny::sliderInput("la_rach",label="Guessing (c)",min=0,max=1,value=0,step=.01),
                                                          shiny::sliderInput("ua_rach",label="Inattentiveness (d)",min=0,max=1,value=1,step=.01),
                                                          shiny::selectInput("modprob_rach",label="Modeled Probability",list("P(X=1|theta)"=1,"P(X|theta)"=2),selected=2),
                                                          shiny::textOutput(outputId="feedback")),
                                      shiny::mainPanel(#shiny::plotOutput(outputId="dich_plot",width="100%"),
                                        plotly::plotlyOutput(outputId="rach_dichotomous_plotly",width="100%",height="700px")
                                      )),
                      ##########################################################################################
                      # PARTIAL CREDIT
                      ##########################################################################################
                      shiny::tabPanel("PCM-GPCM",shiny::titlePanel(shiny::h4("by Metin Bulus")),
                                      shiny::sidebarPanel(shiny::selectInput("D_pcm",label="Scaling Constant",list("1"=1,"1.702"=2),selected=1),
                                                          shiny::sliderInput("alpha_pcm",label="Discrimination",min=-10,max=10,value=1,step=.5),
                                                          shiny::sliderInput("delta1_pcm",label="Step parameter 1",min=-10,max=10,value=-2,step=.1),
                                                          shiny::sliderInput("delta2_pcm",label="Step parameter 2",min=-10,max=10,value=-0.75,step=.1),
                                                          shiny::sliderInput("delta3_pcm",label="Step parameter 3",min=-10,max=10,value=0.75,step=.1),
                                                          shiny::sliderInput("delta4_pcm",label="Step parameter 4",min=-10,max=10,value=2,step=.1)),
                                      shiny::mainPanel(#shiny::plotOutput(outputId="gpcm_plot",width="100%"),
                                        plotly::plotlyOutput(outputId="gpcm_plotly",width="100%",height="700px"))),
                      ##########################################################################################
                      # GRADED RESPONSE
                      ##########################################################################################
                      shiny::tabPanel("GRM",shiny::titlePanel(shiny::h4("Unknown Contributor")),
                                      shiny::sidebarPanel(shiny::selectInput("D_grm",label="Scaling Constant",list("1"=1,"1.702"=2),selected=1),
                                                          shiny::sliderInput("alpha_grm",label="Discrimination",min=-10,max=10,value=1.5,step=.1),
                                                          shiny::sliderInput("delta1_grm",label="Difficulty 1",min=-2.5,max=-1.5,value=-2,step=.1),
                                                          shiny::sliderInput("delta2_grm",label="Difficulty 2",min=-1.25,max=-0.25,value=-0.75,step=.1),
                                                          shiny::sliderInput("delta3_grm",label="Difficulty 3",min=0,max=1.25,value=0.75,step=.1),
                                                          shiny::sliderInput("delta4_grm",label="Difficulty 4",min=1.5,max=2.5,value=2,step=.1)),
                                      shiny::mainPanel(#shiny::plotOutput(outputId="grm_plot",width="100%"),
                                        plotly::plotlyOutput(outputId="grm_plotly",width="100%",height="700px"))),
                      ##########################################################################################
                      # RATING SCALE
                      ##########################################################################################
                      shiny::tabPanel("RSM-GRSM",shiny::titlePanel(shiny::h4("by Metin Bulus")),
                                      shiny::sidebarPanel(shiny::selectInput("D_rsm",label="Scaling Constant",list("1"=1,"1.702"=2),selected=1),
                                                          shiny::sliderInput("alpha_rsm",label="Discrimination",min=-10,max=10,value=1,step=.1),
                                                          shiny::sliderInput("delta_rsm",label="Difficulty",min=-10,max=10,value=0,step=.1),
                                                          shiny::sliderInput("tau1_rsm",label="Threshold 1",min=-10,max=10,value=-1,step=.1),
                                                          shiny::sliderInput("tau2_rsm",label="Threshold 2",min=-10,max=10,value=-0.4,step=.1),
                                                          shiny::sliderInput("tau3_rsm",label="Threshold 3",min=-10,max=10,value=0.4,step=.1),
                                                          shiny::sliderInput("tau4_rsm",label="Threshold 4",min=-10,max=10,value=1,step=.1)),
                                      shiny::mainPanel(#shiny::plotOutput(outputId="grsm_plot",width="100%"),
                                        plotly::plotlyOutput(outputId="grsm_plotly",width="100%",height="700px"))),
                      ##########################################################################################
                      # NOMINAL RESPONSE
                      ##########################################################################################
                      shiny::tabPanel("NRM",shiny::titlePanel(shiny::h4("Unknown Contributor")),
                                      shiny::sidebarPanel(shiny::selectInput("D_nrm",label="Scaling Constant",list("1"=1,"1.702"=2),selected=1),
                                                          shiny::sliderInput("alpha1_nrm",label="Discrimination 1",min=-10,max=10,value=-4,step=.1),
                                                          shiny::sliderInput("alpha2_nrm",label="Discrimination 2",min=-10,max=10,value=-2,step=.1),
                                                          shiny::sliderInput("alpha3_nrm",label="Discrimination 3",min=-10,max=10,value=2,step=.1),
                                                          shiny::sliderInput("alpha4_nrm",label="Discrimination 4",min=-10,max=10,value=4,step=.1),
                                                          shiny::sliderInput("delta1_nrm",label="Difficulty 1",min=-10,max=10,value=-4,step=.1),
                                                          shiny::sliderInput("delta2_nrm",label="Difficulty 2",min=-10,max=10,value=-2,step=.1),
                                                          shiny::sliderInput("delta3_nrm",label="Difficulty 3",min=-10,max=10,value=2,step=.1),
                                                          shiny::sliderInput("delta4_nrm",label="Difficulty 4",min=-10,max=10,value=4,step=.1)),
                                      shiny::mainPanel(#shiny::plotOutput(outputId="nrm_plot",width="100%"),
                                        plotly::plotlyOutput(outputId="nrm_plotly",width="100%",height="700px"))),
                      ##########################################################################################
                      # THURSTONIAN
                      ##########################################################################################
                      shiny::tabPanel("TIRT",
                                      shiny::tabsetPanel(shiny::tabPanel(shiny::withMathJax('$$\\phi\\,\\left[\\frac{-\\gamma_l+\\lambda_l\\eta}{\\sqrt{\\psi^2_l}}\\right]$$'),
                                                                         shiny::sidebarPanel(shiny::sliderInput("pn_gamma",label=shiny::withMathJax('$$\\gamma$$'),min=-10,max=10,value=0,step=.1),
                                                                                             shiny::sliderInput("pn_lambda",label=shiny::withMathJax('$$\\lambda$$'),min=-10,max=10,value=.5,step=.1),
                                                                                             shiny::sliderInput("pn_eta",label=shiny::withMathJax('$$\\eta$$'),min=1,max=100,value=6,step=1),
                                                                                             shiny::sliderInput("pn_psi",label=shiny::withMathJax('$$\\psi$$'),min=0,max=10,value=.5,step=.1)),
                                                                         shiny::mainPanel(#shiny::plotOutput(outputId="dichotomous_plot",width="100%"),
                                                                           plotly::plotlyOutput(outputId="dichotomous_plotly",width="100%",height="700px"))),
                                                         shiny::tabPanel(shiny::withMathJax('$$\\phi\\,\\left[\\frac{-\\gamma+\\lambda_l\\eta_α-\\lambda_k\\eta_b}{\\sqrt{\\psi^2_i+\\psi^2_k}}\\right]$$'),
                                                                         shiny::sidebarPanel(shiny::sliderInput("pn_gamma_l",label=shiny::withMathJax('$$\\gamma$$'),min=-10,max=10,value=0,step=.1),
                                                                                             shiny::sliderInput("pn_lambda_i",label=shiny::withMathJax('$$\\lambda_i$$'),min=-10,max=10,value=.5,step=.1),
                                                                                             shiny::sliderInput("pn_lambda_k",label=shiny::withMathJax('$$\\lambda_k$$'),min=-10,max=10,value=0,step=.1),
                                                                                             shiny::sliderInput("pn_eta_ab",label=shiny::withMathJax('$$\\eta$$'),min=1,max=100,value=6,step=1),
                                                                                             shiny::sliderInput("pn_psi_i",label=shiny::withMathJax('$$\\psi_i$$'),min=0,max=10,value=.5,step=.1),
                                                                                             shiny::sliderInput("pn_psi_k",label=shiny::withMathJax('$$\\psi_k$$'),min=0,max=10,value=.5,step=.1)),
                                                                         shiny::mainPanel(#shiny::plotOutput(outputId="dichotomous_plot1",width="100%"),
                                                                           plotly::plotlyOutput(outputId="dichotomous_plotly1",width="100%",height="700px"))),
                                                         shiny::tabPanel(shiny::withMathJax('$$\\phi\\,\\left(\\alpha+\\left(\\beta_i-\\beta_k\\right)\\eta\\right)$$'),
                                                                         shiny::sidebarPanel(shiny::sliderInput("pn_alpha",label=shiny::withMathJax('$$\\alpha$$'),min=-10,max=10,value=0,step=.1),
                                                                                             shiny::sliderInput("pn_beta_i",label=shiny::withMathJax('$$\\beta_i$$'),min=-10,max=10,value=.5,step=.1),
                                                                                             shiny::sliderInput("pn_beta_k",label=shiny::withMathJax('$$\\beta_k$$'),min=-10,max=10,value=0,step=.1),
                                                                                             shiny::sliderInput("pn_eta_abl",label=shiny::withMathJax('$$\\eta$$'),min=1,max=100,value=6,step=1)),
                                                                         shiny::mainPanel(#shiny::plotOutput(outputId="dichotomous_plot2",width="100%"),
                                                                           plotly::plotlyOutput(outputId="dichotomous_plotly2",width="100%",height="700px"))))),
                      ##########################################################################################
                      # MULTIDIMENSIONAL DICHOTOMOUS
                      ##########################################################################################
                      shiny::tabPanel("MIRT Dichotomous",shiny::titlePanel(shiny::h4("Unknown Contributor")),
                                      shiny::sidebarPanel(width=3,shiny::selectInput("D_mdm",label="Item Response Function",list("Normal Ogive"=1,"Logistic"=2),selected=2),
                                                          shiny::selectInput("comp_mdm",label="Compensatory",list("Yes"=1,"No"=2),selected=1),
                                                          shiny::sliderInput("delta1_mdm",label="Location 1",min=-10,max=10,value=0,step=0.1),
                                                          shiny::sliderInput("delta2_mdm",label="Location 2",min=-10,max=10,value=0,step=0.1),
                                                          shiny::sliderInput("alpha1_mdm",label="Discrimination 1",min=-10,max=10,value=1,step=0.1),
                                                          shiny::sliderInput("alpha2_mdm",label="Discrimination 2",min=-10,max=10,value=1,step=0.1),
                                                          shiny::sliderInput("c_mdm",label="Lower Asymptote",min=0,max=.4,value=0,step=0.1),
                                                          shiny::sliderInput("d_mdm",label="Upper Asymptote",min=0,max=1,value=1,step=0.1),
                                                          shiny::textInput("nametheta1_mdm",label="Name Dimension 1","Math"),
                                                          shiny::textInput("nametheta2_mdm",label="Name Dimension 2","Reading")),
                                      shiny::mainPanel(shiny::tabsetPanel(shiny::tabPanel("Item Response Surface",
                                                                                          shiny::mainPanel(plotly::plotlyOutput(outputId="plotplotly",width="100%",height="800px"))),
                                                                          shiny::tabPanel("Contour Plot",shiny::inputPanel(
                                                                            shiny::sliderInput("nlevels_mdm",label="N Contour Levels",min=10,max=100,value=10,step=1)),
                                                                            shiny::mainPanel(shiny::plotOutput(outputId="plotcont",width="100%",height="700px"))),
                                                                          shiny::tabPanel("Information Plot",
                                                                                          shiny::mainPanel(plotly::plotlyOutput(outputId="infoplotly",width="100%",height="800px"))))))
    )),server=function(input,output){
      ##########################################################################################
      # RACH
      ##########################################################################################
      output$rach_dichotomous_plotly<-plotly::renderPlotly({
        D<-switch(input$D_rach,"1"=1,"2"=1.702)
        for(j in 1:N_rach){ p1_rach[j]<-Pfun_rach(D=D,la=input$la_rach,ua=input$ua_rach,theta=thetas_rach[j],delta=input$delta_rach,alpha=input$alpha_rach) }
        if(input$la_rach==0 & input$ua_rach==1 & input$alpha_rach==1){title<-c("Rasch Model")}
        if(input$la_rach==0 & input$ua_rach==1 & input$alpha_rach!=1){title<-c("1PL or 2PL Model")}
        if(input$ua_rach==1 & input$la_rach!=0){title<-c("3PL Model")}
        if(input$la_rach!=0 & input$ua_rach!=1){title<-c("4PL Model")}
        output$feedback<-shiny::renderText({ifelse(input$ua_rach!=1 & input$la_rach==0,"No such model!",paste(title,": a=",input$alpha_rach," b=",input$delta_rach," c=",input$la_rach," d=",input$ua_rach))})
        modprob<-switch(input$modprob_rach,"1"=1,"2"=2)
        if(modprob==1){
          df<-data.frame(x=thetas_rach,y=p1_rach,type="P(X=1|theta)")
        }
        if(modprob==2){
          df<-rbind(data.frame(x=thetas_rach,y=p1_rach,type="P(X=1|theta)"),
                    data.frame(x=thetas_rach,y=1-p1_rach,type="P(X=0|theta)"))
        }
        plotly::plot_ly(data=df,x=~x,y=~y,color=~type,type="scatter",mode="lines+markers")%>%
          plotly::layout(title=list(text=title),
                         xaxis=list(title="θ"),
                         yaxis=list(title="P(x)",range=c(0,1)),
                         legend=list(x=0,y=.99))%>%
          plotly::add_segments(x=min(thetas_rach,na.rm=TRUE),xend=max(thetas_rach,na.rm=TRUE),y=.5,yend=.5,line=list(color="gray",size=.1),inherit=FALSE,showlegend=FALSE)
      })
      ##########################################################################################
      # PARTIAL CREDIT
      ##########################################################################################
      output$gpcm_plotly<-plotly::renderPlotly({
        D<-switch(input$D_pcm,"1" = 1,"2" = 1.702)
        if(input$alpha_pcm==0){stop("Why would you have an item with 0(zero) discrimination!")}
        p1num_pcm=1
        for(j in 1:N_pcm){
          p2num_pcm[j]<-p2fun_pcm(D=D,theta=thetas_pcm[j],alpha=input$alpha_pcm,delta1=input$delta1_pcm)
          p3num_pcm[j]<-p3fun_pcm(D=D,theta=thetas_pcm[j],alpha=input$alpha_pcm,delta1=input$delta1_pcm,delta2=input$delta2_pcm)
          p4num_pcm[j]<-p4fun_pcm(D=D,theta=thetas_pcm[j],alpha=input$alpha_pcm,delta1=input$delta1_pcm,delta2=input$delta2_pcm,delta3=input$delta3_pcm)
          p5num_pcm[j]<-p5fun_pcm(D=D,theta=thetas_pcm[j],alpha=input$alpha_pcm,delta1=input$delta1_pcm,delta2=input$delta2_pcm,delta3=input$delta3_pcm,delta4=input$delta4_pcm)
        }
        pdenom<-1+p2num_pcm+p3num_pcm+p4num_pcm+p5num_pcm
        p1<-p1num_pcm/pdenom
        p2<-p2num_pcm/pdenom
        p3<-p3num_pcm/pdenom
        p4<-p4num_pcm/pdenom
        p5<-p5num_pcm/pdenom
        title<-ifelse(input$alpha_pcm==1,"Partial Credit Model (Masters,1982)","Generalized Partial Credit Model (Muraki,1992)")
        df<-rbind(data.frame(x=thetas_pcm,y=p1,type="P(X=1|theta)"),
                  data.frame(x=thetas_pcm,y=p2,type="P(X=2|theta)"),
                  data.frame(x=thetas_pcm,y=p3,type="P(X=3|theta)"),
                  data.frame(x=thetas_pcm,y=p4,type="P(X=4|theta)"),
                  data.frame(x=thetas_pcm,y=p5,type="P(X=5|theta)"))
        plotly::plot_ly(data=df,x=~x,y=~y,color=~type,type="scatter",mode="lines+markers")%>%
          plotly::layout(title=title,
                         xaxis=list(title="θ"),
                         yaxis=list(title="P(x)",range=c(0,1)),
                         legend=list(x=0,y=.99))%>%
          plotly::add_segments(x=min(thetas_pcm,na.rm=TRUE),xend=max(thetas_pcm,na.rm=TRUE),y=.5,yend=.5,line=list(color="gray",size=.1),inherit=FALSE,showlegend=FALSE)
      })
      ##########################################################################################
      # GRADED RESPONSE
      ##########################################################################################
      output$grm_plotly<-plotly::renderPlotly({
        D<-switch(input$D_grm,"1"=1,"2"=1.702)
        p<-matrix(NA,nrow=N_grm,4)
        for(j in 1:N_grm){
          p[j,1]<-Pfun_grm(D=D,theta=thetas_grm[j],delta=input$delta1_grm,alpha=input$alpha_grm)
          p[j,2]<-Pfun_grm(D=D,theta=thetas_grm[j],delta=input$delta2_grm,alpha=input$alpha_grm)
          p[j,3]<-Pfun_grm(D=D,theta=thetas_grm[j],delta=input$delta3_grm,alpha=input$alpha_grm)
          p[j,4]<-Pfun_grm(D=D,theta=thetas_grm[j],delta=input$delta4_grm,alpha=input$alpha_grm)
        }
        title<-"Graded Response Model"
        df<-rbind(data.frame(x=thetas_grm,y=1-p[,1],type="P(X=1|theta)"),
                  data.frame(x=thetas_grm,y=p[,1]-p[,2],type="P(X=2|theta)"),
                  data.frame(x=thetas_grm,y=p[,2]-p[,3],type="P(X=3|theta)"),
                  data.frame(x=thetas_grm,y=p[,3]-p[,4],type="P(X=4|theta)"),
                  data.frame(x=thetas_grm,y=p[,4]-0,type="P(X=5|theta)"))
        plotly::plot_ly(data=df,x=~x,y=~y,color=~type,type="scatter",mode="lines+markers")%>%
          plotly::layout(title=title,
                         xaxis=list(title="θ"),
                         yaxis=list(title="P(X=m|theta)",range=c(0,1)),
                         legend=list(x=0,y=.99))%>%
          plotly::add_segments(x=min(thetas_grm,na.rm=TRUE),xend=max(thetas_grm,na.rm=TRUE),y=.5,yend=.5,line=list(color="gray",size=.1),inherit=FALSE,showlegend=FALSE)
      })
      ##########################################################################################
      # RATING SCALE
      ##########################################################################################
      output$grsm_plotly<-plotly::renderPlotly({
        D<-switch(input$D_rsm,"1"=1,"2"=1.702)
        if(input$alpha_rsm==0){stop("Why would you have an item with 0 (zero) discrimination!")}
        p1num_rsm=1
        for(j in 1:N_rsm){
          p2num_rsm[j]<-p2fun_rsm(D=D,theta=thetas_rsm[j],alpha=input$alpha_rsm,delta=input$delta_rsm,tau1=input$tau1_rsm)
          p3num_rsm[j]<-p3fun_rsm(D=D,theta=thetas_rsm[j],alpha=input$alpha_rsm,delta=input$delta_rsm,tau1=input$tau1_rsm,tau2=input$tau2_rsm)
          p4num_rsm[j]<-p4fun_rsm(D=D,theta=thetas_rsm[j],alpha=input$alpha_rsm,delta=input$delta_rsm,tau1=input$tau1_rsm,tau2=input$tau2_rsm,tau3=input$tau3_rsm)
          p5num_rsm[j]<-p5fun_rsm(D=D,theta=thetas_rsm[j],alpha=input$alpha_rsm,delta=input$delta_rsm,tau1=input$tau1_rsm,tau2=input$tau2_rsm,tau3=input$tau3_rsm,tau4=input$tau4_rsm)
        }
        pdenom<-1+p2num_rsm+p3num_rsm+p4num_rsm+p5num_rsm
        p1<-p1num_rsm/pdenom
        p2<-p2num_rsm/pdenom
        p3<-p3num_rsm/pdenom
        p4<-p4num_rsm/pdenom
        p5<-p5num_rsm/pdenom
        title<-ifelse(input$alpha_rsm==1,"Rating Scale Model (Andrich,1978)","Generalized Rating Scale Model (Muraki,1990)")
        df<-rbind(data.frame(x=thetas_rsm,y=p1,type="P(X=1|theta)"),
                  data.frame(x=thetas_rsm,y=p2,type="P(X=2|theta)"),
                  data.frame(x=thetas_rsm,y=p3,type="P(X=3|theta)"),
                  data.frame(x=thetas_rsm,y=p4,type="P(X=4|theta)"),
                  data.frame(x=thetas_rsm,y=p5,type="P(X=5|theta)"))
        plotly::plot_ly(data=df,x=~x,y=~y,color=~type,type="scatter",mode="lines+markers")%>%
          plotly::layout(title=title,
                         xaxis=list(title="θ"),
                         yaxis=list(title="P(X=m|theta)",range=c(0,1)),
                         legend=list(x=0,y=.99))%>%
          plotly::add_segments(x=min(thetas_rsm,na.rm=TRUE),xend=max(thetas_rsm,na.rm=TRUE),y=.5,yend=.5,line=list(color="gray",size=.1),inherit=FALSE,showlegend=FALSE)
      })
      ##########################################################################################
      # NOMINAL RESPONSE
      ##########################################################################################
      output$nrm_plotly<-plotly::renderPlotly({
        D<-switch(input$D_nrm,"1"=1,"2"=1.702)
        p1num_nrm=1
        for(j in 1:N_nrm){
          p2num_nrm[j]<-p2fun_nrm(D=D,theta=thetas_nrm[j],alpha=input$alpha1_nrm,delta1=input$delta1_nrm)
          p3num_nrm[j]<-p3fun_nrm(D=D,theta=thetas_nrm[j],alpha=input$alpha2_nrm,delta2=input$delta2_nrm)
          p4num_nrm[j]<-p4fun_nrm(D=D,theta=thetas_nrm[j],alpha=input$alpha3_nrm,delta3=input$delta3_nrm)
          p5num_nrm[j]<-p5fun_nrm(D=D,theta=thetas_nrm[j],alpha=input$alpha4_nrm,delta4=input$delta4_nrm)
        }
        pdenom<-1+p2num_nrm+p3num_nrm+p4num_nrm+p5num_nrm
        p1<-p1num_nrm/pdenom
        p2<-p2num_nrm/pdenom
        p3<-p3num_nrm/pdenom
        p4<-p4num_nrm/pdenom
        p5<-p5num_nrm/pdenom
        title<-"Nominal Response Model"
        subtitle<-"Note: Baker's (1992) reparamterization used."
        df<-rbind(data.frame(x=thetas_nrm,y=p1,type="P(X=a|theta)"),
                  data.frame(x=thetas_nrm,y=p2,type="P(X=b|theta)"),
                  data.frame(x=thetas_nrm,y=p3,type="P(X=c|theta)"),
                  data.frame(x=thetas_nrm,y=p4,type="P(X=d|theta)"),
                  data.frame(x=thetas_nrm,y=p5,type="P(X=e|theta)"))
        plotly::plot_ly(data=df,x=~x,y=~y,color=~type,type="scatter",mode="lines+markers")%>%
          plotly::layout(title=title,subtitle=subtitle,
                         xaxis=list(title="θ"),
                         yaxis=list(title="P(X=m|theta)",range=c(0,1)),
                         legend=list(x=0,y=.99))%>%
          plotly::add_segments(x=min(thetas_nrm,na.rm=TRUE),xend=max(thetas_nrm,na.rm=TRUE),y=.5,yend=.5,line=list(color="gray",size=.1),inherit=FALSE,showlegend=FALSE)
      })
      ##########################################################################################
      # MULTIDIMENSIONAL DICHOTOMOUS
      ##########################################################################################
      pjj<-reactive({
        if(input$alpha1_mdm==0 | input$alpha2_mdm==0){stop("Select a discrimination value other than 0 (zero)!")}
        D<-switch(input$D_mdm,"1"=1,"2"=1.702)
        comp<-switch(input$comp_mdm,"1"=1,"2"=2)
        pjj<-matrix(NA,nrow=N_mirtd,ncol=N_mirtd)
        if(comp==1){
          for(j1 in 1:N_mirtd){
            for (j2 in 1:N_mirtd){ pjj[j1,j2]<-Pfun_mirtd(theta1=thetas1[j1],theta2=thetas2[j2],c=input$c_mdm,d=input$d_mdm,alpha1=input$alpha1_mdm,alpha2=input$alpha2_mdm,delta1=input$delta1_mdm,delta2=input$delta2_mdm,D=D) }
          }
        }
        if(comp==2){
          for(j1 in 1:N_mirtd){
            for (j2 in 1:N_mirtd){ pjj[j1,j2]<-Pfun_mirtd2(theta1=thetas1[j1],theta2=thetas2[j2],c=input$c_mdm,d=input$d_mdm,alpha1=input$alpha1_mdm,alpha2=input$alpha2_mdm,delta1=input$delta1_mdm,delta2=input$delta2_mdm,D=D) }
          }
        }
        pjj
      })
      output$plotplotly<-plotly::renderPlotly({
        pjj<-pjj()
        p3D<-plotly::plot_ly(x=thetas1,y=thetas2,z=pjj,width=700,height=700,showscale=FALSE) %>%
          plotly::layout(scene=list(xaxis=list(title=input$nametheta1_mdm),yaxis=list(title=input$nametheta2_mdm),zaxis=list(title="P(Y=1)",range=c(0,1)),camera=list(eye=list(x=1.5,y=-1.5,z=1.5)))) %>%
          plotly::add_surface(opacity=1)
      })
      output$plotcont<-shiny::renderPlot({
        pjj<-pjj()
        c<-input$c_mdm
        d<-input$d_mdm
        alpha1<-input$alpha1_mdm
        alpha2<-input$alpha2_mdm
        delta1<-input$delta1_mdm
        delta2<-input$delta2_mdm
        gamma<--(alpha1*delta1+alpha2*delta2)
        omega1<-acos(alpha1/sqrt(alpha1^2+alpha2^2)) #in radians
        omega2<-acos(alpha2/sqrt(alpha1^2+alpha2^2)) #in radians
        Ai<-sqrt(alpha1^2+alpha2^2) #multidimensional discrimination
        Di<--gamma/Ai
        graphics::contour(thetas1,thetas2,pjj,nlevels=input$nlevels_mdm,xlab=input$nametheta1_mdm,ylab=input$nametheta2_mdm)
        abline(h=0,v=0)
        if(c==0 & d==1){
          cont<-contourLines(thetas1,thetas2,pjj,levels=0.5)
          lines(cont[[1]]$x,cont[[1]]$y,lwd=2)
        }
        if(c>0 & d==1){
          cont<-contourLines(thetas1,thetas2,pjj,levels=(c+1)/2)
          lines(cont[[1]]$x,cont[[1]]$y,lwd=2)
        }
        if(c>0 & d<1){
          cont<-contourLines(thetas1,thetas2,pjj,levels=(c+d)/2)
          lines(cont[[1]]$x,cont[[1]]$y,lwd=2)
        }
        comp<-switch(input$comp_mdm,"1"=1,"2"=2)
        if(comp==1){
          if(alpha2>0){ arrows(Di*cos(omega1),Di*sin(omega1),x1=(Di+Ai)*cos(omega1),y1=(Di+Ai)*sin(omega1),code=2,length=0.1,col="blue") }
          if(alpha2<0){ arrows((Di+Ai)*cos(omega1),-(Di+Ai)*sin(omega1),x1=Di*cos(omega1),y1=-Di*sin(omega1),code=1,length=0.1,col="blue") }
        }
      })
      output$infopersp<-renderPlot({
        c<-input$c_mdm
        d<-input$d_mdm
        alpha1<-input$alpha1_mdm
        alpha2<-input$alpha2_mdm
        Ai<-sqrt(alpha1^2+alpha2^2) #multidimensional discrimination
        pjj<-pjj()
        info<-matrix(NA,nrow=N_mirtd,ncol=N_mirtd)
        for(j1 in 1:N_mirtd){
          for (j2 in 1:N_mirtd){ info[j1,j2]<-(Ai^2)*((pjj[j1,j2]-c)^2)*((d-pjj[j1,j2])^2)/((d-c)^2)*((1-pjj[j1,j2])/pjj[j1,j2]) # (Magis,2013)
          }
        }
        graphics::persp(thetas1,thetas2,info,theta=input$angleinfo1_mdm,phi=input$angleinfo2_mdm,zlim=c(0,max(info)),xlab=input$nametheta1_mdm,ylab=input$nametheta2_mdm,zlab="Information",nticks=5,ticktype="detailed")
      })
      output$infoplotly<-plotly::renderPlotly({
        c<-input$c_mdm
        d<-input$d_mdm
        alpha1<-input$alpha1_mdm
        alpha2<-input$alpha2_mdm
        Ai<-sqrt(alpha1^2+alpha2^2) #multidimensional discrimination
        pjj<-pjj()
        info<-matrix(NA,nrow=N_mirtd,ncol=N_mirtd)
        for(j1 in 1:N_mirtd){
          for (j2 in 1:N_mirtd){ info[j1,j2]<-(Ai^2)*((pjj[j1,j2]-c)^2)*((d-pjj[j1,j2])^2)/((d-c)^2)*((1-pjj[j1,j2])/pjj[j1,j2]) # (Magis,2013)
          }
        }
        plotly::plot_ly(x=thetas1,y=thetas2,z=info,width=700,height=700,showscale=FALSE) %>%
          plotly::layout(scene=list(xaxis=list(title=input$nametheta1_mdm),yaxis=list(title=input$nametheta2_mdm),zaxis=list(title="Information"),camera=list(eye=list(x=-1.5,y=1.5,z=1.5)))) %>%
          plotly::add_surface(opacity=1)
      })
      ##########################################################################################
      # THURSTONIAN
      ##########################################################################################
      output$dichotomous_plotly<-plotly::renderPlotly({
        eta<-seq(-input$pn_eta,input$pn_eta,0.1)
        result<-icc_cfa_thurstonian(eta=eta,gamma=input$pn_gamma,lambda=input$pn_lambda,psi=input$pn_psi)
        df<-data.frame(eta,result)
        plotly::plot_ly(data=df,x=~eta,y=~result,type="scatter",mode="lines+markers")%>%
          plotly::layout(title="",
                         xaxis=list(title="η"),
                         yaxis=list(title="P(η)",range=c(0,1)))%>%
          plotly::add_segments(x=min(eta,na.rm=TRUE),xend=max(eta,na.rm=TRUE),y=.5,yend=.5,line=list(color="gray",size=.1),inherit=FALSE,showlegend=FALSE)
      })
      output$dichotomous_plotly1<-plotly::renderPlotly({
        eta<-seq(-input$pn_eta_ab,input$pn_eta_ab,0.1)
        result<-icc_cfa_thurstonian_bf(eta=eta,gamma=input$pn_gamma_l,lambda_i=input$pn_lambda_i,lambda_k=input$pn_lambda_k,psi_i=input$pn_psi_i,psi_k=input$pn_psi_k)
        df<-data.frame(eta,result)
        plotly::plot_ly(data=df,x=~eta,y=~result,type="scatter",mode="lines+markers")%>%
          plotly::layout(title="",
                         xaxis=list(title="η"),
                         yaxis=list(title="P(η)",range=c(0,1)))%>%
          plotly::add_segments(x=min(eta,na.rm=TRUE),xend=max(eta,na.rm=TRUE),y=.5,yend=.5,line=list(color="gray",size=.1),inherit=FALSE,showlegend=FALSE)
      })
      output$dichotomous_plotly2<-plotly::renderPlotly({
        eta<-seq(-input$pn_eta_abl,input$pn_eta_abl,0.1)
        result<-icc_cfa_thurstonian_l(eta=eta,alpha=input$pn_alpha,beta_i=input$pn_beta_i,beta_k=input$pn_beta_k)
        df<-data.frame(eta,result)
        plotly::plot_ly(data=df,x=~eta,y=~result,type="scatter",mode="lines+markers")%>%
          plotly::layout(title="",
                         xaxis=list(title="η"),
                         yaxis=list(title="P(η)",range=c(0,1)))%>%
          plotly::add_segments(x=min(eta,na.rm=TRUE),xend=max(eta,na.rm=TRUE),y=.5,yend=.5,line=list(color="gray",size=.1),inherit=FALSE,showlegend=FALSE)
      })
      ##########################################################################################
      #
      ##########################################################################################
    }
  )
}

