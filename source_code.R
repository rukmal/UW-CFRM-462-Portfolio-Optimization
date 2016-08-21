############
# Imports
############

library(boot)
library(captioner)
library(corrplot)
library(IntroCompFinR)
library(knitr)
library(lubridate)
library(PerformanceAnalytics)
library(tseries)
library(xlsx)

############
# Plot region and support library setup
############

# Plot region setup
default.par <- par()
par(mgp = c(0, 1, 0), mar = c(4.5, 4, 1, 1), mfrow = c(1,1))

# Captioner setup
figures <- captioner(prefix = "Figure")
tables <- captioner(prefix = "Table")


############
# Program Constants
############

# Constants
asset.names <- c("VFINX", "VEURX", "VEIEX", "VBLTX", "VBISX", "VPACX")
asset.colors <- c("lightcyan4", "lightcoral", "lightseagreen", "lightskyblue", "lightsalmon", "lightslateblue")
export.pricedata.name <- "price_data.xlsx"
risk.free <- 0.0004167 # Risk free rate
w0 <- 100000 # Intial investment for value at risk calculations

# Macroeconomic Events
dates.macro <- c(decimal_date(ymd("2012-04-15")), decimal_date(ymd("2015-08-21")))
names(dates.macro) <- c("Europe Credit Crisis", "China Stock Market Crash")
dates.macro.colors <- c("steelblue3", "violetred2")

# Defining date range
start.date <- "2011-06-01"
end.date <- "2016-06-30"


############
# Helper Functions
############

# Cross Reference Management
############################

# Function to add a figure caption to figures object
figures.add <- function (...) {
	figures(..., display = FALSE)
}

# Function to add a table caption to tables object
tables.add <- function (...) {
	tables(..., display = FALSE)
}

# Function to cite a figure
figures.cite <- function (figure.ref) {
	figures(figure.ref, display = "cite")
}

# Function to cite a table
tables.cite <- function (table.ref) {
	tables(table.ref, display = "cite")
}

# Historic Price Gathering
############################

# Function to get adjusted close prices from online services
# See ??get.hist.quote for more info/options
get.adjclose <- function (instrument, interval, start = "1800-01-01", end = Sys.date()) {
	prices <- get.hist.quote(instrument = instrument, quote = c("AdjClose"), start = start, end = end, compression = interval, retclass = "zoo")
	prices
}

# Plot Panels
############################

# Plot Panel for plotting time series data (general)
panel.time.plots <- function (...) {
	lines(...)
	grid()
	abline(h = 0)
}

# Plot panels for plotting time series, with significant news events illustrated on the graph
panel.time.plots.dates <- function (...) {
	abline(v = dates.macro, col = dates.macro.colors, lty = 2)
	panel.time.plots(...)
}

# Miscellaneous
############################

# Function to format and print confidence intervals
# interval arg: c(lower, upper)
stat.ci.print <- function (interval, digits = 6) {
	arg <- paste("[%1.", digits, "f, %1.", digits, "f]", sep = "")
	sprintf(arg, interval[1], interval[2])
}

# Function to format and print a generated (normal) confidence interval, given a
# level of tolerance, mean and standart deviation.
stat.ci.generator <- function (mean, sd, ci, digits = 6) {
	lower <- mean - sd * qnorm(ci / 2)
	upper <- mean + sd * qnorm(ci / 2)
	stat.ci.print(c(lower, upper), digits)
}

# Function to format a percentage to a given number of digits, given a decimal percentage
format.perc <- function (perc, digits = 3) {
	arg <- paste("%2.", digits, "f%%", sep = "")
	sprintf(arg, abs(perc) * 100)
}

# Function to calculate Value At Risk, given an initial investment, mean, sd and risk level
var.calculate.norm <- function (mean, sd, risk, initial = w0) {
	q <- mean + sd * qnorm(risk)
	val.at.risk <- (exp(q) - 1) * initial
	abs(val.at.risk)
}

var.calculate.historical <- function (returns, risk, initial = w0) {
	q <- quantile(returns, probs = risk)
	val.at.risk <- (exp(q) - 1) * initial
	abs(val.at.risk)
}

# Bootstrap functions
############################

# Function to get the target value (t0) from a bootstrap object
boot.target <- function (x) {
	x$t0
}

# Function to compute and return the standard deviation of the values from a bootstrap process
boot.se <- function (x) {
	sd(x$t)
}

# Function to extract upper and lower confidence intervals from a boot.ci object
# See ?boot.ci for more information
boot.ci.extract.normal <- function (x) {
	data <- as.numeric(x$normal)
	lower <- data[2]
	upper <- data[3]
	c(lower, upper)
}

# Function to calculate the Sharpe Ratio for a bootstrap process
asset.boot.sr <- function (x, idx, rf = risk.free) {
	data <- x[idx]
	sr <- (mean(data) - rf) / sd(data)
	sr
}

asset.boot.var <- function(x, idx, risk, initial = w0) {
	mean <- mean(x[idx])
	sd <- sd(x[idx])
	q <- mean + sd * qnorm(risk)
	var <- (exp(q) - 1) * initial
	abs(var)
}

############
# Downloading and cleaning up ETF historical price data
############

# Getting adjusted close prices for each of the securities
vfinx.adjclose <- get.adjclose("VFINX", "m", start.date, end.date)
veurx.adjclose <- get.adjclose("VEURX", "m", start.date, end.date)
veiex.adjclose <- get.adjclose("VEIEX", "m", start.date, end.date)
vbltx.adjclose <- get.adjclose("VBLTX", "m", start.date, end.date)
vbisx.adjclose <- get.adjclose("VBISX", "m", start.date, end.date)
vpacx.adjclose <- get.adjclose("VPACX", "m", start.date, end.date)

# Changing class of index to yearmon, which is ideal for monthly data
index(vfinx.adjclose) <- as.yearmon(index(vfinx.adjclose))
index(veurx.adjclose) <- as.yearmon(index(veurx.adjclose))
index(veiex.adjclose) <- as.yearmon(index(veiex.adjclose))
index(vbltx.adjclose) <- as.yearmon(index(vbltx.adjclose))
index(vbisx.adjclose) <- as.yearmon(index(vbisx.adjclose))
index(vpacx.adjclose) <- as.yearmon(index(vpacx.adjclose))

# Merging price data
prices <- merge(vfinx.adjclose, veurx.adjclose, veiex.adjclose, vbltx.adjclose, vbisx.adjclose, vpacx.adjclose)
colnames(prices) <- asset.names


############
# Plotting historical price data for each of hte ETFs
############

figures.add(name = "time_plot_price", caption = "Timeplot of ETF prices")
plot(prices, xlab = "Time", panel = panel.time.plots.dates, col = asset.colors, main = "")


############
# Computing Continously Compounded Returns
############

# Computing continuously compounded returns, and casting to different types
# for function compatibility
ret.z <- diff(log(prices))  # Type 'zoo'
ret.mat <- coredata(ret.z)  # Type 'matrix'
ret.df <- as.data.frame(coredata(ret.z))  # Type 'dataframe'

# Computing simple returns
ret.simple.z <- exp(ret.z) - 1

# Check if output Excel file exists, if so delete
if (file.exists(export.pricedata.name)) {
	file.remove(export.pricedata.name)
}

# Loop through each asset, and export price, simple and geometric return
# to separate sheets in an Excel file
for (i in seq_along(asset.names)) {
	simple.ret = exp(ret.df[,i]) - 1
	export.data.names <- c("Adjusted Close", "Simple Return", "Continuously Compounded Return")
	export.data = data.frame(prices[,i][-(1:1)], simple.ret, ret.df[,i])
	rownames(export.data) <- index(prices)[-(1:1)]
	colnames(export.data) <- export.data.names
	write.xlsx(export.data, file = export.pricedata.name, sheetName = asset.names[i], append = TRUE)
}


############
# Time plot of historical ETF continously compounded returns
############

plot(ret.z, xlab = "Time", panel = panel.time.plots, col = asset.colors, main = "")
figures.add(name = "time_plot_returns", caption = "Timeplot of ETF continuously compounded returns")


############
# Time plot of growth rate of $1 invested at continuos rate of return of each ETF
# i.e. Equity curve for ETF returns
############

chart.CumReturns(ret.z, col = asset.colors, legend.loc = "topleft", wealth.index = TRUE, ylab = "Wealth ($)", xlab = "Time", main = "")
figures.add(name = "time_plot_equity_curve", caption = "Growth of $1 investment")


############
# ETF univariate statistics
############

univariate.stats <- c(mean, var, sd, skewness, kurtosis)
univariate.stats.names <- c("Mean", "Variance", "Std Dev", "Skewness", "Excess Kurtosis")
asset.univariate.stats <- data.frame()

# Looping through each statistic, and applying the measure to each set of ETF returns
for (statistic in univariate.stats) {
	statistic.result <- apply(ret.z, 2, statistic)
	asset.univariate.stats <- rbind(asset.univariate.stats, statistic.result)
}

# Updating column and row names
colnames(asset.univariate.stats) <- asset.names
rownames(asset.univariate.stats) <- univariate.stats.names

tables.add(name = "asset_univariate_stats", caption = "Univariate Statistics for each of the ETFs")
kable(asset.univariate.stats, caption = tables.cite("asset_univariate_stats"))


############
# Four panel plot for each ETF in the dataset
############

figures.add(name = "graphical_descriptive_stats", caption = "Graphical Descriptve Statistics for Monthly CC Returns of ETFs")
apply(ret.z, 2, fourPanelPlot)

############
# ETF Returns single boxplot
############

figures.add(name = "cc_returns_boxplot", caption = "Box Plot of CC Monthly Returns of the ETFs")
ret.ordered <- apply(ret.df, 2, sort, method = "shell")
boxplot(ret.ordered, width = rep(1,6), plot = TRUE, col = asset.colors, ylab = "Continuously Compounded Returns", xlab = "Exchange Traded Fund (ETF) Name")


############
# Plotting Sample Autocorrelation for each ETF
############

par(mfrow = c(3,2))

sacf = data.frame()

for (i in seq(1, length(asset.names))) {
	par(mar = c(2, 4.5, 1, 1))
	asset.acf <- acf(ret.df[i], lag.max = 10, ylab = paste(asset.names[i], " ", "ACF"), col = asset.colors[i])
	sacf <- rbind(sacf, asset.acf$acf[ , ,1])
}

rownames(sacf) <- c(asset.names)
colnames(sacf) <- paste("Lag ", seq(0, 10, 1))

# Resetting plot attributes
par(default.par)

############
# housekeeping
############

N <- length(ret.z$VFINX)

# First, doing some cleaning on the univariate stats object for easier access
asset.univar.stats <- data.frame(t(data.matrix(asset.univariate.stats)))
colnames(asset.univar.stats) <- c("mean", "var", "sd", "eskew", "kurt")

############
# Standard Erorrs for Mean and Standard Deviation
############

# Computing SE and 95% CI of the mean
asset.se.mean <- asset.univar.stats$sd / sqrt(N)
asset.se.mean.perc <- asset.se.mean / asset.univar.stats$mean
# Creating and formatting mean SE table, with CI and percentage SE
asset.se.mean.table <- data.frame(asset.univar.stats$mean, asset.se.mean, format.perc(asset.se.mean.perc), stat.ci.generator(asset.univar.stats$mean, asset.se.mean, 0.05))
rownames(asset.se.mean.table) <- asset.names
colnames(asset.se.mean.table) <- c("Mean", "Mean SE", "Mean SE (%)", "95% Confidence Interval")
tables.add(name = "asset_mean_se", caption = "Standard Errors and Confidence Intervals for ETF Return Means")
kable(asset.se.mean.table, caption = table("asset_mean_se"))

# Computing SE and 95% CI of the SD
asset.se.sd <- asset.univar.stats$sd / sqrt(N * 2)
asset.se.sd.perc <- asset.se.sd / asset.univar.stats$sd
# Creating and formatting SD SE table with CI and percentage SE
asset.se.sd.table <- data.frame(asset.univar.stats$sd, asset.se.sd, format.perc(asset.se.sd.perc), stat.ci.generator(asset.univar.stats$sd, asset.se.sd, 0.05))
rownames(asset.se.sd.table) <- asset.names
colnames(asset.se.sd.table) <- c("Std Dev", "Std Dev SE", "Std Dev SE (%)", "95% Confidence Interval")
tables.add(name = "asset_se_se", caption = "Standard Errors and Confidence Intervals for ETF Return Standard Deviations")
kable(asset.se.sd.table, caption = table("asset_sd_se"))


############
# Sharpe Ratios, with Bootstrapped Mean and SE
############

# Number of simulations
sims <- 9999

# Computing Sharpe Ratios
asset.univar.sr <- (asset.univar.stats$mean - risk.free) / asset.univar.stats$sd

# Estimating Sharpe Ratios (mean and se) with Bootsrtap
asset.univar.sr.boot <- apply(ret.df, 2, boot, statistic = asset.boot.sr, R = sims)
# Extracting data from bootstrap simulations
asset.univar.sr.boot.x <- as.numeric(lapply(asset.univar.sr.boot, boot.target))
asset.univar.sr.boot.se <- as.numeric(lapply(asset.univar.sr.boot, boot.se))
asset.univar.sr.boot.perc <- asset.univar.sr.boot.se / asset.univar.sr.boot.x

asset.univar.sr.table <- data.frame(asset.univar.sr, asset.univar.sr.boot.x, asset.univar.sr.boot.se, format.perc(asset.univar.sr.boot.perc))

colnames(asset.univar.sr.table) <- c("Sharpe Ratio (A)", "Sharpe Ratio (B)", "Sharpe Ratoio SE (B)", "Sharpe Ratio SE % (B)")
rownames(asset.univar.sr.table) <- asset.names
tables.add(name = "asset_sr_stats", caption = "Monthly Sharpe Ratios with Bootstrap-estimated Standard Errors (Key: A - Analytical, B - Bootstrap)")
kable(asset.univar.sr.table)


############
# Annualizing Results
############

# Annualizing risk-free rate
risk.free.a <- risk.free * 12

# Annualizing stats
asset.univar.a.mean <- asset.univar.stats$mean * 12
asset.univar.a.sd <- asset.univar.stats$sd * sqrt(12)
asset.univar.a.sr <- (asset.univar.a.mean - risk.free.a) / asset.univar.a.sd

# Creating table to be displayed
asset.univar.a.table <- data.frame(asset.univar.a.mean, asset.univar.a.sd, asset.univar.a.sr)
colnames(asset.univar.a.table) <- c("Annualized Mean", "Annualized Std Dev", "Annualized Sharpe Ratio")
rownames(asset.univar.a.table) <- asset.names

tables.add(name = "asset_annualized_stats", caption = "Annualized Mean, Standard Deviation and Sharpe Return for the ETFs")
kable(asset.univar.a.table)


############
# Risk Return tradeoff graph
############

figures.add(name = "asset_risk_return_tradeoff", caption = "Risk-Return Tradeoff for each of the ETFs")

# Creating the plot
plot(x = asset.univar.stats$sd, y = asset.univar.stats$mean, ylab = "Expected Return", xlab = "Standard Deviation", main = "", type = "n")
grid()
text(x = asset.univar.stats$sd, y = asset.univar.stats$mean, labels = asset.names, offset = 1, pos = c(rep(4,2), 2, rep(4, 3)), col = asset.colors)
points(x = asset.univar.stats$sd, y = asset.univar.stats$mean, col = asset.colors, pch = 4)
abline(h = 0, lty = 2, col = "lightsteelblue4")


############
# Correlation Matirx
############

# Computing correlation matrix
rho.mat <- cor(ret.df)
# Computing covariance matirx
cov.mat <- cov(ret.df)

# Plotting the correlation matrix
corrplot.mixed(rho.mat, upper = "ellipse", col = colorRampPalette(c("white","cadetblue2", "cadetblue4"))(20), tl.col = asset.colors)
figures.add(name = "asset_correlation_plot", caption = "Correlation Plot for the Cross Cross-Correlation between ETFs")

# Displaying the correlation matrix
tables.add(name = "asset_corelation_matrix", caption = "Cross-Correlation Matrix of ETFs")
kable(rho.mat, caption = tables("asset_correlation_matrix"))


############
# Value at Risk
############

# Calculating 1% and 5% VaR assuming the normal distriution (Annual),
# all calculations after this are for monthly returns
asset.var.01.annual <- var.calculate.norm(asset.univar.a.mean, asset.univar.a.sd, 0.01)
asset.var.05.annual <- var.calculate.norm(asset.univar.a.mean, asset.univar.a.sd, 0.05)

# Calculating 1% and 5% VaR assuming the normal distribution
asset.var.01 <- var.calculate.norm(asset.univar.stats$mean, asset.univar.stats$sd, 0.01)
asset.var.05 <- var.calculate.norm(asset.univar.stats$mean, asset.univar.stats$sd, 0.05)

# Calculating 1% and 5% VaR using emperical quantiles
asset.var.01.emperical <- apply(ret.df, 2, var.calculate.historical, risk = 0.01)
asset.var.05.emperical <- apply(ret.df, 2, var.calculate.historical, risk = 0.05)

# Calculating bootstrapped SE and 95% confidence intervals
asset.var.01.boot <- apply(ret.df, 2, boot, statistic = asset.boot.var, R = sims, risk = 0.01)
asset.var.05.boot <- apply(ret.df, 2, boot, statistic = asset.boot.var, R = sims, risk = 0.05)

# Extracting SE from bootstrap data
asset.var.01.boot.se <- as.numeric(lapply(asset.var.01.boot, boot.se))
asset.var.05.boot.se <- as.numeric(lapply(asset.var.05.boot, boot.se))

# Calculating 95% confidence intervals for each
asset.var.01.boot.95ci <- lapply(lapply(asset.var.01.boot, boot.ci, conf = 0.95, type = c("norm")), boot.ci.extract.normal)
asset.var.05.boot.95ci <- lapply(lapply(asset.var.05.boot, boot.ci, conf = 0.95, type = c("norm")), boot.ci.extract.normal)

# Prettyprinting confidence intervals
asset.var.01.boot.95ci.print <- unlist(lapply(asset.var.01.boot.95ci, stat.ci.print, digits = 2))
asset.var.05.boot.95ci.print <- unlist(lapply(asset.var.05.boot.95ci, stat.ci.print, digits = 2))

# Creating tables with stats for Value at Risk at each level of risk
asset.var.01.table <- data.frame(asset.var.01, asset.var.01.emperical, asset.var.01.annual, asset.var.01.boot.se, asset.var.01.boot.95ci.print)
asset.var.05.table <- data.frame(asset.var.05, asset.var.05.emperical, asset.var.05.annual, asset.var.05.boot.se, asset.var.05.boot.95ci.print)

# Changing col and row names
colnames(asset.var.01.table) <- c("1% VaR (A)", "1% Var (E)", "1% VaR (Annual, A)", "1% VaR SE (B, A)", "1% VaR SE 95% CI (B, A)")
colnames(asset.var.05.table) <- c("5% VaR (A)", "5% Var (E)", "5% VaR (Annual, A)", "5% VaR SE (B, A)", "5% VaR SE 95% CI (B, A)")
rownames(asset.var.01.table) <- asset.names
rownames(asset.var.05.table) <- asset.names

# Adding captions and displaying tables
tables.add(name = "asset_var_01", caption = "1% Value at Risk Analysis for each ETF (Key: A: Analytical Normal, E - Emperical, B - Bootstrap)")
tables.add(name = "asset_var_01", caption = "5% Value at Risk Analysis for each ETF (Key: A: Analytical Normal, E - Emperical, B - Bootstrap)")
kable(asset.var.01.table, caption = tables("asset_var_01"), digits = 2)
kable(asset.var.05.table, caption = tables("asset_var_05"), digits = 2)


############
# Rolling Analysis
############
