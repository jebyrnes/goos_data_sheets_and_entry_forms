#### ---------------------- POWER ANALYSIS FOR GOMON DATA TYPE ----------------------------------- ####
# AIM:
# The script calculates statistical power to detect changes of specified magnitude in species
# abundance following one of the GOMON sampling designs. Changes in species abundances can simulate
# declining (or increasing) trends through time or pulse events according to a Before-After design.
# The simplest design includes one site sampled in one year with n quadrats. Only trends can be
# simulated for input data with one sampling year. There must be at least two sampling years in the
# input data to simulate pulse events. This is required to estimate the variance among years that is
# then used to simulate timeserirs for the Before and After periods relative to the event disturbance.
#
# GENERAL APROACH
# Basic parameters (variances and mean abundances) are estimated from the input data. Monte Carlo
# simulations are then implemented by generating new data from the estimated parameters and by imposing
# impacts of known magnitude to the simulated data (Effect Size), which are then analysed to recover
# statistically significant (or lack thereof) effects. For the simplest design invovling trends with
# no blocks in the input data, variance among replicated quadrats and mean abundance are esitmated
# directly from the data. For simple designs with more than one year of sampling, variance among
# replicated quadrats and mean abundance are estimated from a linear model. For more complex data
# structures that involve random effects (Blocks), variance and mean abundance are estimated from
# mixed-effect models using REML (through function lmer in package lme4). Monte Carlo Simulations
# based on REML variance estimates still use mixed effect models for trends, whereas pulse events
# are modeled in an ANOVA framework (using function gad in package GAD).
#
# STATISTICAL POWER: it is quantifed as the proportion of significant tests over the number
# of Monte Carlo simulations performed.
#
# EFFECT SIZES (ES):
# ES are imposed as percentage reductions of the estimated mean value of the response variable
# at the first year of sampling (the b0 coefficient from lmer). Statistical power can also be
# calculated for effect sizes that cause an increase in the repsonse variable. In such cases the ES
# must be indicated as negative values in the grid of simulated parameters (see FUNCTION
# ARGUMENTS below).
#
# TREND: a trend is imposed over the entire simulated timeseries as Trend = b0*-ES*Year
#
# EVENT: an event impact assumes there is a Before vs. After period and it is modeled as:
# BA_i = b0*0.5*EffSize for the Before period and BA_i = b0*-0.5*EffSize for the After period
# (i.e. half EF is added to simulated Before data and half EF is removed from After data).
#
# DATA SIMULATION: data are generated using a linear model of the form
# Yijk = b0 + BA_i + VARIANCE.ESTIMATES_j + RESIDUAL_k; variance estimates depend on design and
# are simulated by sampling a normal distribution with MU = 0 and SD equal to the standard
# deviation estimated from the data. The function allows exploring the effects of varying
# number of years, blocks, quadrats for different effect sizes on statistical power. These
# quantities are specified in a sim.grid dataset that is passed to the function (see FUNCTION
# ARGUMENTS below).
#
# SAMPLING DESIGNS:
# there are three sampling designs that can be simulated and specified through formulae;
# each design can accomodate trend or event impacts for an individual sampling site:
# 1) a simple sampling design consisting in one site sampled over time (one or multiple
# years) with n Quadrats at each time;
# 2) a nested sampling design with Blocks nested within Years (different blocks in
# different years) and Quadrats nested within Blocks;
# 3) a crossed sampling design where Year and Blocks are crossed factors (the same Blocks are
# sampled over time).
# For event impacts, Year is nested in BA and Block can be either nested or crossed with Year and BA.
#
# NATURE OF FACTORS: Year is fixed when simulating trends, whereas it is random when simulating
# events; Block is always random; the Before vs. After constrast in event simulations is a fixed factor.
# In the event design where Block is crossed with Year and BA there is no direct test for the BA term.
# A test is constructed if other terms can be eliminated from the model (see specifications in the function) 
#
# FORMULAE: trend and event impacts for the three designs are simulated using the formulae:
# 1) Simple design with one site sampled over multiple years:
# Trend: ~ Year
# 2) Blocks nested in Years:
# Trend: ~ Year + (1|Block) or ~ Year + (Year|Block) depending on whether only intercepts
# or both intercepts and slopes are allowed to vary.
# Event, nested ~ 1 + (1|Year/Block)
# Event, crossed ~ 1 + (1|Year) + (1|Block) + (1|Year:Block). It is important to include
# (1|Year:Block) to estimate the Year x Block interaction variance component.
#
# FUNCTION ARGUMENTS:
# fn - formula
# df - data frame with empirical data. It must include columns Year, Site and a response
# variable.
# sim.grid - a grid of simulated parameters (number of years, blocks, quadrats and effect sizes),
# e.g., sim.grid <- expand.grid(Year=2:3, Block=5:6, Quadrat=4:5, EffSize=seq(0, 0.5, by=0.1)) 
# n.sim - the number of simulations to compute power.
#
# NOTE: for event impacts, the lmer formula in argument fm is converted internally into an
# equivalent ANOVA model.
#
# VALUE: the function returns a list where the first element is a data frame of power estimates
# for the simulated design, whereas the second element is a ggplot2 object to plot the results;
# plots have statistical power on the Y axis and effect size on the X axis; for the simplest design
# invovling a trend of a single effect size, the plot uses Year for the x axis.

#### -------------------------------------------------------------------------------------------- ####

require(dplyr)
require(GAD) # to fit ANOVA model for event power tests
require(lme4) # to estimate variances
require(lmerTest) # to obtain p values
require(sjmisc) # to veryfy design
require(ggplot2) # for plotting
require(fishualize) # nice palettes

# multithreading
require(foreach, quietly=T)
require(doMC, quietly=T)
registerDoMC(cores=15)


gomon_power <- function(fm, df, sim.grid, eff.type=c("trend","event"), n.sim=99, ...) {
	
	# Check for number of years and blocks
	n.yrs <- length(unique(df$Year))
	n.blocks <- length(unique(df$Block))
	n.quads <- nrow(df)/(n.yrs*n.blocks)
	
	if(n.yrs==1&eff.type=="event") stop("At least two years are required in the input dataset to model event impacts")
	
	# Recode levels of variables in df to ensure they are appropriate to derive variance estimates
	# for the specified design. For trend impacts, Year is converted to a continuous variable starting
	# from 1 (the first reported year) and progressing with natural numbers by accounting for missing
	# years (e.g., Years 2019, 2021 and 2023 will become levels 1,3,5). Block is converted to a factor
	# with the same level identifiers across years. This formulation is appropriate to specify both
	# nested and crossed relationships between Year and Block. For event impacts, Year is converted to
	# a factor and Block is converted as in the trend scenario.
	
	if(eff.type=="trend") { 
		
		df <- df %>%
				mutate(
						Year = (as.numeric(Year)-min(as.numeric(Year))) + 1,
						Block = as.factor(rep(1:n.blocks, each=n.yrs*n.quads))
				)
		
	} else{
		
		df <- df %>%
				mutate(
						Year = as.factor(Year),
						Block = as.factor(rep(1:n.blocks, each=n.yrs*n.quads))
		)
	}
	
	str <- strsplit(as.character(fm), split=c('* ~'))
	resp <- str[[2]][1]
	
	# get variance among quadrats directly from the data if there is only one year and no random blocks;
	# use lm if there are multiple years and no radom blocks;
	# use lmer if there are random blocks
	if(n.yrs==1&n.blocks==1) {
		
		b0 <- mean(unlist(df[,resp]))
		sd.quad <- sd(unlist(df[,resp]))
		sd.block <- NA
		
	} else if(n.yrs>1&n.blocks==1&eff.type=="trend") {
		
		m1.lm <- lm(fm, data=df)
		b0 <- coef(m1.lm)[1]
		sd.quad <- summary(m1.lm)$sigma
		sd.block <- NA
		
	} else if(n.blocks>1|eff.type=="event") { 
		
		# fit the model with lmer and extract parameter estimates if there are
		# random blocks
		m1 <- do.call("lmer", list(fm, REML=T, data=df))
		#class(m1)
		#if(any(class(m1)%in%c("lmerMod", "lmerTest","lmerModLmerTest")))
		
		mf <- m1@frame
		mu <- mean(mf[,1])
		coef.m1 <- fixef(m1)
		b0 <- coef.m1[1]
		
		# estimate standard deviation components for different designs with Blocks
		# and type of impact:
		# i) trend over years, random intercepts among blocks: ~ Year + (1|Block)
		# ii) trend over years, random intercepts and slope among Blocks: ~ Year + (Year|Block)
		# iii) event, Blocks nested in Year, both as random effects ~ 1 + (1|Year/Block)
		# iv) event, Year and Block crossed random effects ~ 1 + (1|Year) + (1|Block) + (1|Year:Block)
		
		var.est <- data.frame(VarCorr(m1))
		# residual standard deviation
		sd.quad <- var.est[which(var.est$grp%in%"Residual"),"sdcor"]
		# standard deviation among Blocks
		sd.block.tmp <- var.est[which(var.est$grp%in%c("Block","Block:Year","Year:Block")&
								var.est$var1%in%"(Intercept)"),"sdcor"]
		
		if(length(sd.block.tmp) == 1) {
			
			sd.block <- sd.block.tmp
			
		} else {
			
			if(eff.type=="trend") {
				
				sd.block <- sd.block.tmp[1]
				
				# determine if there is variance among slopes
				# (correlation between blocks and years)
				if(any(var.est[,2]%in%"Year")) {
					
					block.year.cor <- sd.block.tmp[2]
					
				}
				
			} else {
				
				sd.block <- sd.block.tmp[2]
				sd.yrblock <- sd.block.tmp[1]
				
			}
			
		}
		
		if(any(var.est[,1]%in%"Year")) {
			
			sd.year <- var.est[which(var.est$grp%in%c("Year")&
									var.est$var1%in%"(Intercept)"),"sdcor"]		
			
		}
		
	}
	
	# start simulation separately for each effect size
	out <- foreach(k = 1:nrow(sim.grid), .combine="rbind") %dopar% {
		
		sim.k <- sim.grid[k,]
		
		# Number of years
		#n.yrs <- length(unique(sim.k[,"Year"]))
		# NUmber of observations per year
		n.obs.yr <- sim.k[,"Quadrat"]*sim.k[,"Block"]		
		
		if(eff.type=="trend") {
			
			# model design 
			model.des <- expand.grid(
							Year = 1:sim.k[1,"Year"],
							Block = 1:sim.k[1,"Block"],
							Quadrat = 1:sim.k[1,"Quadrat"],
							b0=as.vector(b0),
							EffSize = sim.k[1,"EffSize"]
					) %>%
					mutate(Block=as.factor(Block)) %>%
					arrange(Year,Block,Quadrat)
			
			pval <- foreach(i = 1:n.sim, .combine="c") %do% {
				
				cat('Doing sim ', i, ' of ', n.sim, ": ",
						' Year ', sim.k[1,"Year"], ' of ', max(sim.grid$Year), " |",
						' Block ', sim.k[1,"Block"], ' of ', max(sim.grid$Block), " |",
						' Quadrat ', sim.k[1,"Quadrat"], ' of ', max(sim.grid$Quadrat), " |",
						' EffSize ', sim.k[1,"EffSize"], " (",
						which(unique(sim.grid$EffSize)%in%sim.k[1,"EffSize"]), ' of ',
						length(unique(sim.grid$EffSize)), ")",						
						'\n', sep = '')
				
				# Simulate data using levels in sim.grid
				sim.dat <- model.des %>%
						mutate(
								Trend = b0*-EffSize*Year,
								Block = as.factor(Block),
								) %>%
						group_by(Year, Block) %>%
						mutate(Bj = ifelse(!is.na(sd.block), rnorm(1, 0, sd.block), 0)) %>%
						group_by(Year, Block, Quadrat) %>%
						mutate(Qk=rnorm(1, 0, sd.quad)) %>%
						ungroup() %>%
						mutate(Yijk = b0 + Trend + Bj + Qk) %>%
						rename(!!sym(resp) := Yijk)
				
				# analysis: use lm if the input data does not include random effects (blocks),
				# otherwise use lmer
				if(n.blocks==1) {
					
					m.sim <- do.call("lm", list(fm, data=sim.dat))			
					
				} else {
					
					m.sim <- do.call("lmer", list(fm, REML=T, data=sim.dat))
					
				}
				
				coef.msim <- (summary(m.sim))$coefficients
				
				if (any(rownames(coef.msim)%in%"Year")) {
					
					# for negative impacts (eff.size>0) consider only significant negative trends;
					if(sim.k[1,"EffSize"]>0&coef.msim[which(rownames(coef.msim)=="Year"),"Estimate"]<0) {
						
						pval <- coef.msim[which(rownames(coef.msim)%in%"Year"),"Pr(>|t|)"]
						
					} else if(sim.k[1,"EffSize"]<0&coef.msim[which(rownames(coef.msim)=="Year"),"Estimate"]>0) {
						# for positive impacts (eff.size<0) consider only significant positive trends;
						pval <- coef.msim[which(rownames(coef.msim)%in%"Year"),"Pr(>|t|)"]
						
					} else {
						
						pval <- 1
						
					}
					
				} else {
					
					stop("Year must be included in the fixed part of the model to evaluate trends")
					
				}
				
			}	
			
			term <- "Year"
			
		}	
		
		if(eff.type=="event") {
			
			if(!any(var.est[,1]%in%"Year")) stop("Year must be a random factor to model event effects")
			
			block.check <- grepl("Block",str[[3]], fixed=T) # determine if Block is present in formula
			
			if(!block.check) {
				
				fm1 <- as.formula(paste(resp, " ~", paste(" (ba/year)")))	
				
			} else {
				
				# If Block is present determine if it nested or crossed with Year
				
				block.nested <- grepl("Year/Block",str[[3]], fixed=T) # determine if Block is nested or crossed with Year
				
				# modify formula to inclue Before vs After effect and
				# include Year and Block as nested or crossed random factors
				if(block.nested) {
					
					# for lmer instead of GAD
					#fm1 <- as.formula(paste(resp, " ~", paste(" BA + (1|Year/Block)")))
					fm1 <- as.formula(paste(resp, " ~", paste(" ba/year/block")))		
					
				} else { # if Year and Block are crossed random factors
					
					# if Year and Block are crossed check that fm includes the appropriate terms
					# to estimate the variance components for the interaction
					block.crossed <- grepl("1 + (1 | Year) + (1 | Block) + (1 | Year:Block)",
							str[[3]], fixed=T)
					
					# check for Block:Year in addition to Year:Block in model specification
					if(!block.crossed) {
						
						block.crossed <- grepl("1 + (1 | Year) + (1 | Block) + (1 | Block:Year)",
								str[[3]], fixed=T)
						
					}
					
					if(!block.crossed) stop("The lmer formula for Year and Block as crossed random factors should be specified as \n1 + (1 | Year) + (1 | Block) + (1 | Block:Year)")
					# for lmer instead of GAD
					#fm1 <- as.formula(paste(resp, " ~", paste(" BA + (1|Year) + (1|Block) + (1|Year:Block)")))
					fm1 <- as.formula(paste(resp, " ~", paste(" (ba/year)*block")))	
					
				}
				
			}
			# model design 
			model.des <- expand.grid(
							BA = c("Before","After"),
							Year = 1:sim.k[1,"Year"],
							Block = 1:sim.k[1,"Block"],
							Quadrat = 1:sim.k[1,"Quadrat"],
							b0=as.vector(b0),
							EffSize = sim.k[1,"EffSize"]
					) %>%
					mutate(
							BA = factor(BA, levels=c("Before","After")),
							Year=as.factor(Year),
							Block = as.factor(Block),
					) %>%
					arrange(BA, Year,Block,Quadrat)
			
			pval <- foreach(i = 1:n.sim, .combine="c") %do% {
				
				cat('Doing sim ', i, ' of ', n.sim, ": ",
						' Year ', sim.k[1,"Year"], ' of ', max(sim.grid$Year), " |",
						' Block ', sim.k[1,"Block"], ' of ', max(sim.grid$Block), " |",
						' Quadrat ', sim.k[1,"Quadrat"], ' of ', max(sim.grid$Quadrat), " |",
						' EffSize ', sim.k[1,"EffSize"], " (",
						which(unique(sim.grid$EffSize)%in%sim.k[1,"EffSize"]), ' of ',
						length(unique(sim.grid$EffSize)), ")",						
						'\n', sep = '')
				
				# Simulate data using levels in sim.grid
				# Block is not included
				if(!block.check) {
					
					sim.dat <- model.des %>%
							mutate(BAi = case_when(
											
											BA=="Before" ~ b0*0.5*EffSize,
											BA=="After" ~ b0*-0.5*EffSize,
											TRUE ~ as.numeric(EffSize)									
									)
									
							) %>%
							group_by(BA, Year) %>%
							mutate(Yj = rnorm(1,0,sd.year)) %>%
							group_by(BA, Year, Quadrat) %>%
							mutate(Qr = rnorm(1, 0, sd.quad)) %>%
							ungroup() %>%
							mutate(Yijk = b0 + BAi + Yj + Qr) %>%
							rename(!!sym(resp) := Yijk)
					
					year <- as.random(sim.dat$Year)
					ba <- as.fixed(sim.dat$BA)
					lmf <- do.call("lm", list(fm1, data=sim.dat))
					# fit model
					gadres <- gad(lmf)
					
					pval <- gadres[1,5]
					
					
				} else if(block.nested) {
					
					# Block nested within Year
					sim.dat <- model.des %>%
							mutate(BAi = case_when(
											
											BA=="Before" ~ b0*0.5*EffSize,
											BA=="After" ~ b0*-0.5*EffSize,
											TRUE ~ as.numeric(EffSize)									
									)
							
							) %>%
							group_by(Year) %>%
							mutate(Yj = rnorm(1,0,sd.year)) %>%
							group_by(BA, Year, Block) %>%
							mutate(Bk = rnorm(1, 0, sd.block)) %>%
							group_by(BA, Year, Block, Quadrat) %>%
							mutate(Qr = rnorm(1, 0, sd.quad)) %>%
							ungroup() %>%
							mutate(Yijk = b0 + BAi + Yj + Bk + Qr) %>%
							rename(!!sym(resp) := Yijk)
					
					# analysis
					# for lmer instead of GAD
					#m.sim <- do.call("lmer", list(fm1, REML=T, data=sim.dat))
					#coef.msim <- (summary(m.sim))$coefficients
					
					# use GAD
					block <- as.random(sim.dat$Block)
					year <- as.random(sim.dat$Year)
					ba <- as.fixed(sim.dat$BA)
					lmf <- do.call("lm", list(fm1, data=sim.dat))
					# fit model
					gadres <- gad(lmf)
					
					pval <- gadres[1,5]
					
				} else {
					
					# Block crossed with Year. Include the Year * Block
					# interaction component
					sim.dat <- model.des %>%
							mutate(BAi = case_when(
											
											BA=="Before" ~ b0*0.5*EffSize,
											BA=="After" ~ b0*-0.5*EffSize,
											TRUE ~ as.numeric(EffSize)									
									)
							
							) %>%
							group_by(BA, Year) %>%
							mutate(Yj = rnorm(1,0,sd.year)) %>%
							group_by(BA, Year, Block) %>%
							mutate(
									Bk = rnorm(1, 0, sd.block),
									YBjk = rnorm(1, 0, sd.yrblock),
							) %>%
							group_by(BA, Year, Block, Quadrat) %>%
							mutate(Qr = rnorm(1, 0, sd.quad)) %>%
							ungroup() %>%
							mutate(Yijk = b0 + BAi + Yj + Bk + YBjk + Qr) %>%
							rename(!!sym(resp) := Yijk)		
					
					# analysis
					# for lmer instead of GAD
					#m.sim <- do.call("lmer", list(fm1, REML=T, data=sim.dat))
					#coef.msim <- (summary(m.sim))$coefficients
					
					# use GAD
					block <- as.random(sim.dat$Block)
					year <- as.random(sim.dat$Year)
					ba <- as.fixed(sim.dat$BA)
					lmf <- do.call("lm", list(fm1, data=sim.dat))
					
					# check Mean Square estimates to construct appropriate F ratios
					# estimates(lmf)
					
					# There is no direct test for the main effect of Before vs. After
					# for the model: (ba/year)*block. A test for the main effect of BA
					# can be constructed if other terms can be eliminated from the model
					# (p>0.25 according to Winer 1971) as follows:
					# (i) ba:year, ba:block, ba:year_block can all be eliminated -> use the
					# residual MS as the denominator for an F test of BA
					# (ii) ba:year and ba:block can both be elimiated, but not ba:year:block ->
					# use the ba:year:block term as the denominator for F
					# (iii) only ba:block can be eliminated -> use ba:year as the denominator for F
					# (iv) only ba:year can be eliminated -> use ba:block as the denominator for F
					
					# fit model
					gadres <- gad(lmf)
					
					# check if some terms can be eliminated
					p.ba.year <- gadres[which(rownames(gadres)%in%"ba:year"), 5]
					p.ba.block <- gadres[which(rownames(gadres)%in%"ba:block"), 5]
					p.ba.year.block <- gadres[which(rownames(gadres)%in%"ba:year:block"), 5]
					
					which(c(p.ba.year,p.ba.block,p.ba.year.block) > 0.25)
					
					# (i)
					if(all(c(p.ba.year,p.ba.block,p.ba.year.block) > 0.25)) {
						
						f.ba <- gadres[which(rownames(gadres)%in%"ba"), 3]/gadres[which(rownames(gadres)%in%"Residual"), 3]
						p.ba <- 1-pf(f.ba,
								df1 = gadres[which(rownames(gadres)%in%"ba"), 1],
								df2 = gadres[which(rownames(gadres)%in%"Residual"), 1]
						)
						
					}
					
					# (ii)
					if(p.ba.year > 0.25&&p.ba.block > 0.25&&p.ba.year.block < 0.25) {
						
						f.ba <- gadres[which(rownames(gadres)%in%"ba"), 3]/gadres[which(rownames(gadres)%in%"ba:year:block"), 3]
						p.ba <- 1-pf(f.ba,
								df1 = gadres[which(rownames(gadres)%in%"ba"), 1],
								df2 = gadres[which(rownames(gadres)%in%"ba:year:block"), 1]
						)
						
					}
					
					# (iii)
					if(p.ba.block > 0.25&&p.ba.year < 0.25) {
						
						f.ba <- gadres[which(rownames(gadres)%in%"ba"), 3]/gadres[which(rownames(gadres)%in%"ba:year"), 3]
						p.ba <- 1-pf(f.ba,
								df1 = gadres[which(rownames(gadres)%in%"ba"), 1],
								df2 = gadres[which(rownames(gadres)%in%"ba:year"), 1]
						)
						
					}
					
					# (iv)
					if(p.ba.year > 0.25&p.ba.block < 0.25) {
						
						f.ba <- gadres[which(rownames(gadres)%in%"ba"), 3]/gadres[which(rownames(gadres)%in%"ba:block"), 3]
						p.ba <- 1-pf(f.ba,
								df1 = gadres[which(rownames(gadres)%in%"ba"), 1],
								df2 = gadres[which(rownames(gadres)%in%"ba:block"), 1]
						)
						
					}
					
					# No test
					if(all(c(p.ba.year,p.ba.block) < 0.25)) {
						
						p.ba <- NA
						
					}
					
					pval <- p.ba
					# for lmer instead of GAD
#				if(coef.msim[which(rownames(coef.msim)=="BAAfter"),"Estimate"]<0) {
#					
#					pval <- c(pval, coef.msim[which(rownames(coef.msim)%in%"BAAfter"),5])
#					
#				} else {
#					
#					pval <- c(pval, 1) # include only significant negative effects
#					
#				}
					
				}
				
			}	
			
			term <- "BA"
			
		}
		
		out <- data.frame(term=term, n.years=sim.k[,"Year"], n.blocks=sim.k[,"Block"], n.quads=sim.k[,"Quadrat"],
				eff.size=sim.k[,"EffSize"], power=length(which(pval<0.05))/length(pval))
		
	}
	
	
	block.labels <- setNames(paste0("N. blocks: ", unique(out$n.blocks)),
			unique(out$n.blocks))
	quad.labels <- setNames(paste0("N. quadrats: ", unique(out$n.quads)),
			unique(out$n.quads))
	
	if(length(unique(out$eff.size))==1&max(out$n.years)>1) {
		
		p <- ggplot(out %>% mutate(
								n.years=as.factor(n.years),
								eff.size=as.factor(eff.size),
								n.blocks=as.factor(n.blocks),
								n.quads=as.factor(n.quads)
						),
						aes(x = n.years, y = power, group=eff.size)) +
				geom_point(aes(color=eff.size, fill=eff.size), shape=15, size=4) +
				geom_line(color="#F8766D", linewidth=1.2) +
				scale_fill_manual(values="#F8766D") +
				scale_color_manual(values="#F8766D") +
				geom_hline(yintercept = 0.8, col="grey60", linetype=2, linewidth=1) +
				ylim(0,1) +
				facet_grid(n.quads ~ n.blocks,
						labeller = labeller(n.quads=quad.labels, n.blocks=block.labels)) +
				theme_bw() +
				labs(fill = "Effect size", color = "Effect size", y = "Power", x = "Years") +
				theme(
						legend.position = "right",
						panel.background = element_blank(),
						axis.text.y = element_text(size = 12, colour = "black"),
						axis.text.x = element_text(size = 12, colour = "black"),
						axis.title.x = element_text(size = 14, colour = "black"),
						axis.title.y = element_text(size = 14, colour = "black")
				)
		
		
	} else {
		
		legend.lab <- ifelse(eff.type=="trend", "Number of years",
				"Years within the\nBefore-After\ncomparison")
				
		p <- ggplot(out %>% mutate(
								n.years=as.factor(n.years),
								eff.size=as.factor(eff.size),
								n.blocks=as.factor(n.blocks),
								n.quads=as.factor(n.quads)
						),
						aes(x = eff.size, y = power, group=n.years)) +
				geom_point(aes(fill = n.years, color=n.years), shape=15, size=3) +
				geom_line(aes(col = n.years)) +
				scale_fill_fish_d(option = "Cirrhilabrus_solorensis", direction = 1) +
				scale_color_fish_d(option = "Cirrhilabrus_solorensis", direction = 1) +
				geom_hline(yintercept = 0.8, col="grey60", linetype=2, linewidth=1) +
				ylim(0,1) +
				facet_grid(n.quads ~ n.blocks,
						labeller = labeller(n.quads=quad.labels, n.blocks=block.labels)) +
				theme_bw() +
				labs(fill = legend.lab, color = legend.lab, y = "Power", x = "Effect size") +
				theme(
						legend.position = "right",
						panel.background = element_blank(),
						axis.text.y = element_text(size = 12, colour = "black"),
						axis.text.x = element_text(size = 12, colour = "black"),
						axis.title.x = element_text(size = 14, colour = "black"),
						axis.title.y = element_text(size = 14, colour = "black")
				)
		
	}
	
	res <- list(power.res=out, power.plot=p)
	return(res)
}

#### ---- EXAMPLES ---- ####
setwd('~/Lavori/GOMON')

df <- read.csv2("sll_intertidal_macroalgal_eov_example.csv", dec=".") %>%
		group_by(Year, Site, Block, Quadrat) %>%
		filter(Year==2021) %>%
		summarise(Measurement=sum(Measurement), .groups="drop")

# Specify simulated parameters - NOTE: always include Block even if not replicated in the design (in which case Block=1)
sim.grid <- expand.grid(Year=2:6, Quadrat=6, Block=1, EffSize=c(0.1, 0.5)) 

# 1) Simple design
# 1a) Analysis with no Block (no random effect); the input data may have one or multiple sampling years;
# include Year in the formula even if the input data has only one sampling Year, since the formula is 
# used to analyze simulated trends over multiple years 
# 1a: fit trend
trend.sim <- gomon_power(fm=formula(Measurement ~ Year),
		df=df, sim.grid=sim.grid, eff.type="trend", n.sim=99)

# 1b: fit event: the input data must include at least two sampling years, but no blocks. Replicated years are
# necessary to estimate variance among years that are then used to generate Before-After timeseries.
event.sim <- gomon_power(fm=formula(Measurement ~ 1 + (1|Year)),
		df=df, sim.grid=sim.grid, eff.type="event", n.sim=99)

# Examples below have been tested with a dataset that included replicated blocks (not yet in the GOMON format)

# Include replicated blocks in sim.grid
sim.grid <- expand.grid(Year=c(2,3), Quadrat=5, Block=c(2,6), EffSize=c(0.1, 0.5)) 

# 2) Multiple blocks nested within Year
# 2a: trend (one or multiple years); make nesting explcit in the formula.
trend.sim <- gomon_power(fm=formula(Measurement ~ Year + (1|Year:Block)),
		df=df, sim.grid=sim.grid, eff.type="trend", n.sim=99)

# 2b: event (multiple years); Year and Block are random factors with Block nested in Year
event.sim <- gomon_power(fm=formula(Measurement ~ 1 + (1|Year/Block)),
		df=df, sim.grid=sim.grid, eff.type="event", n.sim=99)

# 3) Multiple blocks crossed with Year
# 3a: trend (one or multiple years); Block is random and we want to estimate the Block and
# Year*Block variance components
trend.sim <- gomon_power(fm=formula(Measurement ~ Year + (1|Block) + (1|Year:Block)),
		df=df, sim.grid=sim.grid, eff.type="trend", n.sim=99)

# 3b: event (multiple years); Year and Block are crossed random factors and we want to estimate
# the Year, Block and Year*Block variance components.
event.sim <- gomon_power(fm=formula(Measurement ~ 1 + (1|Year) + (1|Block) + (1|Year:Block)),
		df=df, sim.grid=sim.grid, eff.type="event", n.sim=99)

#windows(width=7, heigh=6)
trend.sim[[2]]

#windows(width=7, heigh=6)
event.sim[[2]]
