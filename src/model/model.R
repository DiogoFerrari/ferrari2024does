library(conflicted)
library(furrr)
library(MatchIt)
library(cobalt)
library(sjmisc)
library(sjlabelled)
library(lme4)
library(multcomp)
library(writexl)
library(readxl)
library(dtplyr)
library(labelled)
library(kableExtra)
library(modelsummary)
library(marginaleffects)
library(glue)
library(magrittr)
library(broom)
library(broom.mixed)
library(cowplot)
library(patchwork)
library(jtools)
library(ggh4x)
library(rstatix)
library(scales)
library(tidyverse)
conflicts_prefer(
    dplyr::select,
    readxl::read_excel,
    dplyr::filter,
    dplyr::lag,
    tidyr::nest,
    sjlabelled::label_to_colnames,
    sjlabelled::as_label,
    kableExtra::group_rows
)
##
source("../__paths__.R")
source("../__constants__.R")
## 
PRR = F
SAVE = F
SAVE = T
##
time.start <- Sys.time() # code time profile

## * functions
## ** dot functions

.table <- function(data, v1, v2, groups=NULL,
                   margin='all',
                   stat="both",
                   total=c("row", "col"),
                   use.labels=T,
                   digits=1)
{
    ## handling groups NULL
    groups <- rlang::enquo(groups)
    if (rlang::quo_is_null(groups)) {
        groups <- NULL
    } else if (rlang::quo_is_symbol(groups)) {
        groups <- rlang::get_expr(groups)
    } 

    data = data %>% select(!!rlang::ensym(v1), !!rlang::ensym(v2), !!groups)
    if (use.labels) {
        data = data%>% sjlabelled::as_label(.) 
    }
    res = (
        data  
        %>% nest(-!!groups)
        %>% mutate(
                data = future_map(.x=data,
                                  function(.x)
                                  (
                                      if (stat=='n') {
                                          (.x
                                              %>% tabyl(!!rlang::ensym(v1), !!rlang::ensym(v2))   
                                              %>% adorn_totals(total)
                                          )
                                      }else if (stat=='prop'){
                                          (.x
                                              %>% tabyl(!!rlang::ensym(v1), !!rlang::ensym(v2))   
                                              %>% adorn_totals(total)
                                              %>% adorn_percentages(margin)
                                          )
                                      }else{
                                          (.x
                                              %>% tabyl(!!rlang::ensym(v1), !!rlang::ensym(v2))   
                                              %>% adorn_totals(total)
                                              %>% adorn_percentages(margin) 
                                              %>% adorn_pct_formatting(digits = digits) 
                                              %>% adorn_ns() 
                                          )
                                      }
                                  ))
            )
        %>% unnest(data)
    )    
    ## ----- printing -----
    row.label = var_label(data %>% pull(!!rlang::ensym(v1)))
    row.label = ifelse(is.null(row.label), '--', row.label)
    col.label = var_label(data %>% pull(!!rlang::ensym(v2)))
    col.label = ifelse(is.null(col.label), '--', col.label)
    cat(glue("\n\n",
             "Rows: {row.label} ({rlang::ensym(v1)})",
             "\n",
             "Cols: {col.label} ({rlang::ensym(v2)})",
             "\n\n"))
    res%>% print(n=Inf)
    ## ----- -------- -----
    invisible(res)
}

.freq <- function(data, v, groups=NULL, na.rm=F, print.labels=F)
{
    ## handling groups NULL
    groups <- rlang::enquo(groups)
    if (rlang::quo_is_null(groups)) {
        groups <- NULL
    } else if (rlang::quo_is_symbol(groups)) {
        groups <- rlang::get_expr(groups)
    } 

    res = (
        data 
        %>% nest(-!!groups)
        %>% mutate(
                data = future_map(.x=data,
                                  function(.x)
                                  (
                                      .x
                                      %>% sjmisc::frq(!!rlang::ensym(v), show.na=!na.rm)
                                      %>% data.frame()
                                      %>% as_tibble() 
                                  ))
            )
        %>% unnest(data) 
        %>% select(!!groups, variable, everything())  
        ## %>% arrange(!!rlang::enquo(groups))
        %>% rename(
                n = frq,
                perc = raw.prc,
                perc.cum = cum.prc
            )
        
        )
    ## ---- printing --------
    cat(glue("\n\n\nVariable: \n\n",
                 "{unique(res$variable)}\n\n"))
    if (print.labels) {
        labels = (
            res  
            %>% mutate(label = glue("({val}) {label}")) 
            %>% pull(label)
            %>% paste(., collapse='; ')
        )
        cat(glue("{labels}\n\n"))
    }
    cat('\n')
    res%>% select(-variable)%>%  print(n=Inf)
    ## ---- printing --------
    invisible(res)
}

.descriptive.statistics <- function(data, vars=NULL, groups=NULL, use.labels=T)
{
    ## handling vars NULL
    vars <- rlang::enquo(vars)
    if (rlang::quo_is_null(vars)) {
        vars=names(data)
    } else if (rlang::quo_is_symbol(vars)) {
        vars <- rlang::get_expr(vars)
    } 
    ## ## 
    ## handling groups NULL
    groups <- rlang::enquo(groups)
    if (rlang::quo_is_null(groups)) {
        groups <- NULL
    } else if (rlang::quo_is_symbol(groups)) {
        groups <- rlang::get_expr(groups)
    }
    ## 
    data = data %>% select(!!vars, !!groups)
    res = (
        data
        %>% nest(-!!groups) 
        %>% mutate(
                nobs = future_map_int(.x=data, function(.x) nrow(.x)),
                summary = future_map2(.x=data, .y=nobs, function(.x, .y)
                    .x
                    %>% get_summary_stats(type='common')  
                    %>% select(-ci) 
                    %>% mutate(`Missing (%)`=100*(.y-n)/.y) 
                    %>% select(#!!vars.labels[groups],
                            Variable=variable,
                            N=n,
                            contains("Miss"),
                            Mean=mean,
                            Std.Dev=sd,
                            Std.Err=se,
                            Min=min,
                            Median=median,
                            Max=max,
                            IQR=iqr) 
                    ))  
        %>% select(-data, -nobs)
        %>% unnest(summary)
    )
    if (use.labels) {
        var.names  = data %>% names()
        var.labels = data %>% label_to_colnames() %>% names()
        res = (
            res 
            %>% mutate(Variable = factor(Variable,
                                         var.names,
                                         var.labels))
        )
        ## needed to rename labels of grouping variables, when groups are not NULL
        groups <- rlang::enquo(groups)
        if (!rlang::quo_is_null(groups)){
            res = (
                res 
                %>% set_variable_labels(., !!!setNames(var.names, var.labels), .strict=F) 
                %>% sjlabelled::label_to_colnames()
            )
        }
    }
    res %>% print(n=Inf)
    invisible(res)
}

.get_controls <- function(X)
{
    return(paste(X, collapse=' + '))
}

.signif_transformer <- function(digits = 3)
{
    force(digits)
    function(text, envir) {
        x <- identity_transformer(text, envir)
        if (is.numeric(x)) {
            signif(x, digits = digits)
        } else {
            x
        }
    }
}

.str_replace <- function(s, replace)
{
    res = vapply(s,
                 function(x) ifelse(x %in% names(replace),
                                    sub(x=x, pattern=names(replace[x]), replacement=replace[x]),
                                    x),
                 FUN.VALUE = '')
    return(res)
}

## ** others

ptable  <- function(data, v1, v2,
                    margin='all',
                    total=c("row", "col"),
                    use.labels=T,
                    digits=2)
{
    data = data %>% select(v1=!!v1, v2=!!v2) 
    if (use.labels) {
        data = data %>% sjlabelled::as_label(.) 
    }
    res = (
        data
        %>% tabyl(v1, v2)  
        %>% adorn_totals(total)
        %>% adorn_percentages(margin) 
        %>% adorn_pct_formatting(digits = digits) 
        %>% adorn_ns() 
        ## %>% adorn_title("combined")
        %>% as_tibble()   
        %>% rename(!!v1:=v1)
    )    
    row.names(res) = res %>% pull(v1)
    return(res)
}

descriptive.statistics <- function(data, use.labels=T, groups=NULL)
{
    nobs = nrow(data)
    if (!is.null(groups)) {data = data %>% group_by_at(vars(groups))}
    if (use.labels) {data = data %>% label_to_colnames()}
    print(data)
    res = (
        data
        %>% get_summary_stats(type='common')  
        %>% select(-se, -ci) 
        %>% mutate(`Missing (%)`=100*(nobs-n)/nobs) 
        %>% select(!!groups,
                   Variable=variable,
                   N=n,
                   contains("Miss"),
                   Mean=mean,
                   Std.Dev=sd,
                   Min=min,
                   Median=median,
                   Max=max,
                   IQR=iqr)
    )
    return(res)
}

sample.vs.census <- function(df, var, tables, SAVE, scale.down=T, column.spec=NULL)
{
    census  = read_excel(file.path(PATH_DATA_FINAL,'quotas.xlsx'), glue("{var}-census")) 
    sample  = df %>% distinct(rid, .keep_all=TRUE)  %>% select(var)
    ## table
    ## -----
    tab = (
        sample
        %>% frq(!!var)
        %>% data.frame()
        %>% as_tibble()   
        %>% left_join(., census %>%  
                         mutate(freq=round(100*freq, digits=2))%>% 
                         select(category, code, "Freq (Census)"=freq)
                    , by=c("val"='code')) 
        %>% select(
                Code=val,
                Category='label',
                "N (survey)"=frq,
                "Freq (survey)" = raw.prc,
                contains("Freq")
            )
        %>% arrange(Code)  
        %>% drop_na(Code, Category) 
    ) 
    tab %>% print(., n=Inf) 
    ## latex
    ## -----
    caption = tables %>% filter(var==!!var)  %>% pull(caption)
    tabl = (
        tab
        %>% kable(., "latex", booktabs = T, caption=caption,
                  escape=T, align=c("ll", rep("c", ncol(.)-2)),
                  digits=4, longtable = F, table.envir = "table",
                  linesep = NULL)  
    )
    if (!is.null(column.spec)) {
        tabl = tabl %>% column_spec(., 2, width = column.spec, border_right = F) 
    }
    if (scale.down) {
        tabl = tabl %>% kable_styling(latex_options = c("scale_down", "repeat_header"),
                                    position = "center", font_size=NULL) 
    }
    ## 
    table = tables %>% filter(var==!!var)  %>% pull(table)
    if (PRR) {
        tabl = str_replace(string=tabl,
                           pattern="\\\\caption",
                           replacement="\\\\placeholdertable{120}{-90}\\\n\\\\caption")
    }
    if(SAVE){save.table(tab, tabl, PATH_MAN_TABLES, table)}
}

save.figure <- function(g, tab, path, fn, width=NA, height=NA)
{
    figure = fn
    ## table
    cat(glue("\n\nSaving table of figure {figure}...") )
    fn = file.path(path, glue("{figure}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    cat("done!\n")
    ## 
    cat(glue("Saving figure {figure}...") )
    fn  = c(glue("{figure}.pdf"),
            glue("{figure}.png"),
            glue("{figure}.jpg"))
    fns = file.path(path, fn)
    for (fn in fns){ggsave(g, filename=fn, width=width, height=height)}
    cat("done!\n")
}

save.table <- function(tab, tabl, path, fn)
{
    table=fn
    cat(glue("Saving {table}...") )
    fn = file.path(path, glue("{table}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    writeLines(text=tabl, glue("{fn}.tex"))
    cat("done!\n")
}

get_controls <- function(X)
{
    return(paste(X, collapse=' + '))
}

estimate <- function(formula, data, pid, ref.group='G0')
{
    data = (
        data 
        %>% remove_val_labels()  
        %>% mutate(treat.group = factor(as.character(treat.group)),
                   treat.group = relevel(treat.group, ref=ref.group)) 
    )
    fit    = glm(formula, data=data, family='binomial')
    summ   = broom::tidy(fit, conf.int=TRUE)
    glance = glance(fit)
    pred   = make_predictions(fit,
                              pred=pid,
                              at=list(treat.group=c('G0', 'G0 (aware)',
                                                    'G1', 'G2',
                                                    'G3', 'G4')), interval=T)
    res = tibble(fit   = list(fit),
                 summ  = list(summ),
                 glance= list(glance),
                 pred  = list(pred)
                 )
    return(res)
}

estimate.did <- function(fit, pid, digits=2)
{
    ## marginal effects (dg(mu)/dP) labels
    G0.aware = glue("{pid}:treat.groupG0 (aware)") 
    G1       = glue("{pid}:treat.groupG1") 
    G2       = glue("{pid}:treat.groupG2") 
    G3       = glue("{pid}:treat.groupG3") 
    G4       = glue("{pid}:treat.groupG4") 
    
    ## marginal effects (dg(mu)/dP) labels
    b1       = coef(fit)[G1]
    b2       = coef(fit)[G2]
    b3       = coef(fit)[G3]
    b4       = coef(fit)[G4]
    b0.aware = coef(fit)[G0.aware]
    b2.aware = b2 - b0.aware 
    b4.aware = b4 - b0.aware 

    ## differences
    test.ds = glue("({G1}) - ({G2} - `{G0.aware}`) == 0")
    test.rs = glue("({G3}) - ({G4} - `{G0.aware}`) == 0")
    did.ds = tidy(summary(glht(fit, linfct = test.ds)), conf.int=T)
    did.rs = tidy(summary(glht(fit, linfct = test.rs)), conf.int=T)
    did = bind_rows(did.ds %>% mutate(cue='ds'),
                    did.rs %>% mutate(cue='rs')) 

    did = (
        did
        %>% rename(test=lhs)  
        %>% select(-rhs) 
        %>% select(cue, all_of(names(.))) 
        %>% mutate(label=str_replace_all(string=test, pattern="treat.group", replacement="") %>%
                       str_replace_all(string=., pattern="(pid|psid|):|`", replacement=""),
                   PxG1                    = b1,
                   PxG2                    = b2,
                   PxG3                    = b3,
                   PxG4                    = b4,
                   `PxG2 (aware)`          = b2.aware,
                   `PxG4 (aware)`          = b4.aware,
                   `PxG0 (aware)`          = b0.aware,
                   ##
                   baseline.group = case_when(cue == 'ds' ~ 'G1',
                                              cue == 'rs' ~ 'G3'),
                   aware.group = case_when(cue == 'ds' ~ 'G2',
                                           cue == 'rs' ~ 'G4'),
                   PxCue = case_when(cue == 'ds' ~ b1,
                                     cue == 'rs' ~ b3),
                   PxCue.aware = case_when(cue == 'ds' ~ b2.aware,
                                           cue == 'rs' ~ b4.aware),
                   ## DiD
                   did = PxCue - PxCue.aware,
                   change = 100*abs(PxCue - PxCue.aware)/abs(PxCue),
                   )   
        ## labels
        %>% mutate(
                stars = case_when(p.value<0.001 ~ '***',
                                  p.value<0.01  ~ '**',
                                  p.value<0.05  ~ '*',
                                  p.value<0.1   ~ '+',
                                  T ~ '',
                                  ),
                did.label = glue("{round(did, digits)}{stars} ",
                                 "({round(conf.low, digits)}, {round(conf.high, digits)})"),
                change.arrow = case_when(p.value>=0.05 ~ "0",
                                         PxCue  >=PxCue.aware & cue=='rs'~"\\downarrow",
                                         PxCue  < PxCue.aware & cue=='rs'~"\\uparrow",
                                         ## 
                                         ## For ds cue, less cue effect under awareness
                                         ## means that cue effect under awareness is larger
                                         ## because cue effect should be negative
                                         ## PiD scale, goes from
                                         ## Democratic voter (lower value) to
                                         ## Republican voters (higher value)
                                         ## So, ds cue should reduce support (less negative)
                                         ## as pid increases
                                         PxCue  <=PxCue.aware & cue=='ds'~"\\downarrow",
                                         PxCue  > PxCue.aware & cue=='ds'~"\\uparrow",
                                         ),
                change.label = paste0(round(change, 1), "\\% (", change.arrow, ")")
            )
    ) 
    return(did)
}

check_did <- function(res.did, cue, row=1)
{
    cat("\n ------------ Checking (manual check Did) ------------\n")
    i = row
    if (cue=='ds') {
        G.ref = 'G1'
        G.treat = 'G2'
    }else{
        G.ref = 'G3'
        G.treat = 'G4'
    }
    tmp     = res.did %>% filter(cue==!!cue)  %>%  remove_val_labels()
    row.names(tmp) = 1:nrow(tmp)
    tmp     = tmp[i,]
    policy  = tmp$policy
    formula = tmp$formula
    pid     = tmp$pid
    ncovars = tmp$ncovars
    tmp.data= tmp$data[[1]] 
    cat("\n--------------- Computed: ---------------\n")
    print(tmp  %>% t())
    cat("\n--------------- Replication: ---------------\n")
    test = tmp$test
    mod1 = tmp$fit[[1]]
    print(tidy(summary(glht(mod1, linfct = glue("{test} == 0") )), conf.int=T))
    cat("\n---------------Manual check:---------------\n")
    mod1a = glm(formula, data=tmp.data, family="binomial")
    mod2a = glm(formula, data=tmp.data %>%
                             mutate(treat.group=relevel(as.factor(treat.group), 'G0 (aware)')),
                family="binomial")
    ## mod1; mod2
    cat("\nModel with G0 as reference:\n")
    print(tibble( 
        `G0 (aware)`=coef(mod1a)[glue('{pid}:treat.groupG0 (aware)')],
        !!glue("P x {G.ref}")   :=coef(mod1a)[glue('{pid}:treat.group{G.ref}')],
        !!glue("P x {G.treat}") :=coef(mod1a)[glue('{pid}:treat.group{G.treat}')],
        ) %>% t())
    cat("\nModel with G0 (aware) as reference:\n")
    print(tibble( 
        !!glue("P x {G.ref}")   :=coef(mod2a)[glue('{pid}:treat.group{G.ref}')],
        !!glue("P x {G.treat}") :=coef(mod2a)[glue('{pid}:treat.group{G.treat}')],
        `G0`        =coef(mod2a)[glue('{pid}:treat.groupG0')],
        ) %>% t())
    cat("\nSame reg, subtracting the Aware effect:\n")
    (
        coef(mod1a)[glue('{pid}:treat.group{G.ref}')] -
        (coef(mod1a)[glue('{pid}:treat.group{G.treat}')]-
         coef(mod1a)[glue('{pid}:treat.groupG0 (aware)')])
    ) %>% print()
    cat("\nUsing 2 reg with different reference groups:\n")
    (
        coef(mod1a)[glue('{pid}:treat.group{G.ref}')] - coef(mod2a)[glue('{pid}:treat.group{G.treat}')] 
    ) %>% print()

}

create_fig1 <- function(tab, leg.ncol)
{
    x = 'cue'
    y = "avg"
    fill = 'pid.cat'
    linetype = "policy"
    facet1 = 'pid.cat'
    facet2 = 'aware'
    colors = c('Democratic voter' = "blue",
               'Republican voter' = 'red')
    leg = tab %>% pull(policy.group) %>% extract2(1)
    g <- (
        ggplot(tab) 
        + geom_line(aes_string(x=x, y=y, color=fill, 
                               fill=fill, group=linetype, linetype=linetype)) 
        + geom_point(aes_string(x=x, y=y, fill=fill, shape=linetype), size=3.5) 
        + scale_shape_manual(values = 21:24) 
        + scale_color_manual(values = colors) 
        + scale_fill_manual(values = colors) 
        + facet_nested(formula(glue("grid_title2+{facet2} ~ {facet1}") ) )
        + ylim(0,1)
        + labs(
              x        = 'Party Cue Treatment',
              y        = 'Proportion Supporting the Policy',
              color    = leg, 
              fill     = leg,
              linetype = leg,
              shape    = leg
          )
        + ggguides(ncol = leg.ncol) 
        + ggtheme2()
        + guides(fill = 'none', color='none')
    )
    return(g)
}

create_fig2 <- function(tab, policy.group)
{
    x = "term"
    y = "estimate"
    color    = NULL
    fill     = 'policy'
    facet1   = 'aware'
    facet2   = NULL
    title    = NULL
    subtitle = NULL
    caption  = NULL
    dodge    = .4
    ylab     = 'Avearge Treatment Effect on Policy Support'
    xlab     = 'Treatment Group (Party Cue)'
    leg      = policy.group
    g = (
        tab  
        %>% filter(policy.group==!!policy.group) 
        %>% ggplot(.)
        + geom_hline(aes(yintercept=0) ,linetype="dashed", col="red")
        + geom_errorbar(aes_string(x=x, ymin="conf.low", ymax="conf.high", color=fill),
                        width=.2,
                        position = position_dodge(dodge)) 
        + geom_point(aes_string(x=x, y=y, fill=fill, shape=fill), position = position_dodge(dodge))
        ## 
        + scale_shape_manual(values=c(21,22,23, 24))
        + scale_fill_grey(start = 0, end = .7, na.value="red") 
        + scale_color_grey(start = 0, end = .7, na.value="red") 
        ## 
        + facet_wrap(glue("~ {facet1}"), ncol = 1, scales='free_x')
        + theme_bw()
        + ggtheme2()
        + ggguides(ncol=1)
        ##
        + labs(
              x        = xlab,
              y        = ylab,
              color    = leg, 
              fill     = leg,
              linetype = leg,
              shape    = leg,
              title    = title,
              subtitle = subtitle,
              caption  = caption
          )
    )
    return(g)
}

create_fig3 <- function(tab, aware)
{
    x = 'pid'
    y = "att"
    fill = NA
    lt = 3
    facet1 = 'group'
    if (aware=='Not aware')
    {
        xlab = NULL
        tabt = tab %>% filter(aware=='Not aware') 
    }else{
        xlab = "Voters' Party Identification"
        tabt = tab %>%
            filter(aware=='Aware')  %>%
            filter(group!='Control') 
    }
    xbreaks = c(-1, 1)
    xlabels = c('Strong\nDem.', 'Strong\nRep.')
    title = aware
    g = (
        tabt 
        %>% ggplot()
        + geom_line(aes_string(x=x, y=y))
        + geom_line(aes_string(x=x, y='ymin'), linetype=lt)
        + geom_line(aes_string(x=x, y='ymax'), linetype=lt)
        + facet_wrap(paste("~", facet1), nrow=1, scales='free')
        + scale_x_continuous(breaks=xbreaks, limits=c(-1.1, 1.1), labels=xlabels)
        + ylim(0,1)
        + labs(
              x=xlab,
              y='Predicted Probability',
              color=NULL, 
              fill=NULL,
              linetype=NULL,
              shape=NULL,
              title=title,
              caption=NULL
          )
        + ggtheme2()
        + theme(
              axis.text.x=element_text(colour="gray20", size=7),
              plot.title=element_text(hjust=0, size=9, colour='grey40', face='bold'),
              strip.text.x=element_text(size=8, face='bold', hjust=0)
          )  
    )
    return(g)
}

.signif_transformer <- function(digits = 3)
{
    force(digits)
    function(text, envir) {
        x <- identity_transformer(text, envir)
        if (is.numeric(x)) {
            signif(x, digits = digits)
        } else {
            x
        }
    }
}


## * loading

fn = file.path(PATH_DATA_FINAL, "survey.Rdata")
load(file=fn)
df %>% glimpse()

## * overview

cat("\n ------------ Checking (unique treat group) ------------\n")
df %>% group_by(policy, treat.group) %>% summarise(n=n())  %>% arrange(policy, treat.group)  %>%  print(n=100)

cat("\n ------------ Checking (obs in long format by policy) ------------\n")
df %>% group_by(treat.group) %>% summarise(n=n())  %>% print(n=100)

cat("\n ------------ Checking (obs in long format by policy) ------------\n")
df %>% group_by(policy) %>% summarise(n=n())  %>% print(n=100)

cat("\n ------------ Checking (unique treat group) ------------\n")
df %>% group_by(treat.group) %>% summarise(n=n())  %>% arrange(treat.group)  %>%  print(n=100)

cat("\n ------------ Checking (unique obs) ------------\n")
glue("Unique respondents: {df %>% distinct(rid, .keep_all=TRUE)  %>% nrow}") 
glue("Long format (x8 policies): {df %>% nrow}") 
glue("Long format divide by 8 (policies): {df %>% nrow/8}") 


## * --------     Main paper    ----------
## * DONE Estimation
## ** DONE separately by policy

## Note: The estimation is repeated using different reference groups,
##   G0 and G0 (aware)

plan(multisession, workers = parallel::detectCores()-2)
res.pp = (
    df    
    %>% nest(-policy)  
    %>% crossing(pid = c('pid', 'psid')) 
    %>% crossing(covars = list(1, CONTROLS))   
    %>% crossing(ref=c('G0', 'G0 (aware)'))
    %>% rowwise(.)
    %>% mutate(formula = glue("att ~ {pid}*treat.group + {get_controls(covars)}"))  
    %>% ungroup(.) 
    %>% mutate(
            ncovars = future_map_dbl(.x=covars, function(.x) length(.x)-1),
            fit     = future_pmap(list(formula, data, pid, ref), function(formula, data, pid, ref)
                estimate(formula, data, pid, ref)
                ),
            )  
    %>% unnest(fit)
)
plan(sequential)
res.pp


## ** DONE difference-in-differences

res.did = (
    res.pp  
    %>% filter(ref=='G0') 
    %>% mutate(did = future_map2(.x=fit, .y=pid, function(fit=.x, pid=.y) estimate.did(fit, pid))) 
    %>% unnest(did)
)
## res.did %>% glimpse()

## checking 
## --------
cat("\n ------------ Checking (DiD computation) ------------\n")
check_did(res.did, cue='ds', row=3)
check_did(res.did, cue='rs', row=6)


## ** DONE pooled across policies and cue type (outcome: policy support)

cat("\n\nEstimating hierarchical model. This may take a while...")
f = glue("att ~ pid.cat*cue*aware",
         " + (1 + cue*aware | policy)",
         " + (1 | rid)")
res.agg = (
    tibble(covars = list(1, CONTROLS))   
    %>% rowwise(.)
    %>% mutate(formula = glue("{f} + {get_controls(covars)}"))   
    %>% crossing(model=c('lpm', 'logistic')) 
    %>% ungroup(.) 
    %>% mutate(
            ncovars = future_map_dbl(.x=covars, function(.x) length(.x)-1),
            fit = future_map2(.x=formula, .y=model, function(.x, .y)
                if (.y=='lpm')
                {
                    lmer(.x,
                         data=df %>% as_label(aware, pid.cat))
                }else {
                    glmer(.x,
                          data=df %>% as_label(aware, pid.cat),
                          family='binomial')
                }),
            summ = future_map(.x=fit, function(.x) tidy(.x, conf.int=TRUE)),
            glance = future_map(.x=fit, function(.x) glance(.x)),
            pred = future_pmap(list(fit), function(fit)
                predictions(fit, by=c('aware', 'pid.cat', 'cue')
                            )
                )
        )
)
## res.agg 
cat("done!\n\n")

## * DONE Interim analysis (stopping rule check)

ref = 'G0'
tab= (
    res.pp
    %>% filter(ref==!!ref)  
    %>% filter(pid=="pid") 
    %>% select(policy, summ) 
    %>% unnest(summ) 
    %>% filter(str_detect(term, pattern=":"))   
    %>% mutate('Significant at alpha=0.05' = ifelse(p.value<0.05, "Yes", 'No') )
)
tab %>% sjmisc::frq("Significant at alpha=0.05")

## * DONE Tables
## ** Table 3

table='tab-3'
## 
## table
## -----
terms <- c(
    "(Intercept)"                               = "Dem. voter (baseline)",
    "awareAware"				= "Dem. voter x Aware",
    "cueds"					= "Dem. voter x Dem. support cue",
    "cueds:awareAware"				= "Dem. voter x Dem. support cue x Aware",
    "cuers"					= "Dem. voter x Rep. support cue",
    "cuers:awareAware"				= "Dem. voter x Rep. support cue x Aware",
    "pid.catRepublican voter"			= "Rep. voter",
    "pid.catRepublican voter:awareAware"	= "Rep. voter x Aware",
    "pid.catRepublican voter:cueds"		= "Rep. voter x Dem. support cue",
    "pid.catRepublican voter:cueds:awareAware"	= "Rep. voter x Dem. support cue x Aware",
    "pid.catRepublican voter:cuers"		= "Rep. voter x Rep. support cue",
    "pid.catRepublican voter:cuers:awareAware"	= "Rep. voter x Rep. support cue x Aware"
)
res.agg = res.agg %>% mutate(ncovars.label = case_when(ncovars==0~"Unadjusted", T~'Adjusted\\tnote{a}')) 
mods = res.agg %>% filter(model=='lpm') %>% pull(fit)
names(mods) = res.agg %>% filter(model=='lpm') %>% pull(ncovars.label)
tab = (
    modelsummary(mods,
                   estimate  = "{estimate}{stars} ({conf.low}, {conf.high})",
                   statistic=NULL,
                   stars=TRUE,
                   vcov = 'classical', 
                   ## 
                   coef_omit = "SD|Cor",
                   gof_omit = 'Errors|F|RMSE|AIC|BIC', 
                   coef_map=terms,
                   output='data.frame',
                   )%>% 
    select(-part, -statistic)%>% 
    mutate(term = ifelse(lag(term)==term & !is.na(lag(term)), "", term),
           term = str_replace_all(string=term, pattern=':', replacement=' x '))
)
tab
## 

## latex
## -----
caption = glue('\\label{{{table}}}Adjusted and unadjusted linear probability model',
               ' estimates of the effect',
               " of the treatment conditions on partisans' policy support.",
               " Random effects used at policy and respondent levels.",
               ' The 95\\% confidence intervals of the estimates are shown in parenthesis.',
               " The reference groups are Democratic voters under no party ",
               " cue and no awareness exposure."
               )
pvalues = glue("\\\\footnotesize  + p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001\\\\\\\\")
controls = glue_collapse(unlist(LABELS.VARS[CONTROLS]), ", ")
controls = glue("Adjustment variables: {controls}")
tabl = (
    tab
    %>% rename(' '=term) 
    %>% kable(., "latex", booktabs = T, caption=caption,
              escape=F,
              align=c("l", rep("r", ncol(.)-1)),
              digits=4, longtable = F, table.envir = "table",
              linesep = NULL) 
    %>% kable_styling(latex_options = c("scale_down", "repeat_header"),
                      position = "center", font_size=NULL)  
    %>% footnote(
            general	      = pvalues,
            general_title     = "", ## title of the general legend
            threeparttable    = T,
            escape            = F,
            alphabet	      = c(a = controls),
            footnote_as_chunk = F,  # F means fotenotes are spread in multiple lines
            title_format      = c("italic")
        ) 
)
tabl

##
if (SAVE) {
    cat(glue("Saving {table}...") )
    fn = file.path(PATH_MAN_TABLES, glue("{table}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    writeLines(text=tabl, glue("{fn}.tex"))
    cat("done!\n")
}


## for reporting in the paper
## --------------------------
ncovars = 0
mods = res.agg %>% filter(model=='lpm') %>% filter(ncovars==!!ncovars) %>% pull(fit)
tab.report = (
    modelsummary(mods,
                 ## estimate = glue("{{estimate}} ([{{conf.low}}, {{conf.high}}];",
                 ##                 " p-value={{p.value}}) {{stars}}"),
                 estimate = glue("{{estimate}} ; {{conf.low}} ; {{conf.high}};",
                                 " {{p.value}} ;  {{stars}}"),
                 statistic=NULL, stars=TRUE, 
                 vcov = 'classical', coef_omit = "SD|Cor",
                 coef_map=terms, gof_omit = '.', output='data.frame')
)
tab.report 

(
    tab.report %>% 
    select(-part, -statistic)%>% 
    mutate(term = ifelse(lag(term)==term & !is.na(lag(term)), "", term),
           term = str_replace_all(string=term, pattern=':', replacement=' x '))
    %>% separate(., col="(1)",
                 into=c("estimate", "conf.low", 'conf.high', 'p.value', 'stars'), sep=";")  
    %>% mutate(estimate= 100*as.numeric(estimate),
               conf.low = 100*as.numeric(conf.low),
               conf.high= 100*as.numeric(conf.high),
               p.value.eq = case_when(str_detect(p.value, pattern="<")~"$<$", T~"="),
               p.value = str_trim(p.value) %>%
                   str_replace_all(string=., pattern="<", replacement="") ,
               stars = str_trim(stars),
               ##
               estimate = glue("{estimate} ([{conf.low}, {conf.high}];",
                                 " p-value {p.value.eq} {p.value})")
               ) 
    %>% tibble()  
    %>% select(term, estimate)
)%>% print(., n=Inf, width=Inf) 

## * DONE Figures
## ** DONE Figure 1

figure = 'fig-1'
## 
tab = (
    df  
    %>% group_by(policy, pid.cat, treat.group) 
    %>% summarise(n=n(),
                  p=mean(att, na.rm=T)) 
    %>% mutate(
            aware = case_when(str_detect(treat.group, pattern="aware")~1, T~0),
            x = treat.group %>% factor(.,
                                       c('G1',
                                         'G2',
                                         'G0',
                                         'G0 (aware)',
                                         'G3',
                                         'G4'),
                                       c('Dem. Support',
                                         'Dem. Support',
                                         'Control',
                                         'Control',
                                         'Rep. Support',
                                         'Rep. Support')
                                       ), 
            treat.group = treat.group %>% sjlabelled::as_label(.),
            pid.cat = case_when(
                str_detect(treat.group, pattern="aware") ~ glue("{pid.cat} (aware)"),
                !str_detect(treat.group, pattern="aware") ~ glue("{pid.cat} (not aware)"),
                ),
            pid.cat = factor(pid.cat, levels=c("Democratic voter (not aware)",
                                               "Democratic voter (aware)",
                                               "Republican voter (not aware)",
                                               "Republican voter (aware)"
                                      ))
        )
    %>% left_join(., df %>%
                     select(policy, policy.group) %>%
                     distinct(., .keep_all=TRUE) , by=c('policy')) 
    %>% mutate(policy= factor(policy,
                              LABELS.VALUES[['policy.short']],
                              names(LABELS.VALUES[['policy.short']])))
)
tab
## 
x = "x"
y = "p"
color    = NULL
fill     = 'pid.cat'
facet1   = 'policy'
facet2   = 'policy.group'
leg      = NULL
title    = NULL
subtitle = NULL
caption  = NULL
dodge    = .1
ylab     = "Percentage in favor of the policy"
xlab     = "Party cue"
dodge=c(1, 2, .05, .3)
dodge=.1
shape=fill
colors.pid = c(
    "Democratic voter (not aware)" = 'blue',
    "Democratic voter (aware)"     = 'lightblue',
    "Republican voter (not aware)" = 'red',
    "Republican voter (aware)"     = 'pink'
)
g = (
    tab 
    %>% ggplot(.) 
    + geom_line(aes_string(x=x, y=y, group=fill,
                           linetype=fill,
                           color=fill),
                position = position_dodge(dodge),
                size=.6
                )
    + geom_point(aes_string(x=x, y=y, fill=fill, shape=fill),
                 position = position_dodge(dodge),
                 size=3,
                 )
    + scale_x_discrete(labels = scales::wrap_format(10))
    + scale_y_continuous(expand = expansion(mult = c(0, 0)), limits=c(0, 1.2),
                         labels = percent_format(scale=100)) 
    + scale_shape_manual(values=c(21, 21, 22, 22))
    ## + scale_fill_manual(values = colors.pid ) 
    ## + scale_color_manual(values = colors.pid) 
    + scale_linetype_manual(values=c(1,2,1,2))
    + scale_fill_grey(start = 0, end = .7, na.value="red") 
    + scale_colour_grey(start = 0, end = .7, na.value="red") 
    + facet_wrap(glue("~ {facet1}"), ncol = 4)
    + labs(
          x        = xlab,
          y        = ylab,
          color    = leg, 
          fill     = leg,
          linetype = leg,
          shape    = leg,
          title    = title,
          subtitle = subtitle,
          caption  = caption
      )
    + ggguides(ncol=2)
    + ggtheme2()
    + theme(strip.text.x = element_text(size=9, face='bold.italic', hjust=0),
            strip.text.y = element_text(size=9, face="bold", vjust=0)) 
)
g

if (SAVE){save.figure(g, tab, PATH_MAN_FIGURES, figure, 9, 5)}


## * -------- Online supplement ----------
## * Estimation
## ** DONE Manipulation checks
## *** DONE Manipulation checks

y=c('mc1', 'mc2')
df.unique = (
    df 
    %>% distinct(rid, .keep_all=TRUE)  
    %>% select(aware, mc.prior, mc1, mc2, CONTROLS)
)
res.mc = (
    df.unique 
    %>% nest(-mc.prior)  
    %>% crossing(y=y) 
    %>% bind_rows( 
            tibble(mc.prior='either',
                   data=list(df.unique))  
            %>% crossing(y=y) 
        )
    %>% crossing(covars  = list(1, CONTROLS))  
    %>% rowwise(.)
    %>% mutate(f = case_when(mc.prior!='either' ~ glue("{y}~aware+ {get_controls(covars)}"),
                             mc.prior=='either' ~ glue("{y}~aware+ {get_controls(covars)} + ",
                                                       "(1 | mc.prior)")) ,
               ncovars = length(covars)) 
    %>% ungroup(.) 
    %>% mutate(
            adj = ifelse(ncovars>1, "Yes", 'No'),
            fit = future_pmap(list(f=f, mc.prior=mc.prior, data=data), function(f, mc.prior, data)
                ifelse(mc.prior=='either', list(lmer(f, data=data)), list(lm(f, data=data)))[[1]]),
            n = future_map_int(.x=fit, function(.x) nrow(model.frame(.x))),
            summ = future_map(.x=fit, function(.x) tidy(.x, conf.int = T))
        )
) %>% print(., n=Inf) 

## *** DONE Manipulation checks (MC stacked)

tab = (
    df 
    %>% distinct(rid, .keep_all=TRUE) 
    %>% select(rid, aware, mc.prior, mc1, mc2, CONTROLS, pid.cat)
    %>% filter(mc.prior!='none') 
    %>% pivot_longer(c('mc1', 'mc2'), names_to='mc', values_to="mc.value") 
    %>% mutate(mc = relevel(factor(mc), ref='mc2'))
)
tab
## 
mod1 = lmer(glue("mc.value ~ aware  + (1| mc)"), data=tab)
mod2 = lmer(glue("mc.value ~ aware + {get_controls(CONTROLS)} + (1| mc)"), data=tab)
mod3 = lm(glue("mc.value ~ aware*mc"), data=tab)
mod4 = lm(glue("mc.value ~ aware*mc+ {get_controls(CONTROLS)}"), data=tab)
mod1.pid = lmer(glue("mc.value~aware*pid.cat  + (1| mc)"), data=tab)
mod2.pid = lmer(glue("mc.value~aware*pid.cat + {get_controls(CONTROLS)} + (1| mc)"), data=tab)
mod3.pid = lm(glue("mc.value ~ aware*mc*pid.cat"), data=tab)
mod4.pid = lm(glue("mc.value ~ aware*mc*pid.cat + {get_controls(CONTROLS)}"), data=tab)

res.mc.stack = list(mod1, mod1.pid,
                    mod2, mod2.pid,
                    mod3, mod3.pid, 
                    mod4, mod4.pid)

## ** DONE Reestimation (subsample)
## *** DONE separately by policy
## Note: The estimation is repeated using different reference groups,
##   G0 and G0 (aware)


plan(multisession, workers = parallel::detectCores()-2)
res.pp.no.mc = (
    df
    %>% filter(mc.prior=='none') 
    %>% nest(-policy)  
    %>% crossing(pid = c('pid', 'psid')) 
    %>% crossing(covars = list(1, CONTROLS))   
    %>% crossing(ref=c('G0', 'G0 (aware)'))
    %>% rowwise(.)
    %>% mutate(formula = glue("att ~ {pid}*treat.group + {get_controls(covars)}"))  
    %>% ungroup(.) 
    %>% mutate(
            ncovars = future_map_dbl(.x=covars, function(.x) length(.x)-1),
            fit     = future_pmap(list(formula, data, pid, ref), function(formula, data, pid, ref)
                estimate(formula, data, pid, ref)
                ),
            )  
    %>% unnest(fit)
)
plan(sequential)
res.pp.no.mc

## *** DONE pooled across policies (outcome: opinion congruence)
## **** note

## -----------------------------------------------------------------------------
## We need to create two separate data sets for each cue conditions:
##    Dem. suppport (ds) vs control (co)
##    Rep. suppport (rs) vs control (co)
##    It is needed because we have to code partisans consistently in the
##      control (no cue) and treatment (cue) groups. For both groups
##      1/0 means one supports/opposes the policy *in the expected theoretical direction*
##    This means we need to code the control group differently depending
##      on the treatment value
##    Ex:
##    - If Treatment cue is "Dem. support/Rep. oppose," (ds) Then,
##      the outcome for *all* Democratic voters (control or treated) should be
##       1 if the Democratic voter *supports* the policy
##       1 if the Republican voter *opposes* the policy
##       0 otherwise
##    - If Treatment cue is "Rep. support/Dem. oppose:" (rs)
##       1 if the Democratic voter *opposes* the policy
##       1 if the Republican voter *supports* the policy
##       0 otherwise
## 
##    Therefore, value 1 indicates that policy support is in the theoretically
##    expected direction given the treatment value received by the
##    treatment group
## 
##    We need to recode the control group accordingly to make sure
##     the value 1 means the same thing in the control and treatment groups
##    We can also recode the cue into a binary indicator because
##     each dataset will have one treatment condition only
## -----------------------------------------------------------------------------



## **** estimation

## Preparing the data
## ------------------
vars = c("rid", "pid.cat", "cue", "aware", "policy", "att", CONTROLS)
df.ds = (
    df 
    %>% select(vars)
    %>% filter(cue %in% c('co', 'ds'))    
    %>% mutate(
            `Awareness effect`=aware,
            `Party cue effect` = ifelse(cue=='co', 0, 1),
            att.congruent = case_when(
                pid.cat=='Democratic voter' & att == 1 ~ 1,
                pid.cat=='Republican voter' & att == 0 ~ 1,
                ## 
                pid.cat=='Democratic voter' & att == 0 ~ 0,
                pid.cat=='Republican voter' & att == 1 ~ 0,
                ),
            )  
    %>% drop_na(att.congruent)   
)
df.rs = (
    df 
    ## %>% mutate(att=tidyr::replace_na(att, 0))
    %>% select(vars)
    %>% filter(cue %in% c('co', 'rs'))  
    %>% mutate(
            `Awareness effect`=aware,
            `Party cue effect` = ifelse(cue=='co', 0, 1),
            att.congruent = case_when(
                pid.cat=='Republican voter' & att == 1 ~ 1,
                pid.cat=='Republican voter' & att == 0 ~ 0,
                ## 
                pid.cat=='Democratic voter' & att == 1 ~ 0,
                pid.cat=='Democratic voter' & att == 0 ~ 1,
                ),
        )
    %>% drop_na(att.congruent) 
)
cat("\n ------------ Checking (attitude congruent coding) ------------\n")
(
    df.ds 
    %>% group_by(cue, `Party cue effect`, pid.cat, att, att.congruent) 
    %>% summarise(n=n() ) 
) %>% print(., n=Inf, width=Inf) 
(
    df.rs
    %>% group_by(cue, `Party cue effect`, pid.cat, att, att.congruent) 
    %>% summarise(n=n() ) 
) %>% print(., n=Inf, width=Inf) 

## estimation
## ----------
cat('Estimating RE (no adjustment variables)...')
formula.baseline=glue("att.congruent~`Party cue effect`*`Awareness effect`",
               " + (1 + `Party cue effect`*`Awareness effect` | policy)",
               " + (1 | rid)")
res.ds = glmer(formula.baseline, data=df.ds, family="binomial")
res.rs = glmer(formula.baseline, data=df.rs, family="binomial")
## 
cat('Estimating RE (with adjustment variables)...')
formula.adj = as.formula(paste(formula.baseline, "+", get_controls(CONTROLS)))
res.ds.adj = glmer(formula.adj, data=df.ds, family="binomial")
res.rs.adj = glmer(formula.adj, data=df.rs, family="binomial")
cat('Done!')

## *** DONE difference-in-differences

res.did.no.mc = (
    res.pp.no.mc
    %>% filter(ref=='G0') 
    %>% mutate(did = future_map2(.x=fit, .y=pid, function(fit=.x, pid=.y) estimate.did(fit, pid))) 
    %>% unnest(did)
)
## res.did %>% glimpse()

## checking 
## --------
cat("\n ------------ Checking (DiD computation) ------------\n")
check_did(res.did, cue='ds', row=3)
check_did(res.did, cue='rs', row=6)


## *** DONE pooled (overall average)

## Preparing the data
## ------------------
vars = c("rid", "pid.cat", "cue", "aware", "policy", "att", CONTROLS)
df.ds = (
    df  
    %>% filter(mc.prior=='none') 
    %>% select(vars)
    %>% filter(cue %in% c('co', 'ds'))    
    %>% mutate(
            `Awareness effect`=aware,
            `Party cue effect` = ifelse(cue=='co', 0, 1),
            att.congruent = case_when(
                pid.cat=='Democratic voter' & att == 1 ~ 1,
                pid.cat=='Democratic voter' & att == 0 ~ 0,
                ## 
                pid.cat=='Republican voter' & att == 1 ~ 0,
                pid.cat=='Republican voter' & att == 0 ~ 1,
                ),
            )  
    %>% drop_na(att.congruent)   
)
df.rs = (
    df 
    %>% filter(mc.prior=='none') 
    %>% select(vars)
    %>% filter(cue %in% c('co', 'rs'))  
    %>% mutate(
            `Awareness effect`=aware,
            `Party cue effect` = ifelse(cue=='co', 0, 1),
            att.congruent = case_when(
                pid.cat=='Republican voter' & att == 1 ~ 1,
                pid.cat=='Republican voter' & att == 0 ~ 0,
                ## 
                pid.cat=='Democratic voter' & att == 1 ~ 0,
                pid.cat=='Democratic voter' & att == 0 ~ 1,
                ),
        )
    %>% drop_na(att.congruent) 
)
cat("\n ------------ Checking (attitude congruent coding) ------------\n")
(
    df.ds 
    %>% group_by(cue, `Party cue effect`, pid.cat, att, att.congruent) 
    %>% summarise(n=n() ) 
) %>% print(., n=Inf, width=Inf) 
(
    df.rs
    %>% group_by(cue, `Party cue effect`, pid.cat, att, att.congruent) 
    %>% summarise(n=n() ) 
) %>% print(., n=Inf, width=Inf) 
## 
## estimation
## ----------
cat('Estimating RE (no adjustment variables)...')
formula.baseline=glue("att.congruent~`Party cue effect`*`Awareness effect`",
               " + (1 + `Party cue effect`*`Awareness effect` | policy)",
               " + (1 | rid)")
res.ds.no.mc = glmer(formula.baseline, data=df.ds, family="binomial")
res.rs.no.mc = glmer(formula.baseline, data=df.rs, family="binomial")
## 
cat('Estimating RE (with adjustment variables)...')
formula.adj = as.formula(paste(formula.baseline, "+", get_controls(CONTROLS)))
res.ds.no.mc.adj = glmer(formula.adj, data=df.ds, family="binomial")
res.rs.no.mc.adj = glmer(formula.adj, data=df.rs, family="binomial")
cat('Done!')


## *** robustness to MC (MC interaction)


cat("\n\nEstimating hierarchical model. This may take a while...")
f = glue("att ~ pid.cat*cue*aware*mc.prior",
         " + (1 + cue*aware | policy)",
         " + (1 | rid)")
df.tmp = (
    df
    %>% mutate(mc.prior = case_when(mc.prior=='none' ~ 0,
                                    mc.prior %in% c('mc1', 'mc2') ~ 1)) 
)
res.agg.mc.int = (
    tibble(covars = list(1, CONTROLS))   
    %>% rowwise(.)
    %>% mutate(formula = glue("{f} + {get_controls(covars)}"))   
    %>% crossing(model=c('lpm', 'logistic')) 
    %>% ungroup(.) 
    %>% mutate(
            ncovars = future_map_dbl(.x=covars, function(.x) length(.x)-1),
            fit = future_map2(.x=formula, .y=model, function(.x, .y)
                if (.y=='lpm')
                {
                    lmer(.x,
                         data=df.tmp %>% as_label(aware, pid.cat))
                }else {
                    glmer(.x,
                          data=df.tmp %>% as_label(aware, pid.cat),
                          family='binomial')
                }),
            summ = future_map(.x=fit, function(.x) tidy(.x, conf.int=TRUE)),
            glance = future_map(.x=fit, function(.x) glance(.x)),
            pred = future_pmap(list(fit), function(fit)
                predictions(fit, by=c('aware', 'pid.cat', 'cue'))
                )
        )
)
cat("done!\n\n")



## ** DONE balance
## *** SMD

treat     = "treat.group"
ref.group = 'G0'
method    = 'optimal'
distance  = "glm"
threshold = 0.1


## Note: this procedure compares covariate balance between a reference group
##  (e.g., control) and each treatment group
## computing
## ---------
idx = !str_detect(CONTROLS, pattern="nfa|nfc")
f = formula(glue("treat ~ {get_controls(CONTROLS[idx])}"))
df.control = (
    df 
    %>% filter(treat.group==ref.group) 
    %>% select(CONTROLS, treat=!!treat)  
)
res.bal = (
    df  
    %>% select(CONTROLS, treat=!!treat)  
    %>% drop_na(treat)  
    %>% nest(-treat) 
    %>% filter(treat!=!!ref.group)
    %>% mutate(
            data=future_map(.x=data, function(.x)
                .x
                %>% bind_rows(df.control) 
                %>% mutate(treat = case_when(treat==!!ref.group ~ 0, T ~ 1)) 
                ),
            fit=future_map(.x=data, function(.x)
                matchit(formula=f, data=.x, method=method , distance=distance)),
            balance = future_map(.x=fit, function(.x)
                bal.tab(.x, thresholds = c(m = .1), un = TRUE)$Balance  
                %>% tibble(., Covariate=row.names(.))  
                %>% filter(Type!='Distance')  
                %>% select(Covariate, Diff.Un, contains("Thresh")))
                )
        )


## ** DONE missing

tab = (
    df 
    %>% distinct(rid, .keep_all=TRUE) 
    %>% drop_na(cue, aware)  
    %>% select(att, cue, aware, pid.cat, CONTROLS) 
    %>% mutate(missing = as.numeric(rowSums(is.na(.)) > 0))
)
## tab
controls = CONTROLS[!CONTROLS %in%  c('nfc', 'nfa')]
f = glue("missing ~ cue + aware")
f.adj = glue("missing ~ {get_controls(c('cue', 'aware', 'pid.cat', controls))}")
res.miss = list(glm(f, data=tab, family = 'binomial'),
                glm(f.adj, data=tab, family = 'binomial'))
summary(res.miss[[1]])
summary(res.miss[[2]])


## * Tables
## ** DONE Table F1 to F4 (Survey vs Census)

tables = tibble::tribble(
                     ~table   , ~var  , ~label,
                     'tab-f1', 'educ' , 'Education',
                     'tab-f2', 'inchh', 'Income',
                     'tab-f3', 'age'  , 'Age',
                     'tab-f4', 'race' , 'Race'
                 ) %>% 
    mutate(caption = glue("\\label{{{table}}}{label}: Sample versus Census") )

sample.vs.census(df, 'educ' , tables, SAVE, F, column.spec="7cm")
sample.vs.census(df, 'inchh', tables, SAVE, F)
sample.vs.census(df, 'age'  , tables, SAVE, F)
sample.vs.census(df, 'race' , tables, SAVE, F)

## ** DONE Table F5

table = 'tab-f5'
## 
tab = (
    df   
    %>% distinct(rid, .keep_all=TRUE) 
    %>% select(CONTROLS, policy, pid, psid, att) 
) 
tab = tab %>% descriptive.statistics()
tab
## 
caption = glue('\\label{{{table}}}Descriptive Statistics') 
tabl = (
    tab 
    %>% kable(., "latex", booktabs = T, caption=caption,
              escape=T, align=c("l", rep("c", ncol(.)-1)),
              digits=4, longtable = F, table.envir = "table",
              linesep = NULL)
)
tabl
if(SAVE){save.table(tab, tabl, PATH_MAN_TABLES, table)}

## ** DONE Table F6

table = 'tab-f6'
## 
tab = (
    df 
    %>% group_by(treat.group, policy) 
    %>% summarise(n = n())  
    %>% group_by(treat.group) 
    %>% summarise(
            Mean = mean(n, na.rm=T),
            Min  = min(n, na.rm=T),
            Max  = max(n, na.rm=T) 
        )  
    %>% mutate(Group = treat.group %>% remove_val_labels(),
               Cue = treat.group %>% sjlabelled::as_label(.) ) 
    %>% select(Group, Cue, everything(), -treat.group)
)
tab %>% print(., n=Inf, width=Inf) 
## 
caption = glue("\\label{{{table}}}Sample size by treatment group and across policies") 
tabl = (
    tab 
    %>% kable(., "latex", booktabs = T, caption=caption,
              escape=F, align=c("l", 'l', rep("c", ncol(.)-2)),
              digits=4, longtable = F, table.envir = "table",
              linesep = NULL)
    %>% add_header_above(
            header = c(" "= 2,
                       'Sample size across\npolicy topic'=3)) 
)
tabl
if(SAVE){save.table(tab, tabl, PATH_MAN_TABLES, table)}

## ** DONE Table F7

table = 'tab-f7'
## 
omit = NULL
tab = modelsummary(res.miss,
                   statistic='({conf.low}, {conf.high})',
                   stars=TRUE, ## c('*' = .1, '**' = .05, "***"=0.01),
                   vcov = 'classical', #"classical", "robust", "stata", "HC4", "HC0",
                   ## 
                   coef_omit = omit,
                   coef_rename=c(
                       "cueds"="Dem. support cue",
                       "cuers"="Rep. support cue",
                       "pid.catRepublican voter" = 'Rep. voter',
                       unlist(LABELS.VARS)
                   ),
                   output='data.frame',
                   )%>% 
    select(-part, -statistic)%>% 
    mutate(term = ifelse(lag(term)==term & !is.na(lag(term)), "", term))
tab
caption = glue("\\label{{{table}}}Logistic regression of the missing cases indicator on treatment conditions and adjustment variables.")
tabl = (
    tab
    %>% kable(., "latex", booktabs = T, caption=caption,
              escape=F, align=c("l", rep("c", ncol(.)-1)),
              digits=4, longtable = F, table.envir = "table",
              linesep = NULL) 
    %>% kable_styling(latex_options = c("scale_down", "repeat_header", "hold_position"),
                      position = "center", font_size=NULL) 
)
tabl
if (SAVE) {
    cat(glue("Saving {table}...") )
    fn = file.path(PATH_MAN_TABLES, glue("{table}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    writeLines(text=tabl, glue("{fn}.tex"))
    cat("done!\n")
}


## ** DONE Table G1 to G4 (regressions with PID)

ref='G0'
tables = (
    tibble::tribble(
                ~policy , ~table  , ~pid,
                'gun'   , 'tab-g1', 'pid',
                'care'  , 'tab-g1', 'pid',
                'min'   , 'tab-g2', 'pid',
                'tax'   , 'tab-g2', 'pid',
                ##
                'elec'  , 'tab-g3', 'pid',
                'ocean' , 'tab-g3', 'pid',
                'fda'   , 'tab-g4', 'pid',
                'admin' , 'tab-g4', 'pid',
            )
)
## 
caption = glue("OLS estimates with standard errors clustered at respondent level.",
               " The outcomes are attitudes toward policies (columns).",
               " Party identification goes from Democratic (low values)",
               " to Republican voters (high values).") 
tabs = (
    res.pp 
    %>% filter(ref==!!ref) 
    %>% filter(pid=='pid') 
    %>% left_join(., df %>%
                     select(policy, policy.group) %>%
                     distinct(., .keep_all=TRUE), by=c('policy'))  
    %>% select(policy, policy.group, pid, ref, covars, ncovars, fit) 
    %>% left_join(., tables, by=c('policy', 'pid')) 
    %>% arrange(table, policy.group, pid)    
    %>% mutate(model.label = glue("{policy %>% sjlabelled::as_label(.) }"))
    %>% nest(-table) 
    %>% mutate(
            tab = future_map(.x=data, function(.x)
                modelsummary(.x$fit %>% setNames(., .x$model.label),
                             statistic='({conf.low}, {conf.high})',
                             stars=TRUE, ## c('*' = .1, '**' = .05, "***"=0.01),
                             vcov = 'classical', #"classical", "robust", "stata", "HC4", "HC0",
                             coef_omit = "Intercept",
                             output='data.frame',
                             )%>% 
                select(-part, -statistic)%>% 
                mutate(term = ifelse(lag(term)==term & !is.na(lag(term)), "", term),
                       term = stringr::str_replace_all(string=term, pattern="treat.group",
                                                       replacement="") ) %>%
                rename(" "=term)  
                ),
            caption = future_map2_chr(.x=data, .y=table, function(.x, .y)
                glue("\\label{{{.y}}}{caption}",
                     " Party identification measure: {unique(toupper(.x$pid))}")),
            ##
            ## latex
            ## -----
            tab.latex = purrr::map2(.x=tab, .y=caption, function(.x, .y)
            (
                .x
                %>% kable(., "latex", booktabs = T, caption=.y,
                          escape=T, align=c("l", rep("c", ncol(.)-1)),
                          digits=4, longtable = F, table.envir = "table",
                          linesep = NULL)
                %>% kable_styling(latex_options = c("scale_down", "repeat_header"),
                                  position = "center", font_size=NULL)  
                %>% add_header_above(
                        header = c(' '= 1,
                                   'Outcome: Policy attitude'=4), escape=FALSE)  
                %>% column_spec(2:5, width = "2.5cm") 
                %>% str_replace_all(string=.,
                                    pattern="..\\\\centering\\\\arraybackslash.p", replacement="x")
            )
            )
        )
)
tabs %>% print(n=100)
## 
if (SAVE) {
    for (i in 1:nrow(tabs))
    {
        table = tabs$table[[i]]
        tab   = tabs$tab[[i]]
        tabl  = tabs$tab.latex[[i]]

        cat(glue("Saving {table}...") )
        fn = file.path(PATH_MAN_TABLES, glue("{table}"))
        write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
        write_xlsx(x=tab, path=glue("{fn}.xlsx"))
        writeLines(text=tabl, glue("{fn}.tex"))
        cat("done!\n")
    }
}

## checking
## --------
## tab$tab[[1]]
## tab$tab.latex[[1]]

## ** DONE Table G9

## See Table 3 of the main paper

## ** DONE Table H1: manipulation check stacked

table = "tab-h1"
## 
# table
tab = modelsummary(res.mc.stack,
                   statistic='({conf.low}, {conf.high})',
                   stars=TRUE, ## c('*' = .1, '**' = .05, "***"=0.01),
                   vcov = 'classical', #"classical", "robust", "stata", "HC4", "HC0",
                   coef_omit = "Intercept",
                   coef_rename=c('mcmc1'='MC1',
                                 'aware'='Aware',
                                 'pid.catRepublican voter'='Rep. voter',
                                 unlist(LABELS.VARS[CONTROLS])
                                 ),
                   gof_omit = 'Errors|F|AIC|BIC|Lik', # regexp to exclude stats summary (AIC, etc)
                   output='data.frame',
                   )%>% 
    select(-part, -statistic)%>% 
    mutate(term = ifelse(lag(term)==term & !is.na(lag(term)), "", term))
tab

## 
## latex
caption=glue("\\label{{{table}}}Linear regression models of the stacked manipulation check questions on the awareness message.") 
pvalues = glue("\\multicolumn{{{ncol(tab)}}}{{r}}{{\\rule{{0pt}}{{1em}}}",
               "+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001}") 
tabl = (
    tab
    %>% kable(., "latex", booktabs = T, caption=caption, escape=F,
              align=c("l", rep("c", ncol(.)-1)),
              digits=4, longtable = F, table.envir = "table",
              linesep = NULL)
    %>% kable_styling(latex_options = c("scale_down", "repeat_header", "hold_position"),
                      position = "center", font_size=NULL)
    ## add nlines
    %>% row_spec(., 28, extra_latex_after = 'MC Random effects & Yes & Yes & Yes & Yes & No & No& No & No\\\\') 
    %>% row_spec(., 28, extra_latex_after = '\\midrule')  
    %>% landscape() 
)
tabl
## 
if (SAVE) {
    cat(glue("Saving {table}...") )
    fn = file.path(PATH_MAN_TABLES, glue("{table}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    writeLines(text=tabl, glue("{fn}.tex"))
    cat("done!\n")
}



## ** DONE Table I1 to I4   (PSID regressions)

ref='G0'
tables = (
    tibble::tribble(
                ~policy , ~table  , ~pid,
                'gun'   , 'tab-i1', 'psid',
                'care'  , 'tab-i1', 'psid',
                'min'   , 'tab-i2', 'psid',
                'tax'   , 'tab-i2', 'psid',
                ##
                'elec'  , 'tab-i3', 'psid',
                'ocean' , 'tab-i3', 'psid',
                'fda'   , 'tab-i4', 'psid',
                'admin' , 'tab-i4', 'psid'
            )
)
## 
caption = glue("OLS estimates with standard errors clustered at respondent level.",
               " The outcomes are attitudes toward policies (columns).",
               " Party identification goes from Democratic (low values)",
               " to Republican voters (high values).") 
tabs = (
    res.pp 
    %>% filter(ref==!!ref)  
    %>% filter(pid=='psid') 
    %>% left_join(., df %>%
                     select(policy, policy.group) %>%
                     distinct(., .keep_all=TRUE), by=c('policy'))  
    %>% select(policy, policy.group, pid, ref, covars, ncovars, fit) 
    %>% left_join(., tables, by=c('policy', 'pid')) 
    %>% arrange(table, policy.group, pid)    
    %>% mutate(model.label = glue("{policy %>% sjlabelled::as_label(.) }"))
    %>% nest(-table) 
    %>% mutate(
            tab = future_map(.x=data, function(.x)
                modelsummary(.x$fit %>% setNames(., .x$model.label),
                             statistic='({conf.low}, {conf.high})',
                             stars=TRUE, ## c('*' = .1, '**' = .05, "***"=0.01),
                             vcov = 'classical', #"classical", "robust", "stata", "HC4", "HC0",
                             ## 
                             coef_omit = "Intercept",
                             output='data.frame',
                             )%>% 
                select(-part, -statistic)%>% 
                mutate(term = ifelse(lag(term)==term & !is.na(lag(term)), "", term),
                       term = stringr::str_replace_all(string=term, pattern="treat.group",
                                                       replacement="") ) %>%
                rename(" "=term)  
                ),
            caption = future_map2_chr(.x=data, .y=table, function(.x, .y)
                glue("\\label{{{.y}}}{caption}",
                     " Party identification measure: {unique(toupper(.x$pid))}")),
            ##
            ## latex
            ## -----
            tab.latex = purrr::map2(.x=tab, .y=caption, function(.x, .y)
            (
                .x
                %>% kable(., "latex", booktabs = T, caption=.y,
                          escape=T, align=c("l", rep("c", ncol(.)-1)),
                          digits=4, longtable = F, table.envir = "table",
                          linesep = NULL)
                %>% kable_styling(latex_options = c("scale_down", "repeat_header"),
                                  position = "center", font_size=NULL)  
                %>% add_header_above(
                        header = c(' '= 1,
                                   'Outcome: Policy attitude'=4), escape=FALSE)  
                %>% column_spec(2:5, width = "2.5cm") 
                %>% str_replace_all(string=.,
                                    pattern="..\\\\centering\\\\arraybackslash.p", replacement="x")
            )
            )
        )
)
tabs %>% print(n=100)
## 
if (SAVE) {
    for (i in 1:nrow(tabs))
    {
        table = tabs$table[[i]]
        tab   = tabs$tab[[i]]
        tabl  = tabs$tab.latex[[i]]
        ## 
        cat(glue("Saving {table}...") )
        fn = file.path(PATH_MAN_TABLES, glue("{table}"))
        write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
        write_xlsx(x=tab, path=glue("{fn}.xlsx"))
        writeLines(text=tabl, glue("{fn}.tex"))
        cat("done!\n")
    }
}


## ** DONE Table I5

table = "tab-i5"

terms <- c(
    "(Intercept)"                               = "Dem. voter (baseline)",
    "awareAware"				= "Dem. voter x Aware",
    "cueds"					= "Dem. voter x Dem. support cue",
    "cueds:awareAware"				= "Dem. voter x Dem. support cue x Aware",
    "cuers"					= "Dem. voter x Rep. support cue",
    "cuers:awareAware"				= "Dem. voter x Rep. support cue x Aware",
    "pid.catRepublican voter"			= "Rep. voter",
    "pid.catRepublican voter:awareAware"	= "Rep. voter x Aware",
    "pid.catRepublican voter:cueds"		= "Rep. voter x Dem. support cue",
    "pid.catRepublican voter:cueds:awareAware"	= "Rep. voter x Dem. support cue x Aware",
    "pid.catRepublican voter:cuers"		= "Rep. voter x Rep. support cue",
    "pid.catRepublican voter:cuers:awareAware"	= "Rep. voter x Rep. support cue x Aware",
    ## 
    "mc.prior"                                  =
        "Dem. voter x MC before",
    "awareAware:mc.prior"			=
        "Dem. voter x Aware x MC before",
    "cueds:mc.prior"				=
        "Dem. voter x Dem. support cue x MC before",
    "cueds:awareAware:mc.prior"	                =
        "Dem. voter x Dem. support cue x Aware x MC before",
    "cuers:mc.prior"				=
        "Dem. voter x Rep. support cue x MC before",
    "cuers:awareAware:mc.prior"			=
        "Dem. voter x Rep. support cue x Aware x MC before",
    "pid.catRepublican voter:mc.prior"		=
        "Rep. voter x MC before",
    "pid.catRepublican voter:awareAware:mc.prior"=
        "Rep. voter x Aware x MC before",
    "pid.catRepublican voter:cueds:mc.prior"	=
        "Rep. voter x Dem. support cue x MC before",
    "pid.catRepublican voter:cueds:awareAware:mc.prior"	=
        "Rep. voter x Dem. support cue x Aware x MC before",
    "pid.catRepublican voter:cuers:mc.prior"		=
        "Rep. voter x Rep. support cue x MC before",
    "pid.catRepublican voter:cuers:awareAware:mc.prior"	=
        "Rep. voter x Rep. support cue x Aware x MC before"
)

res.agg.mc.int  = res.agg.mc.int %>% mutate(
                                         ncovars.label = case_when(ncovars==0~"Unadjusted",
                                                                   T~'Adjusted\\tnote{a}')
                                     ) 
mods        = res.agg.mc.int %>% filter(model=='lpm') %>% pull(fit)
names(mods) = res.agg.mc.int %>% filter(model=='lpm') %>% pull(ncovars.label)


tab = (
    modelsummary(mods,
                   estimate  = "{estimate}{stars} ({conf.low}, {conf.high})",
                   statistic=NULL,
                   stars=TRUE,
                   vcov = 'classical', 
                   ## 
                   coef_omit = "SD|Cor",
                   gof_omit = 'Errors|F|RMSE|AIC|BIC|ICC', 
                   coef_map=terms,
                   output='data.frame',
                   )%>% 
    select(-part, -statistic)%>% 
    mutate(term = ifelse(lag(term)==term & !is.na(lag(term)), "", term),
           term = str_replace_all(string=term, pattern=':', replacement=' x '))
)
tab

## latex
## -----
caption = glue('\\label{{{table}}}Linear probability model estimating the effect',
               ' of the treatment conditions on partisans policy support.',
               ' The 95\\% confidence intervals are shown in parenthesis.',
               ' Model shows the effect of asking the manipulation checks (MC)',
               ' questions before the policy-cue questions.'
               )
pvalues = glue("\\\\footnotesize  + p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001\\\\\\\\")
controls = glue_collapse(unlist(LABELS.VARS[CONTROLS]), ", ")
controls = glue("Adjustment variables: {controls}")
tabl = (
    tab
    %>% rename(' '=term) 
    %>% kable(., "latex", booktabs = T, caption=caption,
              ## col.names = T,
              escape=F,
              ## align=c("l", rep("c", ncol(.)-1)),
              align=c("l", rep("r", ncol(.)-1)),
              digits=4, longtable = F, table.envir = "table",
              linesep = NULL) 
    %>% kable_styling(latex_options = c("scale_down", "repeat_header", "hold_position"),
                      position = "center", font_size=NULL)  
    %>% footnote(
            general			= pvalues,
            general_title	= "", ## title of the general legend
            threeparttable          = T,
            escape                  = F,
            alphabet		= c(a = controls),
            footnote_as_chunk	= F,  # F means fotenotes are spread in multiple lines
            title_format		= c("italic")
        ) 
)
tabl




if (SAVE) {
    cat(glue("Saving {table}...") )
    fn = file.path(PATH_MAN_TABLES, glue("{table}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    writeLines(text=tabl, glue("{fn}.tex"))
    cat("done!\n")
}

## ** DONE Table i6

table = 'tab-i6'
## 
## table
## -----
plan(sequential)
mods = list('Dem. Support (1)' = res.ds,
            'Dem. Support (2)' = res.ds.adj,
            'Rep. Support (3)' = res.rs,
            'Rep. Support (4)' = res.rs.adj
            )
add.info = tribble(~`term`,~`Dem. Support (1)`,~`Dem. Support (2)`,
                   ~`Rep. Support (3)`,~`Rep. Support (4)`,
                  'Controls$^a$', 'No', "Yes", "No", "Yes") 
attr(add.info, 'position') = c(7)
tab = (
    modelsummary(mods,
                 statistic='({conf.low}, {conf.high})',
                 escape=F,
                 stars=T,
                 vcov = 'classical', 
                 coef_rename = unlist(LABELS.VARS),
                 add_rows = add.info,
                 coef_omit = "Intercept|^Cor .*|educ|age|inc|white|female|nfc|nfa",
                 gof_omit = 'F|BIC|AIC|R2|RMSE|ICC', 
                 output='data.frame',
                 )
    %>% select(-part, -statistic)
    %>% mutate(term = ifelse(lag(term)==term & !is.na(lag(term)), "", term),
               term = str_replace(string=term, pattern=":", replacement=" x ") %>% 
                   str_replace(string=., pattern="`Party cue effect`", replacement="cue") %>% 
                   str_replace(string=., pattern="`Awareness effect`", replacement="awareness") %>% 
                   str_replace(string=.,
                               pattern="Party cue effect x Awareness effect",
                               replacement="Party cue x Awareness effect"),
               ) 
    %>% rename(" "=term)   
    %>% rename_with(~str_replace(string=., pattern="(Dem|Rep). Support", replacement=""))
)
cat(glue("\n\n{names(mods)}\n\n") )
tab  %>% print(n=100, na.print = '')

## 
## latex
## -----
adj.vars = paste(unlist(LABELS.VARS[names(LABELS.VARS) %in% CONTROLS]),
                 collapse=', ')
pvalues=glue("\\\\footnotesize  + p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001\\\\\\\\")
caption=glue("\\label{{{table}}} Causal effect of party cues and",
             " awareness across policy issues. Point estimates",
             " are logistic regression coefficients with random",
             " effects at subject and policy levels.")
tabl = (
    tab 
    %>% kable(., "latex", booktabs = T, caption=caption,
              escape=F, align=c("l", rep("c", ncol(.)-1)),
              digits=4, longtable = F, table.envir = "table",
              linesep = NULL)
    %>% kable_styling(latex_options = c("scale_down", "repeat_header", "hold_position"),
                      position = "center", font_size=NULL)   
    %>% add_header_above(
            header = c(' '= 1,
                       'Party Cue: Dem. Support'=2,
                       'Party Cue: Rep. Support'=2
                       ),
            escape=FALSE)  
    %>% add_header_above(
            header = c(' '= 1,
                       'Outcome: Partisan Policy Attitude Congruent with Party Cue'=4),
            escape=FALSE)  
    %>% kableExtra::footnote(
                        general			= pvalues,
                        general_title	= "", ## title of the general legend
                        alphabet	= c(a = glue("Controls: {adj.vars}") ),
                        threeparttable          = T,
                        escape                  = F,
                        footnote_as_chunk	= F,  # F means fotenotes are spread in multiple lines
                        title_format		= NULL
                    ) 
)
tabl
if (PRR) {
    tabl = str_replace(string=tabl,
                       pattern="\\\\caption",
                       replacement="\\\\placeholdertable{120}{-150}\\\n\\\\caption")
}
## cat(tabl)
if (SAVE) {save.table(tab, tabl, PATH_MAN_TABLES, table)}

## 
cat("\n ------------ Report ------------\n")
for (mod in 1:length(mods))
{
    glue("Collecting {mod}")
    tab = avg_predictions(mods[[mod]],
                          variables = c('Party cue effect', 'Awareness effect'),
                          type='response')%>% 
        as_tibble() %>%
        rename("pred"=estimate)
    ## 
    print(glue("Model: {names(mods)[mod]}"))
    print(tab)
}


## ** DONE Table i7 (logistic)

table='tab-i7'
## 
## table
## -----
terms <- c(
    "(Intercept)"                               = "Dem. voter (baseline)",
    "awareAware"				= "Dem. voter x Aware",
    "cueds"					= "Dem. voter x Dem. support cue",
    "cueds:awareAware"				= "Dem. voter x Dem. support cue x Aware",
    "cuers"					= "Dem. voter x Rep. support cue",
    "cuers:awareAware"				= "Dem. voter x Rep. support cue x Aware",
    "pid.catRepublican voter"			= "Rep. voter",
    "pid.catRepublican voter:awareAware"	= "Rep. voter x Aware",
    "pid.catRepublican voter:cueds"		= "Rep. voter x Dem. support cue",
    "pid.catRepublican voter:cueds:awareAware"	= "Rep. voter x Dem. support cue x Aware",
    "pid.catRepublican voter:cuers"		= "Rep. voter x Rep. support cue",
    "pid.catRepublican voter:cuers:awareAware"	= "Rep. voter x Rep. support cue x Aware"
)
res.agg = res.agg %>% mutate(ncovars.label = case_when(ncovars==0~"Unadjusted", T~'Adjusted\\tnote{a}')) 
mods = res.agg %>% filter(model=='logistic') %>% pull(fit)
names(mods) = res.agg %>% filter(model=='lpm') %>% pull(ncovars.label)
tab = (
    modelsummary(mods,
                   estimate  = "{estimate}{stars} ({conf.low}, {conf.high})",
                   ## statistic='({conf.low}, {conf.high})',
                   statistic=NULL,
                   stars=TRUE,
                   vcov = 'classical', 
                   ## 
                   coef_omit = "SD|Cor",
                   gof_omit = 'Errors|F|RMSE|AIC|BIC|ICC', 
                   coef_map=terms,
                   ## add_rows = info.controls,
                   output='data.frame',
                   )%>% 
    select(-part, -statistic)%>% 
    mutate(term = ifelse(lag(term)==term & !is.na(lag(term)), "", term),
           term = str_replace_all(string=term, pattern=':', replacement=' x '))
)
tab

## latex
## -----
caption = glue('\\label{{{table}}}Logistic regression estimates of the effect',
               ' of the treatment conditions on partisans policy support.',
               ' The 95\\% confidence intervals are shown in parenthesis.')
pvalues = glue("\\\\footnotesize  + p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001\\\\\\\\")
controls = glue_collapse(unlist(LABELS.VARS[CONTROLS]), ", ")
controls = glue("Adjustment variables: {controls}")
tabl = (
    tab
    %>% rename(' '=term) 
    %>% kable(., "latex", booktabs = T, caption=caption,
              ## col.names = T,
              escape=F,
              ## align=c("l", rep("c", ncol(.)-1)),
              align=c("l", rep("r", ncol(.)-1)),
              digits=4, longtable = F, table.envir = "table",
              linesep = NULL) 
    %>% kable_styling(latex_options = c("hold_position", "scale_down", "repeat_header"),
                      position = "center", font_size=NULL)  
    %>% footnote(
            general			= pvalues,
            general_title	= "", ## title of the general legend
            threeparttable          = T,
            escape                  = F,
            alphabet		= c(a = controls),
            footnote_as_chunk	= F,  # F means fotenotes are spread in multiple lines
            title_format		= c("italic")
        ) 
)
tabl


if (SAVE) {
    cat(glue("Saving {table}...") )
    fn = file.path(PATH_MAN_TABLES, glue("{table}"))
    write.table(x=tab, file=glue("{fn}.csv") , row.names=F, sep=';')
    write_xlsx(x=tab, path=glue("{fn}.xlsx"))
    writeLines(text=tabl, glue("{fn}.tex"))
    cat("done!\n")
}


## for reporting in the paper
## --------------------------
ncovars = 0
mods = res.agg %>% filter(model=='lpm') %>% filter(ncovars==!!ncovars) %>% pull(fit)
tab.report = (
    modelsummary(mods,
                 estimate = glue("{{estimate}} ; {{conf.low}} ; {{conf.high}};",
                                 " {{p.value}} ;  {{stars}}"),
                 statistic=NULL, stars=TRUE, 
                 vcov = 'classical', coef_omit = "SD|Cor",
                 coef_map=terms, gof_omit = '.', output='data.frame')
)
tab.report 

(
    tab.report %>% 
    select(-part, -statistic)%>% 
    mutate(term = ifelse(lag(term)==term & !is.na(lag(term)), "", term),
           term = str_replace_all(string=term, pattern=':', replacement=' x '))
    %>% separate(., col="(1)",
                 into=c("estimate", "conf.low", 'conf.high', 'p.value', 'stars'), sep=";")  
    %>% mutate(estimate= 100*as.numeric(estimate),
               conf.low = 100*as.numeric(conf.low),
               conf.high= 100*as.numeric(conf.high),
               p.value.eq = case_when(str_detect(p.value, pattern="<")~"$<$", T~"="),
               p.value = str_trim(p.value) %>%
                   str_replace_all(string=., pattern="<", replacement="") ,
               stars = str_trim(stars),
               ##
               estimate = glue("{estimate} ([{conf.low}, {conf.high}];",
                                 " p-value {p.value.eq} {p.value})")
               ) 
    %>% tibble()  
    %>% select(term, estimate)
)%>% print(., n=Inf, width=Inf) 

## * Figures
## ** DONE Figure F1

figure = 'fig-f1'
## 
## 
treat     = "treat.group"
ref.group = 'G0'
## table
## -----
tab = (
    res.bal 
    %>% select(treat, balance) 
    %>% unnest(balance)  
    %>% set_value_labels(Covariate=setNames(names(unlist(LABELS.VARS[CONTROLS])),
                                            unlist(LABELS.VARS[CONTROLS])))  
    %>% sjlabelled::as_label(.)  
    %>% mutate(diff = abs(Diff.Un))
)
tab %>% print(n=100)
## 
## plot
## ----
x = "diff"
y = "Covariate"
color    = NULL
fill     = 'treat'
facet1   = NULL
facet2   = NULL
leg      = 'Treatmet group'
title    = NULL
subtitle = NULL
caption  = NULL
dodge    = .1
ylab     = NULL
xlab     = glue('Absolute Standardized Mean Difference (reference group: {ref.group})') 
g = (
    tab
    %>% ggplot(.)
    + geom_vline(aes(xintercept=threshold), linetype="dashed", col="red")
    + geom_point(aes_string(x=x, y=y, fill=fill, shape=fill))
    + scale_colour_grey(start = 0, end = .7, na.value="red") 
    + scale_fill_grey(start = 0, end = .7, na.value="red") 
    + scale_shape_manual(values=c(21,22,23, 24,25))
    + labs(
          x        = xlab,
          y        = ylab,
          color    = leg, 
          fill     = leg,
          linetype = leg,
          shape    = leg,
          title    = title,
          subtitle = subtitle,
          caption  = caption
      )
    + ggtheme()
    + ggguides(ncol=2)
)
g
if (SAVE){save.figure(g, tab, PATH_MAN_FIGURES , figure, 8, 4.5)}



## ** DONE Figure G1 and G2

SAVE=T
figures = c('fig-g2', 'fig-g1')
for (figure in figures)
{
    ## 
    path   = PATH_MAN_FIGURES
    ## 
    pid = 'pid'
    policy = 'gun'
    policy = 'care'
    policy = 'admin'
    policy = 'elec'
    policy = 'min'
    policy = 'fda'
    policy = 'tax'
    policy = 'ocean'
    ncovars = 0
    ## 
    ## table
    ## -----
    gs = list()
    i  = 0
    for (treat in c('Dem. Support/Rep. Oppose',
                    'Rep. Support/Dem. Oppose'))
    {
        i = i+1
        tab= (
            res.pp
            %>% filter(pid==!!pid)  
            ## %>% filter(policy==!!policy)  
            %>% filter(ncovars==!!ncovars) 
            %>% select(policy, ncovars, pred) 
            %>% unnest(pred) 
            %>% mutate(
                    group = case_when(
                        treat.group== "G0" ~  'Control',
                        treat.group== "G1" ~ 'Dem. Support/Rep. Oppose',
                        treat.group== "G2" ~ 'Dem. Support/Rep. Oppose',
                        treat.group== "G3" ~ 'Rep. Support/Dem. Oppose',
                        treat.group== "G4" ~ 'Rep. Support/Dem. Oppose',
                        ),
                    aware = case_when(
                        ## treat.group== "G0" ~ ' Not aware',
                        ## treat.group== "G1" ~ ' Not aware',
                        ## treat.group== "G2" ~ 'Aware',    
                        ## treat.group== "G3" ~ ' Not aware',
                        ## treat.group== "G4" ~ 'Aware',    
                        treat.group== "G0" ~ 'Control',
                        treat.group== "G1" ~ ' Not aware',
                        treat.group== "G2" ~ 'Aware',    
                        treat.group== "G3" ~ ' Not aware',
                        treat.group== "G4" ~ 'Aware',    
                        ),
                    ) 
            %>% drop_na()  
            %>% filter(group == !!treat | group=='Control')  
            %>% mutate(policy = factor(policy,
                                       LABELS.VALUES[['policy.short']],
                                       names(LABELS.VALUES[['policy.short']])
                                       ))
        )
        ## plots
        ## -----
        x = "pid"
        y = "att"
        color    = 'aware'
        fill     = 'aware'
        facet1   = 'policy'
        facet2   = NULL
        leg      = 'Awareness'
        title    = NULL
        treat     = str_replace(string=treat, pattern='Dem.', replacement='Democrats in Congress')%>% 
            str_replace(string=., pattern='Rep.', replacement='Republicans in Congress')
        subtitle = glue("Party cue: {treat}")
        caption  = NULL
        dodge    = .1
        ylab     = 'Predicted probability of supporting the policy'
        xlab = "Voters' Party Identification"
        xbreaks = c(-.9, .9)
        xlabels = c('Democrat', 'Republican')
        g = (
            tab 
            %>% ggplot(.)
            + geom_line(aes_string(x=x, y=y, group=fill, color=fill, linetype=fill), size=1)
            + geom_ribbon(aes_string(x=x, ymin="ymin", ymax="ymax", linetype=fill, color=fill),
                          fill="transparent", alpha=0, show.legend=F,
                          data=.%>% filter(aware!='Control') )
            ## + scale_fill_grey(start = 0, end = .7, na.value="red") 
            ## + scale_colour_grey(start = 0, end = .7, na.value="red") 
            ## + scale_colour_manual(values = c("black", 'gray60', 'black')) 
            + scale_colour_manual(values = c("black", 'gray60', 'black')) 
            + scale_linetype_manual(values=c(1, 2, 2))
            + scale_x_continuous(breaks=xbreaks, limits=c(-1.2, 1.2), labels=xlabels)
            + facet_wrap(glue("~ {facet1}"), ncol=4,
                         labeller = labeller(groupwrap = label_wrap_gen(35)))
            + labs(
                  x        = xlab,
                  y        = ylab,
                  color    = leg, 
                  fill     = leg,
                  linetype = leg,
                  shape    = leg,
                  title    = title,
                  subtitle = subtitle,
                  caption  = caption
              )
            + ggguides()
            + ggtheme2()
            + theme(strip.placement = "outside",
                    strip.background = element_rect(colour="white", fill="white"),
                    strip.text.x = element_text(size=9, face='bold', hjust=0),
                    strip.text.y = element_text(size=9, face="bold", vjust=0)) 
        )
        gs[[i]] = g
    }
    ## if(figure=='fig-3'){ g=gs[[2]] }else{ g=gs[[1]] }
    if(figure=='fig-g2'){ g=gs[[2]] }else{ g=gs[[1]] }
    print(g)
    if (SAVE) {save.figure(g, tab, path, figure, 10, 6)}
}

## 
cat("\n ------------ Report ------------\n")
## policy=c('admin', 'elec')
policy=c('min', 'tax')
tab = (
    res.pp
    %>% filter(pid==!!pid) 
    %>% filter(ncovars==!!ncovars) 
    %>% filter(policy %in% !!policy) 
    %>% filter(ref=='G0') 
    %>% select(ref, policy, summ, fit, pred)
    %>% unnest(pred)    
    %>% filter(pid %in% c(-1, 1)) 
    %>% select(ref, policy, pid, treat.group, att)
    %>% mutate(att=100*att)
    %>% pivot_wider(names_from=treat.group,
                    values_from=att) 
    %>% mutate(
            `G3-G0` = G3-G0,
            `G4-G0 (aware)` = G4-`G0 (aware)`,
            did = abs(`G3-G0`) - abs(`G4-G0 (aware)`)
        ) 
    %>% select(ref,policy, pid, G3, G0, `G3-G0`, G4, 'G0 (aware)', `G4-G0 (aware)`, did)
)
tab %>% arrange(policy, pid, did) %>% print(n=100)
## 
tab = (
    res.pp
    %>% filter(pid==!!pid) 
    %>% filter(ncovars==!!ncovars) 
    %>% filter(policy %in% !!policy) 
    ## %>% filter(ref=='G0') 
    %>% select(ref, policy, summ, fit, pred)
    %>% unnest(summ) 
    %>% filter(str_detect(term, pattern="pid:.*(G3|G4)")) 
    %>% mutate(b = glue("{term}: ($\beta$={estimate}; p={p.value})",
                        .transformer = .signif_transformer(3))%>% 
                   str_replace(string=., pattern="treat.group", replacement="")
               ) 
    %>% select(ref, policy, term, estimate, b)
)
tab%>% print(n=100)
## 


## ** DONE Figure H1

figure = 'fig-h1'
## 
tab = (
    res.mc 
    %>% unnest(summ)  
    %>% filter(term=='aware')  
    %>% mutate(
            y.label = factor(y, levels=c('mc1', 'mc2'),
                             labels=c(glue("{str_wrap(LABELS.VARS[['mc1']], width = 55)}\n(MC 1)") ,
                                      glue("{str_wrap(LABELS.VARS[['mc2']], width = 55)}\n(MC 2)"))
                             ),
            mc.prior.label = case_when(
                mc.prior=='either'            ~ 'Before or after (aggreagated)',
                mc.prior=='none'              ~ 'After (none asked before)',
                mc.prior=='mc1'    & y=='mc1' ~ 'Before',
                mc.prior=='mc2'    & y=='mc2' ~ 'Before',
                mc.prior=='mc2'    & y=='mc1' ~ 'After',
                mc.prior=='mc1'    & y=='mc2' ~ 'After',
            )
            )
    %>% select(-data, -fit, -covars)
)
tab
## 
x = 'y.label'
y = "estimate"
color    = NULL
fill     = "mc.prior.label"
shape    = "mc.prior.label"
linetype = 'adj'
facet2   = NULL
leg      = "Manipulation check (MC) asked before or after the policy questions?"
title    = NULL
subtitle = NULL
caption  = NULL
dodge    = .3
ylab     = "Average effect of the awareness message on \nagreeing with the statement"
xlab     = 'Manipulation check (MC) statement'
g = (
    tab
    %>% ggplot(.)
    + geom_hline(aes(yintercept=0) ,linetype="dashed", col="red")
    + geom_errorbar(aes_string(x=x, color=fill, linetype=linetype, shape=shape,
                               ymin="conf.low", ymax="conf.high"),
                    width=.1, position = position_dodge(dodge)) 
    + geom_point(aes_string(x=x, y=y, fill=fill, linetype=linetype, shape=shape), alpha=1,
                    position = position_dodge(dodge))
    + scale_colour_grey(start = 0, end = .7, na.value="red") 
    + scale_fill_grey(start = 0, end = .7, na.value="red") 
    + scale_shape_manual(values=c(21,22, 25, 24))
    ## + facet_wrap(glue("~ {facet1}"), ncol = 1, scales='free')
    + labs(
          x        = xlab,
          y        = ylab,
          color    = leg, 
          fill     = leg,
          linetype = "Adjustment covariates used?",
          shape    = leg,
          title    = title,
          subtitle = subtitle,
          caption  = caption
      )
    + ggguides(ncol=2)
)
g
if (SAVE){
    save.figure(g, tab, PATH_MAN_FIGURES , figure, 8.5, 4)
}

## cat("\n ------------ Report ------------\n")
## mod = (
##     res.mc 
##     %>% filter(mc.prior=='mc2')   
##     %>% filter(y=='mc2') 
##     %>% filter(ncovars==1)  
##     %>% pull(fit) %>% extract2(1) 
## )




## * done

print(sessionInfo())
print(version)
print(difftime(Sys.time(), time.start, units='mins'))
cat('\nAll done!\n')

