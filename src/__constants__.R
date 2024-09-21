
cat("\nLoading constants...\n")

CONTROLS = c('educ.std', 'age.std', 'inchh.std', 'white', 'female', 'nfc', 'nfa')
LABELS = list(
    rid = 'Respondent id',
    ## 
    age         = 'Age',
    age.std     = 'Age (std)',
    educ        = 'Education',
    educ.std    = 'Education (std)',
    inchh       = 'Income ',
    inchh.std   = 'Income (std)',
    gender      = 'Gender',
    female      = 'Gender (female)',
    race        = 'Race',
    white       = 'Race (white)',
    ## 
    nfc = "Need for cognition",
    nfa = "Need for accuracy",
    ## 
    ##
    pid     = "Party identification (ANES)",
    pid.cat = "Party identification (ANES)",
    psid    = "Party identity (social)",
    ## 
    cue = 'Party cue',
    cue = c(
        'Control'                 ='co',
        'Dem. Support/Rep. Oppose'='ds',
        'Dem. Support/Rep. Oppose'='ro',
        'Rep. Support/Dem. Oppose'='do',
        'Rep. Support/Dem. Oppose'='rs' 
    ),
    cue.short = 'Party cue',
    cue.short = c(
        'Control'     ='co',
        'Dem. Support'='ds',
        'Dem. Support'='ro',
        'Rep. Support'='do',
        'Rep. Support'='rs' 
    ),
    aware = 'Awareness',
    aware = c(
        'Not aware' = 0,
        'Aware' = 1
    ),
    treat.group = 'Treatment group',
    treat.group = c(
        'Control'                          ='G0',
        'Control (aware)'                  ='G0 (aware)',
        'Dem. Support/Rep. Oppose'         ='G1',
        'Dem. Support/Rep. Oppose (aware)' ='G2',
        'Rep. Support/Dem. Oppose'         ='G3',
        'Rep. Support/Dem. Oppose (aware)' ='G4'
    ),
    policy = 'Policy',
    policy = c(
        'Gun owner checks'                                        = "gun" ,
        'Health care'                                             = "care",
        "Minimum wage"                                            = "min" ,
        "Taxes on the rich"                                       = "tax" ,
        ## 
        "Electronic filing systems for government agencies"       = "elec" ,
        "Increase budget for research institutes of oceanography" = "ocean",
        'Reallocate budget between from FDA and CPS'              = "fda"  ,
        "Training period for adminitrative government staff"      = "admin"
        ## 
    ),
    policy.short = 'Policy attitude',
    policy.short = c(
        "Minimum wage"          = 'min'  ,
        "Taxes on the rich"     = 'tax'  ,
        'Health care'           = 'care' ,
        'Gun owner checks'      = 'gun'  ,
        ##
        'Budget (FDA to CPS)'   = 'fda'  ,
        "Budget (oceanography)" = 'ocean',
        ##
        "Electronic filing"     = 'elec' ,
        "Training for staff"    = 'admin'
    ),
    policy.group = 'Policy group',
    policy.group = c(
        'High-salience policies' = "1",
        'Low-salience policies'  = "2"
    ),
    ## 
    att  = 'Policy attitude',
    att.congruent = 'Policy attitude (congruent)',
    ##
    mc1 = "Party endorsement is always a reliable indicator of what the policy is actually about",
    mc2 = "I can always confidently infer the content of a policy just by looking at which party endorses it",
    mc.prior = 'Manipulation check showed prior to policy questions'
)
LABELS.VARS   = LABELS[sapply(LABELS, function(x){length(x)==1})]
LABELS.VALUES = LABELS[sapply(LABELS, function(x){length(x)>1})]

