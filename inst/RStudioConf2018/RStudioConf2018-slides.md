---
title: "Adaptive Feedback for LearnR"
author: "Daniel Kaplan"
date: "February 2, 2018"
output: 
  ioslides_presentation: 
    keep_md: yes
    logo: ~/KaplanFiles/Explore/checkr/inst/RStudioConf2018/mosaic-square.png
    self_contained: no
    smart: no
    widescreen: yes
    df_print: kable
    theme: yeti
runtime: shiny_prerendered
---



--------------

> Write the one-line version of "Hello, World!" in R. Your code should cause the message `Hello, World!` to appear. 




<div class="tutorial-exercise-support" data-label="hello-world-check" data-caption="Code" data-completion="1" data-diagnostics="1" data-startover="1" data-lines="0">

```text
#hello_check(USER_CODE)
```

</div>

<div class="tutorial-exercise" data-label="hello-world" data-caption="Code" data-completion="1" data-diagnostics="1" data-startover="1" data-lines="0">

```text
"Hello, World!"
```

<script type="application/json" data-opts-chunk="1">{"fig.width":7.5,"fig.height":4.5,"fig.retina":1,"fig.align":"default","fig.keep":"high","fig.show":"asis","warning":true,"error":false,"message":true,"exercise.df_print":"kable","exercise.checker":["function (label = NULL, user_code = NULL, solution_code = NULL, ","    check_code = NULL, envir_result = NULL, evaluate_result = NULL, ","    ..., debug = FALSE) ","{","    if (debug) {","        save_file_name <- sprintf(\"~/Downloads/CheckR/chunk-%s.rds\", ","            label)","        saveRDS(list(label = label, user_code = user_code, solution_code = solution_code, ","            check_code = check_code, envir = envir_result, evaluate_result = evaluate_result), ","            file = save_file_name)","    }","    if (is.null(envir_result)) {","        res <- pre_check(user_code, solution_code)","        if (!res$correct) {","            return(list(correct = FALSE, type = \"error\", location = \"prepend\", ","                message = res$message))","        }","        else {","            return(TRUE)","        }","    }","    if (!is.null(envir_result)) {","        res <- parse_check(user_code)","        if (!res$correct) {","            return(list(correct = FALSE, type = \"error\", location = \"prepend\", ","                message = res$message))","        }","    }","    parsed_check_code <- parse(text = check_code)","    res <- eval(parsed_check_code, envir = list(USER_CODE = user_code))","    if (res$action == \"ok\" && res$message == \"\") ","        res$message = \"Good\"","    feedback_type <- switch(res$action, pass = \"success\", fail = \"error\", ","        ok = \"warning\")","    final <- list(correct = (res$action == \"pass\"), message = res$message, ","        type = feedback_type, location = \"prepend\")","    final","}"]}</script></div>

## The man behind the curtain

Ordinarily, you would not show students the `checkr` statements implementing this behavior. But our purpose here is to introduce `checkr`, So here are the statements for the above exercise.


```
 [1] code <- for_checkr(USER_CODE)
 [2] res <- misconception(code, line_where(code, passif(is.null(F))), 
 [3]     message = paste("Just typing the string doesn't do it. Note that the output", 
 [4]         "has quotes and is formatted as an output, not a simple message."))
 [5] res <- misconception(res, line_where(res, passif(F == print)), 
 [6]     message = paste("Using print() isn't right.", "True, that's the way character strings are displayed,", 
 [7]         "but the format of the display is as a quoted output and", 
 [8]         "not just a simple message."))
 [9] res <- line_where(res, insist(F == cat, "Try the cat() function."))
[10] check(arg_number(res, 1), passif(V == "Hello, World!", "Good job!"), 
[11]     failif(tolower(V) == "hello, world!", "Get the capitalization right!"), 
[12]     failif(TRUE, "The string should be 'Hello, World!', not '{{V}}'."))
```

Breaking this down, line by line:

- [1] accepts the user submission from `learnr`. This is always called `USER_CODE`. The function `for_check()` does some pre-processing of the user submission to turn it into evaluated code and format it for use in later `checkr` functions.
- [2] tests for a particular kind of mistaken answer. The `misconception()` function will generate a `checkr` fail message, if the pattern identified in the second argument passes. That pattern, `line_where(code, is.null(F))`, means, "scan the code looking for a line where no function is being used." This will captures a line that contains only a character string.
- [5] tests for another specific misconception, that the user invokes `print()` on the string. 
- [9] looks whether the function invoked by the user is `cat()`. If not, the check fails. (Note that [5] already ruled out that `print()` was being invoked.)
- [10] checks the argument to the `cat()` function. (We know it's `cat()`, because [9] has established this.) If that argument is exactly `"Hello, World!" the submission passes. Otherwise, we check for a particular error involving capitalization and, if that's not the case, generate a message to tell the student what's wrong.

Depending on the submission, any of the checks on lines 2, 5, 9, and 10 might fail. If a check fails, later checks that use the previous result will short circuit to a failed check. For instance, if the check on line [2] fails, the remaining checks won't be performed in detail: they will just pass along the failed result from line [2].

An instructor with a different pedagogical approach might prefer to structure the checking in an entirely different way. For instance, here are `checkr` statements that simply tell the user whether or not the submission did what was requested:


```
## [1] code <- for_checkr(USER_CODE)
## [2] line_binding(code, cat("Hello, World!"), passif(TRUE, "That's right."), 
## [3]     fail = "No. Try aain.")
```

<div class="tutorial-exercise" data-label="hello-world-strict" data-caption="Code" data-completion="1" data-diagnostics="1" data-startover="1" data-lines="0">

```text
"Hello, World!"
```

<script type="application/json" data-opts-chunk="1">{"fig.width":7.5,"fig.height":4.5,"fig.retina":1,"fig.align":"default","fig.keep":"high","fig.show":"asis","warning":true,"error":false,"message":true,"exercise.df_print":"kable","exercise.checker":["function (label = NULL, user_code = NULL, solution_code = NULL, ","    check_code = NULL, envir_result = NULL, evaluate_result = NULL, ","    ..., debug = FALSE) ","{","    if (debug) {","        save_file_name <- sprintf(\"~/Downloads/CheckR/chunk-%s.rds\", ","            label)","        saveRDS(list(label = label, user_code = user_code, solution_code = solution_code, ","            check_code = check_code, envir = envir_result, evaluate_result = evaluate_result), ","            file = save_file_name)","    }","    if (is.null(envir_result)) {","        res <- pre_check(user_code, solution_code)","        if (!res$correct) {","            return(list(correct = FALSE, type = \"error\", location = \"prepend\", ","                message = res$message))","        }","        else {","            return(TRUE)","        }","    }","    if (!is.null(envir_result)) {","        res <- parse_check(user_code)","        if (!res$correct) {","            return(list(correct = FALSE, type = \"error\", location = \"prepend\", ","                message = res$message))","        }","    }","    parsed_check_code <- parse(text = check_code)","    res <- eval(parsed_check_code, envir = list(USER_CODE = user_code))","    if (res$action == \"ok\" && res$message == \"\") ","        res$message = \"Good\"","    feedback_type <- switch(res$action, pass = \"success\", fail = \"error\", ","        ok = \"warning\")","    final <- list(correct = (res$action == \"pass\"), message = res$message, ","        type = feedback_type, location = \"prepend\")","    final","}"]}</script></div>

<div class="tutorial-exercise-support" data-label="hello-world-strict-check" data-caption="Code" data-completion="1" data-diagnostics="1" data-startover="1" data-lines="0">

```text
hello_fun_strict(USER_CODE)
```

</div>

## R Markdown

This is an R Markdown presentation. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document.

## Slide with Bullets

- Bullet 1
- Bullet 2
- Bullet 3

## Slide with R Output


```r
summary(cars)
```

```
##      speed           dist       
##  Min.   : 4.0   Min.   :  2.00  
##  1st Qu.:12.0   1st Qu.: 26.00  
##  Median :15.0   Median : 36.00  
##  Mean   :15.4   Mean   : 42.98  
##  3rd Qu.:19.0   3rd Qu.: 56.00  
##  Max.   :25.0   Max.   :120.00
```

## Slide with Plot

![](RStudioConf2018-slides_files/figure-html/pressure-1.png)<!-- -->

<!--html_preserve-->
<script type="application/shiny-prerendered" data-context="server-start">
library(learnr)
library(checkr)
knitr::opts_chunk$set(echo = FALSE)
tutorial_options(exercise.checker = checkr::check_for_learnr)
# tutorial_options(exercise.checker = function(...) cat("Bogus checker\n"))

hello_check <- function(USER_CODE) {
  code <- for_checkr(USER_CODE)
  res <- misconception(code, line_where(code, passif(is.null(F))), 
                       message = paste(
                         "Just typing the string doesn't do it. Note that the output", 
                         "has quotes and is formatted as an output, not a simple message."))
  res <- misconception(res, line_where(res, passif(F == print)), 
                       message = paste(
                         "Using print() isn't right.",
                         "True, that's the way character strings are displayed,", 
                         "but the format of the display is as a quoted output and",
                         "not just a simple message."))
  res <- line_where(res, insist(F == cat, "Try the cat() function."))
  check(arg_number(res, 1), passif(V == "Hello, World!", "Good job!"),
        failif(tolower(V) == "hello, world!", "Get the capitalization right!"),
        failif(TRUE, "The string should be 'Hello, World!', not '{{V}}'.")) 
}

two_plus_two_check <- function(USER_CODE) {
  res <- for_checkr(USER_CODE)
  res <- line_where(res, insist(V == 4, "The result should be 4, not {{V}}."))
  res <- line_where(res, 
                    insist(F == `+`, paste(
                      "Think about what function corresponds to 'addition'.", 
                      "It isn't {{F}}.")))
  arg_number(res, 1, failif(V != 2, "The first argument should be 2, not {{EX}}."),
             passif(TRUE, "Yes, that's it!"))
}

hello_fun_strict <- function(USER_CODE) {
  code <- for_checkr(USER_CODE)
  line_binding(code, 
               cat("Hello, World!"),  # a pattern with no flexibility.
               passif(TRUE, "That's right."),
               fail = "No. Try aain.")
}
</script>
<!--/html_preserve-->
<!--html_preserve-->
<script type="application/shiny-prerendered" data-context="server">
learnr:::register_http_handlers(session, metadata = NULL)
</script>
<!--/html_preserve-->
<!--html_preserve-->
<script type="application/shiny-prerendered" data-context="server">
`tutorial-exercise-hello-world-result` <- learnr:::setup_exercise_handler(reactive(req(input$`tutorial-exercise-hello-world-code-editor`)), session)
output$`tutorial-exercise-hello-world-output` <- renderUI({
  `tutorial-exercise-hello-world-result`()
})
</script>
<!--/html_preserve-->
<!--html_preserve-->
<script type="application/shiny-prerendered" data-context="server">
`tutorial-exercise-hello-world-strict-result` <- learnr:::setup_exercise_handler(reactive(req(input$`tutorial-exercise-hello-world-strict-code-editor`)), session)
output$`tutorial-exercise-hello-world-strict-output` <- renderUI({
  `tutorial-exercise-hello-world-strict-result`()
})
</script>
<!--/html_preserve-->
