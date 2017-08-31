expr_creates_var <- function(correctName=NULL){
  e <- get("e", parent.frame())
  # TODO: Eventually make auto-detection of new variables an option.
  # Currently it can be set in customTests.R
  delta <- if(!customTests$AUTO_DETECT_NEWVAR){
    safeEval(e$expr, e)
  } else {
    e$delta
  }
  if(is.null(correctName)){
    results <- expectThat(length(delta) >= 1,
                          testthat::is_true(),
                          label=paste(deparse(e$expr), 
                                      "não cria uma variável."))  
  } else {
    results <- expectThat(correctName %in% names(delta), 
                          testthat::is_true(), 
                          label=paste(deparse(e$expr),
                                      "não cria uma variável nomeada",
                                      correctName))
  }
  if(results$passed){
    e$newVar <- e$val
    e$newVarName <- names(delta)[1]
    e$delta <- mergeLists(delta, e$delta)
  } else {
    e$delta <- list()
  }
  return(results$passed)
}

omnitest <- function(correctExpr=NULL, correctVal=NULL, strict=FALSE){
  e <- get("e", parent.frame())
  # Trivial case
  if(is.null(correctExpr) && is.null(correctVal))return(TRUE)
  # Testing for correct expression only
  if(!is.null(correctExpr) && is.null(correctVal)){
    passed <- expr_identical_to(correctExpr)
    if(!passed)e$delta <- list()
    return(passed)
  }
  # Testing for both correct expression and correct value
  # Value must be character or single number
  valGood <- NULL
  if(!is.null(correctVal)){
    if(is.character(e$val)){
      valResults <- expectThat(e$val,
                               is_equivalent_to(correctVal, label=correctVal),
                               label=(e$val))
      if(is(e, "dev") && !valResults$passed)swirl_out(valResults$message)
      valGood <- valResults$passed
      # valGood <- val_matches(correctVal)
    } else if(!is.na(e$val) && is.numeric(e$val) && length(e$val) == 1){
      cval <- try(as.numeric(correctVal), silent=TRUE)
      valResults <- expectThat(e$val, 
                               equals(cval, label=correctVal),
                               label=toString(e$val))
      if(is(e, "dev") && !valResults$passed)swirl_out(valResults$message)
      valGood <- valResults$passed
    }
  }
  exprGood <- ifelse(is.null(correctExpr), TRUE, expr_identical_to(correctExpr))
  if(valGood && exprGood){
    return(TRUE)
  } else if (valGood && !exprGood && !strict){
    swirl_out("Esta não é a função que estava esperando, mas funciona.")
    swirl_out("Executei a expressão correta no caso do resultado ser necessário em uma próxima questão.")
    eval(parse(text=correctExpr),globalenv())
    return(TRUE)
  } else {
    e$delta <- list()
    return(FALSE)
  }
}

notify <- function() {
  e <- get("e", parent.frame())
  if(e$val == "Não") return(TRUE)
  
  good <- FALSE
  while(!good) {
    # Get info
    name <- readline_clean("Qual é o teu nome completo? ")
    address <- readline_clean("Qual é o email da pessoa que você gostaria de informar? ")
    
    # Repeat back to them
    message("\nAs informações estão corretas?\n")
    message("Seu nome: ", name, "\n", "Enviar para: ", address)
    
    yn <- select.list(c("Sim", "Não"), graphics = FALSE)
    if(yn == "Sim") good <- TRUE
  }
  
  # Get course and lesson names
  course_name <- attr(e$les, "course_name")
  lesson_name <- attr(e$les, "lesson_name")
  
  subject <- paste(name, "completou com sucesso: ", course_name, "-", lesson_name)
  body = ""
  
  # Send email
  swirl:::email(address, subject, body)
  
  hrule()
  message("Tentei criar um novo email com a seguinte informação:\n")
  message("Para: ", address)
  message("Assunto: ", subject)
  message("Mensagem: <empty>")
  
  message("\nSe isso não funcionou, você pode enviar o email manualmente.")
  hrule()
  
  # Return TRUE to satisfy swirl and return to course menu
  TRUE
}

readline_clean <- function(prompt = "") {
  wrapped <- strwrap(prompt, width = getOption("width") - 2)
  mes <- stringr::str_c("| ", wrapped, collapse = "\n")
  message(mes)
  readline()
}

hrule <- function() {
  message("\n", paste0(rep("#", getOption("width") - 2), collapse = ""), "\n")
}
