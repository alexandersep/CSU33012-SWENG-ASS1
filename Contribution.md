# CSU33012 - Software Engineering - Assignment 1
* Infix validator and calculator written in **Haskell** using *Hunit*, *QuckTest* 
  for unit testing and github workflow actions.

## Contributors & Contributions
* Alexander Sepelenco
    - Acted as github lead with organising, README, issues, pull requests, 
      github workflow, and setting up Haskell with stack. 
    - Set up github including github workflow with caching.
    - Set up Haskell unit testing Hunit, Quickcheck.
    - Implemented unit tests and functions for `isOperator`, `iOperand`,
      `operatorPrecedence`, `errorPrecedence`, `isOperatorLeftAssociative`, 
      `errorLeftAssociativity`, `removeSpaces`, `splitToList`.
    - Implemented the basic Input and Output when running programme. 
* Niall Sauvage
    - Implemented the following functions and their respective unit tests `infixValidator`, `popOperatorStackUpToParen`,
      `infixValidator'`, `countBrackets`, `infixToPostfix`, 
      `popOperatorStack`, `getFirstElem`, `evaluatePostfix`, `evaluatePostfix'`,
      `evaluateExpression`.
    - Implemented parsing of inputted string into postfix once it has been split.
    - Implemented evaluation of resulting postfix strings into single answer.
    - Worked on changes to Main.hs.

### Graph of commit logs
* Main Branch \
![Main branch](imgs/main.png) 
\newpage
* Calculator-Branch \
![Calc Branch 1](imgs/calculator-commit-1.png) \
![Calc Branch 2](imgs/calculator-commit-2.png)
* Expression-Validation Branch \
![Expression Validation 3](imgs/expression-validation-3.png) \
![Expression Validation 2](imgs/expression-validation-2.png) \
![Expression Validation 1](imgs/expression-validation-1.png)
* Postfix-Prefix Branch \
![Postfix](imgs/postfix-prefix.png)

#### Link To repo
[https://github.com/alexandersep/CSU33012-SWENG-ASS1](https://github.com/alexandersep/CSU33012-SWENG-ASS1) 
