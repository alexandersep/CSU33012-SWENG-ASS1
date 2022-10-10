# CSU33012 - Software Engineering - Assignment 1
* Infix validator and calculator written in **Haskell** using *Hunit*, *QuckCheck* 
  for unit testing and github workflow actions.

## Contributors & Contributions
* Alexander Sepelenco
    - Acted as github lead with organising, README, issues, pull requests, 
      github workflow, and setting up Haskell with stack.
    - Set up Haskell unit testing: Hunit, and Quickcheck.
    - Set up github including github workflow with caching.
    - Implemented the following functions and their respective unit tests
      `isOperator`, `iOperand`, `operatorPrecedence`, `errorPrecedence`,
      `isOperatorLeftAssociative`, `errorLeftAssociativity`, `removeSpaces`,
      `splitToList`, `addZeroStringUnaryHeadPositiveOrNegative`,
      `combineUnaryOperators`, `removeUnaryHeadPositive`, `removePlusNum`,
      `combineNum`
    - Implemented the basic Input and Output when running programme. 
    - Implemented Unary parsing, `-` and `+` and ensured it worked effectively
      with Niall's evaluator, and validators.
* Niall Sauvage
    - Implemented the following functions and their respective unit tests 
      `infixValidator`, `popOperatorStackUpToParen`, `infixValidator'`, 
      `countBrackets`, `infixToPostfix`, `popOperatorStack`, `getFirstElem`, 
      `evaluatePostfix`, `evaluatePostfix'`, `evaluateExpression`.
    - Implemented parsing of inputted string into postfix once it has been split.
    - Implemented evaluation of resulting postfix strings into a single answer.
    - Worked on changes to Main.hs.

### Graph/commit logs
* Graph of commit logs \
    * Graph Part 3 \
    ![Graph 3](imgs/graph3.png)
    * Graph Part 2 \
    ![Graph 2](imgs/graph2.png)
    * Graph Part 1 \
    ![Graph 1](imgs/graph1.png)
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
