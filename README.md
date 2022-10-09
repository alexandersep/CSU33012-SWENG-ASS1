# CSU33012-SWENG-ASS1
Haskell Implementation of Unit testing a basic calculator

## Instructions for Setup
1. Clone repository with `git clone https://github.com/alexandersep/CSU33012-SWENG-ASS1.git`
2. Enter CSU33012-SWENG-ASS1/ with `cd CSU33012-SWENG-ASS1/`
3. Initialise the stack with `stack init` 
4. Compile test folder with `stack test` 
5. Give yourself executable permissions for the `run.sh` script with `chmod +x run.sh`
6. Run Haskell programme with `./run.sh`.

## Calculator Usage & Examples 
* Inputs include brackets, negative operands, operands, operators "+-/\*^" and unary operators "+-"
```bash
./run.sh
Haskell Infix Calculator, Note: Input without Quotes e.g. 2 + 3 instead of "2 + 3"
Please input an infix expression: -3 *-2 / 5 + 1 - ((1 - 0) - ( -1 --2))*4
The answer is: Just 2.2

./run.sh
Haskell Infix Calculator, Note: Input without Quotes e.g. 2 + 3 instead of "2 + 3"
Please input an infix expression: --- -+ + - + -- 3 *+-+-2 / -- 5 + 1 - (  (- 1 - 0) - ( -1 --2)  )*  4
The answer is: Just 7.8

./run.sh
Haskell Infix Calculator, Note: Input without Quotes e.g. 2 + 3 instead of "2 + 3"
Please input an infix expression: 3 * (-3 * 3) + 0 + 0 - 1 - 1 - 1 + ( +0 + 0) *--4
The answer is: Just (-30.0)
```

### Report with Contributions
* The report of contributors and who contributed what can be found 
  in the file `Contribution.md` as text or `Contribution.pdf` as a pdf.

#### Contributors
Alexander Sepelenco, sepelena@tcd.ie \
Niall Sauvage, sauvagen@tcd.ie
