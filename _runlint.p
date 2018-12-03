
/*------------------------------------------------------------------------
    File        : _runlint.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Dec 03 21:33:15 ALMT 2018
    Notes       :
  ----------------------------------------------------------------------*/
ROUTINE-LEVEL ON ERROR UNDO, THROW.

{adecomm/oeideservice.i}
/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER iFileName AS CHARACTER NO-UNDO.

DEFINE VARIABLE mLint AS HANDLE NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


     

/* ***************************  Main Block  *************************** */
iFileName = SUBSTRING(iFileName, 2).

PUBLISH "LintHandle" (OUTPUT mLint).

IF VALID-HANDLE(mLint) THEN
    DELETE PROCEDURE mLint.

RUN _lintview.r PERSISTENT SET mLint(iFileName).
