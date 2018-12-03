&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME mLintView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS mLintView 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
/* ********************  Preprocessor Definitions  ******************** */
&SCOPED-DEFINE VIEW_ID      "com.openedge.pdt.text.views.OERuntimeView":U
&SCOPED-DEFINE SECONDARY_ID "ProlintView":U
&SCOPED-DEFINE VIEW_TITLE   "Prolint":U

/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER iStartFile AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE mViewHwnd     AS INT64     NO-UNDO.
DEFINE VARIABLE mSortField    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSortAsc      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mStartTime    AS INT64     NO-UNDO.
DEFINE VARIABLE mElapsedTime  AS DECIMAL   NO-UNDO.

DEFINE TEMP-TABLE ttFile NO-UNDO
    FIELD Unit        AS CHARACTER
    FIELD NumWarnings AS INT64.

DEFINE TEMP-TABLE ttLint NO-UNDO
    FIELD Severity AS INT64     LABEL "Severity":T FORMAT "9":U
    FIELD LineNum  AS INT64     LABEL "Line":T FORMAT ">>>>9":U
    FIELD RuleID   AS CHARACTER LABEL "Rule":T FORMAT "x(15)":U
    FIELD Unit     AS CHARACTER LABEL "Unit":T FORMAT "x(64)":U
    FIELD SourceID AS CHARACTER LABEL "Source":T FORMAT "x(64)":U
    FIELD Comment  AS CHARACTER LABEL "Description":T FORMAT "x(150)":U   
    INDEX iMain AS PRIMARY Unit SourceID LineNum.

DEFINE STREAM sLog.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME mBrwResults

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttLint

/* Definitions for BROWSE mBrwResults                                   */
&Scoped-define FIELDS-IN-QUERY-mBrwResults ttLint.Severity ttLint.LineNum ttLint.RuleID ttLint.Unit ttLint.SourceID ttLint.Comment   
&Scoped-define ENABLED-FIELDS-IN-QUERY-mBrwResults   
&Scoped-define QUERY-STRING-mBrwResults FOR EACH ttLint NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-mBrwResults OPEN QUERY mBrwResults FOR EACH ttLint NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-mBrwResults ttLint
&Scoped-define FIRST-TABLE-IN-QUERY-mBrwResults ttLint


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-mBrwResults}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 mBtnLint mBtnSave ~
mBrwResults 
&Scoped-Define DISPLAYED-OBJECTS mWarnings mCurrentFile mProgress 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR mLintView AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON mBtnLint DEFAULT 
     IMAGE-UP FILE "prolint/images/prolint.gif":U
     IMAGE-DOWN FILE "prolint/images/prolint.gif":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON mBtnSave 
     IMAGE-UP FILE "prolint/images/savelog.gif":U
     IMAGE-DOWN FILE "prolint/images/savelog.gif":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE mCurrentFile AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 64 BY .71 NO-UNDO.

DEFINE VARIABLE mProgress AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 184 BY .71 NO-UNDO.

DEFINE VARIABLE mWarnings AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .71 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30 BY 1.19.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 66 BY 1.19.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 186 BY 1.19.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY mBrwResults FOR 
      ttLint SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE mBrwResults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS mBrwResults mLintView _FREEFORM
  QUERY mBrwResults NO-LOCK DISPLAY
      ttLint.Severity
      ttLint.LineNum
      ttLint.RuleID
      ttLint.Unit
      ttLint.SourceID
      ttLint.Comment
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 282 BY 5.86 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     mBtnLint AT ROW 1.24 COL 4 WIDGET-ID 2
     mBtnSave AT ROW 1.24 COL 10 WIDGET-ID 6
     mBrwResults AT ROW 2.43 COL 3 WIDGET-ID 200
     mWarnings AT ROW 8.62 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     mCurrentFile AT ROW 8.62 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     mProgress AT ROW 8.62 COL 98 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     RECT-1 AT ROW 8.38 COL 3 WIDGET-ID 14
     RECT-2 AT ROW 8.38 COL 33 WIDGET-ID 16
     RECT-3 AT ROW 8.38 COL 99 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 285.6 BY 8.67 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW mLintView ASSIGN
         HIDDEN             = YES
         TITLE              = "prolint"
         HEIGHT             = 8.95
         WIDTH              = 286.4
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 299.6
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 299.6
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB mLintView 
/* ************************* Included-Libraries *********************** */

{prolint/core/dlc-version.i}
{adecomm/oeideservice.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW mLintView
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* BROWSE-TAB mBrwResults mBtnSave fMain */
/* SETTINGS FOR FILL-IN mCurrentFile IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       mCurrentFile:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN mProgress IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       mProgress:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN mWarnings IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       mWarnings:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(mLintView)
THEN mLintView:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE mBrwResults
/* Query rebuild information for BROWSE mBrwResults
     _TblList          = "ttLint"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"ttLint.Severity" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"ttLint.LineNum" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"ttLint.RuleID" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"ttLint.Unit" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"ttLint.SourceID" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"ttLint.Comment" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE mBrwResults */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME mLintView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mLintView mLintView
ON END-ERROR OF mLintView /* prolint */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mLintView mLintView
ON WINDOW-CLOSE OF mLintView /* prolint */
DO:
    /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME mBrwResults
&Scoped-define SELF-NAME mBrwResults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mBrwResults mLintView
ON START-SEARCH OF mBrwResults IN FRAME fMain
DO:
    DEFINE VARIABLE hSortColumn  AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE hQueryHandle AS HANDLE        NO-UNDO.

    hSortColumn = BROWSE mBrwResults:CURRENT-COLUMN.
    IF hSortColumn:SORT-ASCENDING = TRUE THEN
        mSortAsc = FALSE.
    ELSE
        mSortAsc = TRUE.
    mBrwResults:CLEAR-SORT-ARROWS().
    ASSIGN
        hSortColumn:SORT-ASCENDING = mSortAsc
        mSortField = hSortColumn:NAME.       

    RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mBtnLint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mBtnLint mLintView
ON CHOOSE OF mBtnLint IN FRAME fMain
DO:
    FIND FIRST ttFile WHERE ttFile.Unit = mCurrentFile NO-ERROR.
    IF NOT AVAILABLE(ttFile) THEN
    DO:
        CREATE ttFile.
        ttFile.Unit = mCurrentFile.
    END. 
    FOR EACH ttLint WHERE ttLint.Unit = mCurrentFile:
        DELETE ttLint.
    END.
    ttFile.NumWarnings = 0.    
    RUN prolint/core/prolint.p (mCurrentFile, ?, "", FALSE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mBtnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mBtnSave mLintView
ON CHOOSE OF mBtnSave IN FRAME fMain
DO:
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK mLintView 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE
DO:         
    RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:    
    
    IF VALID-HANDLE(hOEIDEService) THEN 
    DO:  
        ShowView ({&VIEW_ID}, {&SECONDARY_ID}, {&VIEW_ACTIVATE}).
        setViewTitle({&VIEW_ID}, {&SECONDARY_ID}, {&VIEW_TITLE}).
        RUN getViewHwnd IN hOEIDEService({&VIEW_ID}, {&SECONDARY_ID}, OUTPUT mViewHwnd) NO-ERROR. 
        DELETE OBJECT mLintView NO-ERROR.
        CREATE WINDOW mLintView ASSIGN
            IDE-WINDOW-TYPE    = 0  /* embedded mode */
            IDE-PARENT-HWND    = mViewHwnd
            HIDDEN             = TRUE
            TITLE              = {&VIEW_TITLE}
            HEIGHT             = 13
            WIDTH              = 140
            MAX-HEIGHT         = 320
            MAX-WIDTH          = 320
            VIRTUAL-HEIGHT     = 320
            VIRTUAL-WIDTH      = 320
            RESIZE             = TRUE
            SCROLL-BARS        = FALSE
            STATUS-AREA        = FALSE
            BGCOLOR            = ?
            FGCOLOR            = ?
            KEEP-FRAME-Z-ORDER = TRUE
            THREE-D            = TRUE
            MESSAGE-AREA       = FALSE
            SENSITIVE          = TRUE.
        mBrwResults:ALLOW-COLUMN-SEARCHING = TRUE.        
        ON END-ERROR OF mLintView OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
        DO:
            IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
        END.
      
        ON WINDOW-CLOSE OF mLintView
        DO:            
            APPLY "close":U TO THIS-PROCEDURE.
            RETURN NO-APPLY.
        END.

        ON WINDOW-RESIZED OF mLintView 
        DO:
            RUN ResizeWindow.
        END.
        
        /* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.
              */
        ASSIGN 
            CURRENT-WINDOW = {&WINDOW-NAME}
            THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
        ASSIGN 
            FRAME {&FRAME-NAME}:SCROLLABLE = FALSE.
        setEmbeddedWindow({&VIEW_ID}, {&SECONDARY_ID}, {&WINDOW-NAME}:HANDLE).        
    END.
    
    /* set private-data to a unique value, so Prolint can determine if this window is already running.
     if prolint can't find a window with this private data, it will lauch one. */    
    ASSIGN
        mSortField = "Severity"
        mCurrentFile = iStartFile
        mLintView:PRIVATE-DATA = "prolint_outputhandler_logwin.w":U.
    
    RUN enable_UI.

    SUBSCRIBE TO "LintHandle" ANYWHERE.
    SUBSCRIBE TO "oeide_event" ANYWHERE.
    SUBSCRIBE TO "Prolint_outputhandler_oea" ANYWHERE.
    SUBSCRIBE TO "Prolint_InitializeResults" ANYWHERE.
    SUBSCRIBE TO "Prolint_AddResult" ANYWHERE.
    SUBSCRIBE TO "Prolint_FinalizeResults" ANYWHERE.
    SUBSCRIBE TO "Prolint_Status_action" ANYWHERE.
    SUBSCRIBE TO "Prolint_Status_FileStart" ANYWHERE.
    SUBSCRIBE TO "Prolint_Status_Profile" ANYWHERE.
    SUBSCRIBE TO "Prolint_Status_Progress" ANYWHERE.
    SUBSCRIBE TO "Prolint_Status_StartTimer" ANYWHERE.
    SUBSCRIBE TO "Prolint_Status_StopTimer" ANYWHERE.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DbgPrt mLintView 
PROCEDURE DbgPrt PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iMessage AS CHARACTER NO-UNDO.
    OUTPUT STREAM sLog TO VALUE("oeide.log") APPEND.
    PUT STREAM sLog UNFORMATTED NOW CHR(9) iMessage SKIP.
    OUTPUT STREAM sLog CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI mLintView  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(mLintView)
  THEN DELETE WIDGET mLintView.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI mLintView  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY mWarnings mCurrentFile mProgress 
      WITH FRAME fMain IN WINDOW mLintView.
  ENABLE RECT-1 RECT-2 RECT-3 mBtnLint mBtnSave mBrwResults 
      WITH FRAME fMain IN WINDOW mLintView.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW mLintView.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LintHandle mLintView 
PROCEDURE LintHandle :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oHandle AS HANDLE NO-UNDO.
    
    oHandle = THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oeide_event mLintView 
PROCEDURE oeide_event :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iEventName   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iProjectName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iProgramName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iEventData   AS CHARACTER NO-UNDO.
    
    IF NOT VALID-HANDLE(mLintView) THEN
    DO:
        DELETE PROCEDURE THIS-PROCEDURE.
        RETURN.
    END.
    
    IF iEventName = "get-focus" THEN 
    DO:
        mCurrentFile = iProgramName.
        RUN UpdateStatus IN THIS-PROCEDURE.
        RUN OpenQuery IN THIS-PROCEDURE.
    END.        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery mLintView 
PROCEDURE OpenQuery PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hQueryHandle AS HANDLE        NO-UNDO.

    hQueryHandle = BROWSE mBrwResults:QUERY.
    hQueryHandle:QUERY-CLOSE().
    hQueryHandle:QUERY-PREPARE(SUBSTITUTE("FOR EACH ttLint WHERE ttLint.Unit = &1 NO-LOCK BY &2&3", QUOTER(mCurrentFile), mSortField, IF mSortAsc THEN "" ELSE " DESC")).
    hQueryHandle:QUERY-OPEN().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prolint_AddResult mLintView 
PROCEDURE Prolint_AddResult :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pCompilationUnit  AS CHARACTER    NO-UNDO.  /* the sourcefile we're parsing          */
    DEFINE INPUT PARAMETER pSourcefile       AS CHARACTER    NO-UNDO.  /* may be an includefile                 */
    DEFINE INPUT PARAMETER pLineNumber       AS INTEGER NO-UNDO.  /* line number in pSourceFile            */
    DEFINE INPUT PARAMETER pDescription      AS CHARACTER    NO-UNDO.  /* human-readable hint                   */
    DEFINE INPUT PARAMETER pRuleID           AS CHARACTER    NO-UNDO.  /* defines rule-program and maps to help */
    DEFINE INPUT PARAMETER pSeverity         AS INTEGER NO-UNDO.  /* importance of this rule, scale 0-9    */

    CREATE ttLint.
    ASSIGN 
        ttLint.Unit        = pCompilationUnit
        ttLint.SourceId    = pSourcefile
        ttLint.LineNum     = pLineNumber
        ttLint.Comment     = pDescription
        ttLint.RuleID      = pRuleID
        ttLint.Severity    = pSeverity.
    FIND FIRST ttFile WHERE ttFile.Unit = pCompilationUnit NO-ERROR.
    IF NOT AVAILABLE(ttFile) THEN
    DO:
        CREATE ttFile.
        ttFile.Unit = pCompilationUnit.
    END. 
    ttFile.NumWarnings = ttFile.NumWarnings + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prolint_FinalizeResults mLintView 
PROCEDURE Prolint_FinalizeResults :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    FIND FIRST ttFile WHERE ttFile.Unit = mCurrentFile NO-ERROR.
    IF NOT AVAILABLE(ttFile) THEN
    DO:
        CREATE ttFile.
        ttFile.Unit = mCurrentFile.
    END.
    
    mElapsedTime = ROUND(mElapsedTime / 1000, 2).
    mProgress = SUBSTITUTE("Total time: &1 sec":T, mElapsedTime).    
    mWarnings = STRING(ttFile.NumWarnings) + " warnings":U.
    RUN UpdateStatus IN THIS-PROCEDURE.    
    
    RUN OpenQuery IN THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prolint_InitializeResults mLintView 
PROCEDURE Prolint_InitializeResults :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pClearOutput AS LOGICAL NO-UNDO.

    mProgress = "Working...".
    mWarnings = "0 warnings":U.
    RUN UpdateStatus IN THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prolint_outputhandler_oea mLintView 
PROCEDURE Prolint_outputhandler_oea :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oAlive AS LOGICAL NO-UNDO.
    
    oAlive = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prolint_Status_action mLintView 
PROCEDURE Prolint_Status_action :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pAction AS CHAR NO-UNDO.
    
    FIND FIRST ttFile WHERE ttFile.Unit = mCurrentFile NO-ERROR.
    IF NOT AVAILABLE(ttFile) THEN
    DO:
        CREATE ttFile.
        ttFile.Unit = mCurrentFile.
    END. 
    
    mProgress = pAction.    
    mWarnings = STRING(ttFile.NumWarnings) + " warnings":U.
    RUN UpdateStatus IN THIS-PROCEDURE.
    
    PROCESS EVENTS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prolint_Status_FileStart mLintView 
PROCEDURE Prolint_Status_FileStart :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pSourceFile AS CHAR NO-UNDO.

    mCurrentFile = pSourceFile. 
    RUN UpdateStatus IN THIS-PROCEDURE.        
    
    PROCESS EVENTS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prolint_Status_Profile mLintView 
PROCEDURE Prolint_Status_Profile :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pProfile AS CHAR NO-UNDO.                                                
                                                 
    PROCESS EVENTS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prolint_Status_Progress mLintView 
PROCEDURE Prolint_Status_Progress :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pProgress AS CHAR NO-UNDO.
    
    mProgress = pProgress.
    RUN UpdateStatus IN THIS-PROCEDURE.
    PROCESS EVENTS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prolint_Status_StartTimer mLintView 
PROCEDURE Prolint_Status_StartTimer :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    mStartTime = ETIME(FALSE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prolint_Status_StopTimer mLintView 
PROCEDURE Prolint_Status_StopTimer :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    mElapsedTime = mElapsedTime + ETIME(FALSE) - mStartTime.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ResizeWindow mLintView 
PROCEDURE ResizeWindow PRIVATE :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
 /*   DEFINE VARIABLE hFrame AS HANDLE NO-UNDO.
    hFrame = FRAME {&FRAME-NAME}:HANDLE.
   
    DO WITH FRAME {&FRAME-NAME}:
       
        ASSIGN 
            hFrame:HEIGHT = {&WINDOW-NAME}:HEIGHT - 1  /* room for the statusbar */ 
            hFrame:WIDTH  = {&WINDOW-NAME}:WIDTH.
    
        ASSIGN 
            mBrwResults:HEIGHT-PIXELS = hFrame:HEIGHT-PIXELS - mBrwResults:Y
            mBrwResults:WIDTH-PIXELS  = hFrame:WIDTH-PIXELS.       
       
    END.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateStatus mLintView 
PROCEDURE UpdateStatus PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    FIND FIRST ttFile WHERE ttFile.Unit = mCurrentFile NO-ERROR.
    IF AVAILABLE(ttFile) THEN
        mWarnings = STRING(ttFile.NumWarnings) + " warnings":U.
    ELSE
        mWarnings = "0 warnings":U.    
    
    IF VALID-HANDLE(FRAME fMain:HANDLE) THEN
        DISPLAY mWarnings mCurrentFile mProgress WITH FRAME fMain.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

