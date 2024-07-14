/*
 *  The scanner definition for COOL.
 */
%x COMMENT
%x STRING
%x STRING_ESCAPE
/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

%}

/*
 * Define names for regular expressions here.
 */

DARROW          =>
CLASS           class
ELSE            else
FI              fi
IF              if
IN              in
INHERITS        inherits
LET             let
LOOP            loop
POOL            pool
THEN            then
WHILE           while
CASE            case
ESAC            esac
OF              of
NEW             new
ISVOID          isvoid
ASSIGN          <-
NOT             not
LE              <=
BOOL_TRUE       (?i:true)
BOOL_FALSE      (?i:false)

%%

     /*
      *  Nested comments
      */

"(*"                    { BEGIN(COMMENT); }
<COMMENT>"*)"           { BEGIN(INITIAL); }
<COMMENT>.
<COMMENT>\n             { curr_lineno++; }
<COMMENT><<EOF>>        { BEGIN(INITIAL); cool_yylval.error_msg = "EOF in comment"; return ERROR; }
"--".*                 {}

    /*
    *  The multiple-character operators.
    */
{DARROW} { return (DARROW); }
{ASSIGN} { return (ASSIGN); }
{LE} { return (LE); }

    /*
    * Keywords are case-insensitive except for the values true and false,
    * which must begin with a lower-case letter.
    */

{CLASS} { return (CLASS); }
{ELSE} { return (ELSE); }
{FI} { return (FI); }
{IF} { return (IF); }
{IN} { return (IN); }
{INHERITS} { return (INHERITS); }
{LET} { return (LET); }
{LOOP} { return (LOOP); }
{POOL} { return (POOL); }
{THEN} { return (THEN); }
{WHILE} { return (WHILE); }
{CASE} { return (CASE); }
{ESAC} { return (ESAC); }
{OF} { return (OF); }
{NEW} { return (NEW); }
{ISVOID} { return (ISVOID); }
{NOT} { return (NOT); }
{BOOL_TRUE} { cool_yylval.boolean = true; return (BOOL_CONST); }
{BOOL_FALSE} { cool_yylval.boolean = false; return (BOOL_CONST); }


    /* OBJECTID, TYPEID, INT_CONST */
[0-9]+                      {
                                cool_yylval.symbol = inttable.add_string(yytext, yyleng);
                                return INT_CONST;
                            }
[A-Z_][a-zA-Z0-9_]*         {
                                cool_yylval.symbol = idtable.add_string(yytext, yyleng);
                                return TYPEID;
                            }
[a-z][a-zA-Z0-9_]*          {
                                cool_yylval.symbol = stringtable.add_string(yytext, yyleng);
                                return OBJECTID;
                            }



    /*
      *  String constants (C syntax)
      *  Escape sequence \c is accepted for all characters c. Except for 
      *  \n \t \b \f, the result is c.
      *
      */
\"                          {
                                BEGIN(STRING); 
                                memset(string_buf, 0, sizeof(MAX_STR_CONST));
                                string_buf_ptr = string_buf;
                            }

    
<STRING>[^\"\\]*\"          {
                                /* does not include the last character */
                                memcpy(string_buf_ptr, yytext, yyleng - 1);
                                string_buf_ptr += yyleng - 1;
                                // cout << "string_buf is " << string_buf << endl;
                                // cout << "leng is " <<  string_buf_ptr - string_buf + 1 << endl;
                                cool_yylval.symbol = stringtable.add_string(string_buf, string_buf_ptr - string_buf + 1);
                                BEGIN(INITIAL);
                                return STR_CONST;
                            }

<STRING>[^\"\\]*\\          {
                                // does not include the last character escape
                                memcpy(string_buf_ptr, yytext, yyleng - 1);
                                string_buf_ptr += yyleng - 1;
                                BEGIN(STRING_ESCAPE);
                            }
<STRING_ESCAPE>n            {
                                // cout << "escape \\n !" << endl;
                                *(string_buf_ptr++) = '\n';
                                BEGIN(STRING);
                            }

<STRING_ESCAPE>b            {
                                *(string_buf_ptr++) = '\b';
                                BEGIN(STRING);
                            }

<STRING_ESCAPE>t                    {
                                        *(string_buf_ptr++) = '\t';
                                        BEGIN(STRING);
                                    }

<STRING_ESCAPE>f                    {
                                        *(string_buf_ptr++) = '\f';
                                        BEGIN(STRING);
                                    }
<STRING_ESCAPE>.                    {
                                        *(string_buf_ptr++) = yytext[0];
                                        BEGIN(STRING);
                                    }                                    
<STRING_ESCAPE>\n                   {
                                        *(string_buf_ptr++) = '\n';
                                        ++curr_lineno;
                                        BEGIN(STRING);
                                    }
<STRING_ESCAPE>0 {
    cool_yylval.error_msg = "String contains null character";
    BEGIN(STRING);
    return (ERROR);
}

<STRING>[^\"\\]*$ {
    // push first
    // contains the last character for yytext does not include \n
    //setup error later
    cool_yylval.error_msg = "Unterminated string constant";
    BEGIN(INITIAL);
    ++curr_lineno;
    return (ERROR);
}

<STRING_ESCAPE><<EOF>> {
    cool_yylval.error_msg = "EOF in string constant";
    BEGIN(INITIAL);
    return (ERROR);
}

<STRING><<EOF>> {
    cool_yylval.error_msg = "EOF in string constant";
    BEGIN(INITIAL);
    return (ERROR);
}

    /* illegal characters */
[\[\]\'>]                   {
                                cool_yylval.error_msg = yytext;
                                return (ERROR);
                            }
    /* blank */
[ \t\f\r\v]                 {}

\n                          { curr_lineno++; }
    /* legal characters*/
.                           {
                                return yytext[0];
                            }

%%
