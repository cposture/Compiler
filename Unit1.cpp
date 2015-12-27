/*** PL0 COMPILER WITH CODE GENERATION ***/
//---------------------------------------------------------------------------
//_CRT_SECURE_NO_WARNINGS : Visual studio macro 
#define _CRT_SECURE_NO_WARNINGS
//#define DEGUG

#include <cstdio>
#include <cstdlib>
#include <string>
#include <iostream>
#include <cstring>
#include <cmath>
#include "Unit1.h"

using namespace std;
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
#define  AL  10  /* LENGTH OF IDENTIFIERS */
#define  NORW  21  /* # OF RESERVED WORDS */
#define  TXMAX  100  /* LENGTH OF IDENTIFIER TABLE */
#define  NMAX     14  /* MAX NUMBER OF DEGITS IN NUMBERS */
#define  AMAX   2047  /* MAXIMUM ADDRESS */
#define  LEVMAX    3  /* MAX DEPTH OF BLOCK NESTING */
#define  CXMAX  200  /* SIZE OF CODE ARRAY */

typedef enum  {
	NUL, IDENT, NUMBER, PLUS, MINUS, TIMES,
	SLASH, ODDSYM, EQL, NEQ, LSS, LEQ, GTR, GEQ,
	LPAREN, RPAREN, COMMA, SEMICOLON, PERIOD,
	BECOMES, BEGINSYM, ENDSYM, IFSYM, THENSYM,
	WHILESYM, WRITESYM, READSYM, DOSYM, CALLSYM,
	CONSTSYM, VARSYM, PROCSYM, PROGSYM, TIMESEQ, ELSESYM, DIVEQ, FORSYM, STEPSYM, UNTILSYM, AND, OR, NOT,CHARSYM,DOUBLESYM,DOUBLENUM,ARRAYSYM,LEFTSQ,RIGHTSQ,SYMBOLNUM //SYMBOLNUM :标识符总数量
} SYMBOL;

const char *SYMOUT[] = { "NUL", "IDENT", "NUMBER", "PLUS", "MINUS", "TIMES",
	"SLASH", "ODDSYM", "EQL", "NEQ", "LSS", "LEQ", "GTR", "GEQ",
	"LPAREN", "RPAREN", "COMMA", "SEMICOLON", "PERIOD",
	"BECOMES", "BEGINSYM", "ENDSYM", "IFSYM", "THENSYM",
	"WHILESYM", "WRITESYM", "READSYM", "DOSYM", "CALLSYM",
	"CONSTSYM", "VARSYM", "PROCSYM", "PROGSYM", "TIMESEQ" ,"ELSESYM", "DIVEQ","FORSYM","STEPSYM","UNTILSYM","AND","OR","NOT","CHAR","DOUBLE","DOUBLENUM","ARRAYSYM","LEFTSQ","RIGHTSQ"};
typedef  int *SYMSET; // SET OF SYMBOL;S
typedef  char ALFA[11];
typedef  enum { CONSTANT, VARIABLE, PROCEDUR ,CHAR, DOUBLE, ARRAY} OBJECTS;//标识符类型
typedef  enum { LIT, OPR, LOD, STO, CAL, INI, JMP, JPC } FCT;
typedef struct {
	FCT F;     /*FUNCTION CODE*/
	int L; 	/*0..LEVMAX  LEVEL*/
	double  A;     /*0..AMAX    DISPLACEMENT ADDR*/
} INSTRUCTION;
/* LIT O A -- LOAD CONSTANT A             */
/* OPR 0 A -- EXECUTE OPR A               */
/* LOD L A -- LOAD VARIABLE L,A           */
/* STO L A -- STORE VARIABLE L,A          */
/* CAL L A -- CALL PROCEDURE A AT LEVEL L */
/* INI 0 A -- INCREMET T-REGISTER BY A    */
/* JMP 0 A -- JUMP TO A                   */
/* JPC 0 A -- JUMP CONDITIONAL TO A       */
char   CH;  /*LAST CHAR READ*/
SYMBOL SYM; /*LAST SYMBOL READ*/
ALFA   ID;  /*LAST IDENTIFIER READ*/
int    NUM; /*LAST NUMBER READ*/
double DNUM;	//最近读入的实数
int    CC;  /*CHARACTER COUNT*/
int    LL;  /*LINE LENGTH*/
int    CX;  /*CODE ALLOCATION INDEX*/
char   LINE[81];
INSTRUCTION  CODE[CXMAX]; //可以用CX索引
ALFA    KWORD[NORW + 1];
SYMBOL  WSYM[NORW + 1];
SYMBOL  SSYM['^' + 1];
ALFA    MNEMONIC[9];
//表示声明开始的符号集合,表示语句开始的符号集,表示因子开始的符号集合
SYMSET  DECLBEGSYS, STATBEGSYS, FACBEGSYS;

struct {
	ALFA NAME;
	OBJECTS KIND;
	union {
		int VAL;   /*CONSTANT*/
		struct { int LEVEL, ADR, SIZE, ARRDEM;} vp;  /*VARIABLE,PROCEDUR:*/
	};
} TABLE[TXMAX];

FILE *FIN, *FOUT;
int ERR;

void EXPRESSION(SYMSET FSYS, int LEV, int &TX);
void TERM(SYMSET FSYS, int LEV, int &TX);

void bitCharge(void *sour, void *des, int length)
{
	unsigned char *t_sour = (unsigned char*)sour;
	unsigned char *t_des = (unsigned char*)des;
	for(int i = 0; i < length; i++)
		t_des[i] = t_sour[i];
}
//---------------------------------------------------------------------------
//SYMSET为int型指针，如果存在某一SYMBOL（int型）则以该SYMBOL为索引的单元为1
int SymIn(SYMBOL SYM, SYMSET S1) {
	return S1[SYM];
}
//---------------------------------------------------------------------------

SYMSET SymSetUnion(SYMSET S1, SYMSET S2) {
	SYMSET S = (SYMSET)malloc(sizeof(int)* SYMBOLNUM); 
	for (int i = 0; i < SYMBOLNUM; i++)
		if (S1[i] || S2[i]) S[i] = 1;
		else S[i] = 0;
	return S;
}
//---------------------------------------------------------------------------
SYMSET SymSetAdd(SYMBOL SY, SYMSET S) {
	SYMSET S1;
	S1 = (SYMSET)malloc(sizeof(int)* SYMBOLNUM);
	for (int i = 0; i < SYMBOLNUM; i++) S1[i] = S[i];
	S1[SY] = 1;
	return S1;
}
//---------------------------------------------------------------------------
SYMSET SymSetNew(SYMBOL a) {
	SYMSET S; int i;
	S = (SYMSET)malloc(sizeof(int)* SYMBOLNUM);
	for (i = 0; i < SYMBOLNUM; i++) S[i] = 0;
	S[a] = 1;
	return S;
}
//---------------------------------------------------------------------------
SYMSET SymSetNew(SYMBOL a, SYMBOL b) {
	SYMSET S; int i;
	S = (SYMSET)malloc(sizeof(int)* SYMBOLNUM);
	for (i = 0; i < SYMBOLNUM; i++) S[i] = 0;
	S[a] = 1;  S[b] = 1;
	return S;
}
//---------------------------------------------------------------------------
SYMSET SymSetNew(SYMBOL a, SYMBOL b, SYMBOL c) {
	SYMSET S; int i;
	S = (SYMSET)malloc(sizeof(int)* SYMBOLNUM);
	for (i = 0; i < SYMBOLNUM; i++) S[i] = 0;
	S[a] = 1;  S[b] = 1; S[c] = 1;
	return S;
}
//---------------------------------------------------------------------------
SYMSET SymSetNew(SYMBOL a, SYMBOL b, SYMBOL c, SYMBOL d) {
	SYMSET S; int i;
	S = (SYMSET)malloc(sizeof(int)* SYMBOLNUM);
	for (i = 0; i < SYMBOLNUM; i++) S[i] = 0;
	S[a] = 1;  S[b] = 1; S[c] = 1; S[d] = 1;
	return S;
}
//---------------------------------------------------------------------------
SYMSET SymSetNew(SYMBOL a, SYMBOL b, SYMBOL c, SYMBOL d, SYMBOL e) {
	SYMSET S; int i;
	S = (SYMSET)malloc(sizeof(int)* SYMBOLNUM);
	for (i = 0; i < SYMBOLNUM; i++) S[i] = 0;
	S[a] = 1;  S[b] = 1; S[c] = 1; S[d] = 1; S[e] = 1;
	return S;
}
//---------------------------------------------------------------------------
SYMSET SymSetNew(SYMBOL a, SYMBOL b, SYMBOL c, SYMBOL d, SYMBOL e, SYMBOL f) {
	SYMSET S; int i;
	S = (SYMSET)malloc(sizeof(int)* SYMBOLNUM);
	for (i = 0; i < SYMBOLNUM; i++) S[i] = 0;
	S[a] = 1;  S[b] = 1; S[c] = 1; S[d] = 1; S[e] = 1; S[f] = 1;
	return S;
}
//---------------------------------------------------------------------------
SYMSET SymSetNULL() {
	SYMSET S; int i;
	S = (SYMSET)malloc(sizeof(int)* SYMBOLNUM);
	for (i = 0; i < SYMBOLNUM; i++) S[i] = 0;
	return S;
}
//---------------------------------------------------------------------------
void Error(int n) {
	string s;
	s = s + "***" + ' ' + std::to_string(CC - 1) + "^";
	cout << s << n << endl;
	ERR++;
} /*Error*/
//---------------------------------------------------------------------------
void GetCh() {
	if (CC == LL) {
		if (feof(FIN)) {
			cout << "PROGRAM INCOMPLETE" << endl;
			fprintf(FOUT, "PROGRAM INCOMPLETE\n");
			fclose(FOUT);
			system("Pause");
			exit(0);
		}
		LL = 0; CC = 0;
		CH = ' ';
		while (!feof(FIN) && CH != 10)
		{
			CH = fgetc(FIN);  LINE[LL++] = CH;
		}
		LINE[LL - 1] = ' ';  LINE[LL] = 0;
		string s = std::to_string(CX);
		while (s.length() < 3) s = " " + s;
		s = s + " " + LINE;
		cout << s << endl;
		fprintf(FOUT, "%s\n", s.c_str());
	}
	CH = LINE[CC++];
} /*GetCh()*/
//---------------------------------------------------------------------------
void GetSym() {
	int i, J, K;   ALFA  A;
	while (CH <= ' ') GetCh();
	if (CH >= 'A' && CH <= 'Z') { /*ID OR RESERVED WORD*/
		K = 0;
		do {
			if (K<AL) A[K++] = CH;
			GetCh();
		} while ((CH >= 'A' && CH <= 'Z') || (CH >= '0' && CH <= '9'));
		A[K] = '\0';
		strcpy(ID, A); i = 1; J = NORW;
		do {
			K = (i + J) / 2;
			if (strcmp(ID, KWORD[K]) < 0) J = K - 1;
			else if (strcmp(ID, KWORD[K]) > 0) i = K + 1;
			else break;
		} while (i <= J);
		if (i <= J) SYM = WSYM[K];
		else SYM = IDENT;
	}
	else if(CH == '\'')
	{
		std::string s;
		
		GetCh();
		while(CH != '\'')
		{
			s.push_back(CH);
			GetCh();
		}
		GetCh();
		if(s.length() == 0)
		{
			Error(38);
		}
		else if(s.length() == 1)
		{
			NUM = s[0];
			SYM = NUMBER;
		}
		else
		{
		//字符串
		}
	}
	else
		if(CH >= '0' && CH <= '9')//这里不能加上.,否则在处理"END."时，会把后面的"."处理成DOUBLENUM，而不是PERIOD
		{
			//同时处理整数和实数
			std::string s;
			int real = 0;
			K = 0;
			do{
				if(CH == '.')
					real++;
				if(real > 1)
					Error(30);		
				s.push_back(CH);
				K++;
				GetCh();
			}while((CH >= '0' && CH <= '9') || CH == '.');
			if(real == 0)
			{
				if(K > NMAX)
					Error(30);
				NUM = std::atoi(s.c_str());
				SYM = NUMBER;
			}
			else if(real == 1)
			{
				DNUM = std::atof(s.c_str());
				SYM = DOUBLENUM;
			}
		//if(CH >= '0' && CH <= '9') { /*NUMBER*/
		//	K = 0; NUM = 0; SYM = NUMBER;
		//	do {
		//		NUM = 10 * NUM + (CH - '0');
		//		K++; GetCh();
		//	} while (CH >= '0' && CH <= '9');
		//	if (K > NMAX) Error(30);
		}
		else //运算符处理（分多个字节和一个字节）
			if (CH == ':') {
				GetCh();
				if (CH == '=') { SYM = BECOMES; GetCh(); }
				else SYM = NUL;
			}
			else /* THE FOLLOWING TWO CHECK WERE ADDED
					BECAUSE ASCII DOES NOT HAVE A SINGLE CHARACTER FOR <= OR >= */
				if (CH == '<') {
					GetCh();
					if (CH == '=') 
					{ 
						SYM = LEQ; 
						GetCh(); 
					}
					else if(CH == '>')
					{
						SYM = NEQ;
						GetCh();
					}
					else
					{
						SYM = LSS;
					}
				}
				else if (CH == '>') {
					GetCh();
					if (CH == '=') 
					{ 
						SYM = GEQ; 
						GetCh(); 
					}
					else 
						SYM = GTR;
				}
				else if (CH == '*')
				{
					GetCh();
					if (CH == '=')
					{ 
						SYM = TIMESEQ; 
						GetCh(); 
					}
					else 
						SYM = TIMES;
				}
				else if (CH == '/')
				{
					GetCh();
					if(CH == '*')	//多行注释处理
					{
						do
						{
							GetCh();
							if(CH == '*')
							{
								GetCh();
								if(CH == '/')
									break;
							}
						}while(CH != 0);
						if(CH == 0)
							Error(37);
						else
							GetCh();
					}
					else if (CH == '=')
					{
						SYM = DIVEQ;
						GetCh();
					}
					else
					{
						SYM = SLASH;
					}
				}
				else if(CH == '|')
				{
					GetCh();
					if(CH == '|')
					{
						SYM = OR;	
						GetCh();
					}
					else
					{
						Error(37); //未知标识符
					}
				}
				else //当运算符只占一个字节时，在这里处理
				{
					SYM = SSYM[CH]; GetCh();
				}
} /*GetSym()*/
//---------------------------------------------------------------------------
void GEN(FCT X, int Y, int Z) {
	if (CX > CXMAX) {
		cout << "PROGRAM TOO LONG" << endl;
		fprintf(FOUT, "PROGRAM TOO LONG\n");
		fclose(FOUT);
		exit(0);
	}
	CODE[CX].F = X; CODE[CX].L = Y; CODE[CX].A = Z;
	CX++;
} /*GEN*/
//---------------------------------------------------------------------------
//后置条件：SYM属于S1与S2并集中的元素
void TEST(SYMSET S1, SYMSET S2, int N) {
	if (!SymIn(SYM, S1)) {
		Error(N);
		while (!SymIn(SYM, SymSetUnion(S1, S2))) GetSym();
	}
} /*TEST*/
//---------------------------------------------------------------------------
void ENTER(OBJECTS K, int LEV, int &TX, int &DX) { /*ENTER OBJECT INTO TABLE*/
	TX++;
	strcpy(TABLE[TX].NAME, ID); TABLE[TX].KIND = K;
	switch (K) {
		case CONSTANT:
			if (NUM > AMAX) { Error(31); NUM = 0; }
			TABLE[TX].VAL = NUM;
			break;
		case VARIABLE:
		case CHAR:
			TABLE[TX].vp.LEVEL = LEV; TABLE[TX].vp.ADR = DX; DX++;
			break;
		case DOUBLE://这里double占4个字节
			TABLE[TX].vp.LEVEL = LEV; TABLE[TX].vp.ADR = DX; DX++;
			break;
		case PROCEDUR:
			TABLE[TX].vp.LEVEL = LEV;
			break;
		case ARRAY:
			//数组所在层以及数组首地址
			TABLE[TX].vp.LEVEL = LEV;
			TABLE[TX].vp.ADR = DX;
			TABLE[TX].vp.ARRDEM = NUM;
			DX = DX + NUM;
			break;
	}
} /*ENTER*/
//---------------------------------------------------------------------------
int POSITION(ALFA ID, int TX) { /*FIND IDENTIFIER IN TABLE*/
	int i = TX;
	strcpy(TABLE[0].NAME, ID);
	while (strcmp(TABLE[i].NAME, ID) != 0) i--;
	return i;
} /*POSITION*/
//---------------------------------------------------------------------------
void ConstDeclaration(int LEV, int &TX, int &DX) {
	if (SYM == IDENT) {
		GetSym();
		if (SYM == EQL || SYM == BECOMES) {
			if (SYM == BECOMES) Error(1);
			GetSym();
			if (SYM == NUMBER) { ENTER(CONSTANT, LEV, TX, DX); GetSym(); }
			else Error(2);
		}
		else Error(3);
	}
	else Error(4);
} /*ConstDeclaration()*/

//CHAR类型处理函数
void CharDeclaration(int LEV, int &TX, int &DX)
{
	if(SYM == IDENT)
	{
		GetSym();
		if(SYM == LEFTSQ)
		{
			GetSym();
			if(SYM == NUMBER)
			{
				GetSym();
				if(SYM == RIGHTSQ)
				{
					GetSym();
					ENTER(ARRAY,LEV,TX,DX);			
				}
				else
				{
					Error(40);
				}
			}
			else
			{
				Error(39);
			}
		}
		else
		{
			//记录在符号表
			ENTER(CHAR,LEV,TX,DX);
		}
	}
	else
	{
		Error(4);
	}
}

//double类型处理

void DoubleDeclaration(int LEV, int &TX, int &DX)
{
	if(SYM == IDENT)
	{
		//记录在符号表
		ENTER(DOUBLE, LEV, TX, DX);
		GetSym();
	}
}
//---------------------------------------------------------------------------
void VarDeclaration(int LEV, int &TX, int &DX) {
	if (SYM == IDENT) { ENTER(VARIABLE, LEV, TX, DX); GetSym(); }
	else Error(4);
} /*VarDeclaration()*/
//---------------------------------------------------------------------------
void ListCode(int CX0) {  /*LIST CODE GENERATED FOR THIS Block*/
	if (true) //changed
		for (int i = CX0; i < CX; i++) {
			string s = std::to_string(i);
			while (s.length() < 3)s = " " + s;
			s = s + " " + MNEMONIC[CODE[i].F] + " " + std::to_string(CODE[i].L) + " " + std::to_string((int)CODE[i].A);
			cout << s << endl;
			fprintf(FOUT, "%3d%5s%4d%4f\n", i, MNEMONIC[CODE[i].F], CODE[i].L, CODE[i].A);
		}
} /*ListCode()*/;
//---------------------------------------------------------------------------
void FACTOR(SYMSET FSYS, int LEV, int &TX) {
	int i;
	TEST(FACBEGSYS, FSYS, 24);
	while (SymIn(SYM, FACBEGSYS)) {
		if (SYM == IDENT) {
			i = POSITION(ID, TX);
			if (i == 0) Error(11);
			else
				switch (TABLE[i].KIND) {
					case CONSTANT: GEN(LIT, 0, TABLE[i].VAL); break;
					case VARIABLE: 
					case CHAR:
					case DOUBLE:
							GEN(LOD, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR); break;
					case PROCEDUR: Error(21); break;
				}
			GetSym();
		}
		else
			if (SYM == NUMBER) {
				if (NUM > AMAX) { Error(31); NUM = 0; }
				GEN(LIT, 0, NUM); GetSym();
			}
			else if(SYM == DOUBLENUM)
			{
				GEN(LIT, 0, DNUM);
				GetSym();
			}
			else
				if (SYM == LPAREN) {
					GetSym(); EXPRESSION(SymSetAdd(RPAREN, FSYS), LEV, TX);
					if (SYM == RPAREN) GetSym();
					else Error(22);
				}
		TEST(FSYS, FACBEGSYS, 23);
	}
}/*FACTOR*/
//---------------------------------------------------------------------------
void TERM(SYMSET FSYS, int LEV, int &TX) {  /*TERM*/
	SYMBOL MULOP;
	FACTOR(SymSetUnion(FSYS, SymSetNew(TIMES, SLASH)), LEV, TX);
	while (SYM == TIMES || SYM == SLASH) {
		MULOP = SYM;  GetSym();
		FACTOR(SymSetUnion(FSYS, SymSetNew(TIMES, SLASH)), LEV, TX);
		if (MULOP == TIMES) GEN(OPR, 0, 4);
		else GEN(OPR, 0, 5);
	}
} /*TERM*/;
//---------------------------------------------------------------------------
void EXPRESSION(SYMSET FSYS, int LEV, int &TX) {
	SYMBOL ADDOP;
	if (SYM == PLUS || SYM == MINUS) {
		ADDOP = SYM; GetSym();
		TERM(SymSetUnion(FSYS, SymSetNew(PLUS, MINUS)), LEV, TX);
		if (ADDOP == MINUS) GEN(OPR, 0, 1);
	}
	else TERM(SymSetUnion(FSYS, SymSetNew(PLUS, MINUS)), LEV, TX);
	while (SYM == PLUS || SYM == MINUS) {
		ADDOP = SYM; GetSym();
		TERM(SymSetUnion(FSYS, SymSetNew(PLUS, MINUS)), LEV, TX);
		if (ADDOP == PLUS) GEN(OPR, 0, 2);
		else GEN(OPR, 0, 3);
	}
} /*EXPRESSION*/
//---------------------------------------------------------------------------
void CONDITION(SYMSET FSYS, int LEV, int &TX) {
	SYMBOL RELOP;
	if (SYM == ODDSYM) 
	{ 
		GetSym(); 
		EXPRESSION(FSYS, LEV, TX); 
		GEN(OPR, 0, 6); 
	}
	else if(SYM == NOT)
	{
		GetSym();
		EXPRESSION(FSYS,LEV,TX);
		GEN(OPR,0,17);
	}
	else {
		EXPRESSION(SymSetUnion(SymSetUnion(SymSetNew(EQL, NEQ, LSS, LEQ, GTR, GEQ),SymSetNew(AND,OR)), FSYS), LEV, TX);
		if (!SymIn(SYM, SymSetUnion(SymSetNew(EQL, NEQ, LSS, LEQ, GTR, GEQ),SymSetNew(AND,OR)))) Error(20);
		else {
			RELOP = SYM; GetSym(); EXPRESSION(FSYS, LEV, TX);
			switch (RELOP) {
				case EQL: GEN(OPR, 0, 8);  break;
				case NEQ: GEN(OPR, 0, 9);  break;
				case LSS: GEN(OPR, 0, 10); break;
				case GEQ: GEN(OPR, 0, 11); break;
				case GTR: GEN(OPR, 0, 12); break;
				case LEQ: GEN(OPR, 0, 13); break;
				case AND: GEN(OPR, 0, 4); break;
				case OR: GEN(OPR,0,2); break;
			}
		}
	}
} /*CONDITION*/
//---------------------------------------------------------------------------
void STATEMENT(SYMSET FSYS, int LEV, int &TX) {   /*STATEMENT*/
	int i, CX1, CX2,CX3;
	SYMBOL sym_temp;
	switch (SYM) {
		case IDENT:
			i = POSITION(ID, TX);
			if (i == 0) Error(11);
			else
				//添加字符类型的识别
				if (TABLE[i].KIND != VARIABLE && TABLE[i].KIND != CHAR && TABLE[i].KIND != DOUBLE) { /*ASSIGNMENT TO NON-VARIABLE*/
					Error(12); i = 0;
				}
			GetSym();
			if (SYM == BECOMES) { sym_temp = SYM; GetSym(); }
			else if (SYM == TIMESEQ) { sym_temp = SYM; GetSym(); }
			else if (SYM == DIVEQ){ sym_temp = SYM; GetSym(); }
			else Error(13);
			if (i != 0)
			{
				if (sym_temp == BECOMES)
				{
					//计算表达式的值,并将值放在栈顶
					EXPRESSION(FSYS, LEV, TX);
					GEN(STO, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR);
				}    
				else if (sym_temp == TIMESEQ)
				{
					//计算表达式的值,并将值放在栈顶
					EXPRESSION(FSYS, LEV, TX);
					GEN(LOD, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR);
					GEN(OPR, 0, 4);
					GEN(STO, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR);
				}
				else if (sym_temp == DIVEQ)
				{
					//将A的值放到栈顶
					GEN(LOD, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR);
					//计算表达式的值,并将值放在栈顶
					EXPRESSION(FSYS, LEV, TX);
					//将栈顶和次栈顶值相除，结果放在栈顶
					GEN(OPR, 0, 5);
					//将栈顶的值放在A中
					GEN(STO, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR);
				}
			}
			break;
		case READSYM:
			GetSym();
			if (SYM != LPAREN) Error(SYMBOLNUM);
			else
				do {
					GetSym();
					if (SYM == IDENT) i = POSITION(ID, TX);
					else i = 0;
					if (i == 0) Error(SYMBOLNUM);
					else {
						GEN(OPR, 0, 16);
						GEN(STO, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR);
					}
					GetSym();
				} while (SYM == COMMA);
			if (SYM != RPAREN) {
				Error(SYMBOLNUM);
				while (!SymIn(SYM, FSYS)) GetSym();
			}
			else GetSym();
			break; /* READSYM */
		case WRITESYM:
			GetSym();
			if (SYM == LPAREN) {
				do {
					GetSym();
					if(SYM == IDENT)
					{
						i = POSITION(ID,TX);
						if(i == 0)
							Error(11);
						if(TABLE[i].KIND == CHAR)
						{
							EXPRESSION(SymSetUnion(SymSetNew(RPAREN, COMMA), FSYS), LEV, TX);
						GEN(OPR,0,18);
						}
						else
						{
							EXPRESSION(SymSetUnion(SymSetNew(RPAREN, COMMA), FSYS), LEV, TX);
						GEN(OPR, 0, 14);
						}
					}
					else
					{
						EXPRESSION(SymSetUnion(SymSetNew(RPAREN, COMMA), FSYS), LEV, TX);
					GEN(OPR, 0, 14);
					}
				} while (SYM == COMMA);
				if (SYM != RPAREN) Error(SYMBOLNUM);
				else GetSym();
			}
			GEN(OPR, 0, 15);
			break; /*WRITESYM*/
		case CALLSYM:
			GetSym();
			if (SYM != IDENT) Error(14);
			else {
				i = POSITION(ID, TX);
				if (i == 0) Error(11);
				else
					if (TABLE[i].KIND == PROCEDUR)
						GEN(CAL, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR);
					else Error(15);
				GetSym();
			}
			break;
		case IFSYM:
			GetSym();
			CONDITION(SymSetUnion(SymSetNew(THENSYM), FSYS), LEV, TX); //这里的new应该不能包含DOSYM吧，已去除
			if (SYM == THENSYM) GetSym();
			else Error(16);
			//保存待回填的地址(E假出口)
			CX1 = CX;  GEN(JPC, 0, 0); //JPC条件为假时跳转,否则顺序执行
			STATEMENT(SymSetUnion(SymSetNew(ELSESYM),FSYS), LEV, TX);//注意在statement里获取了一个语句
			CX2 = CX;
			GEN(JMP, 0, 0);
			GetSym();
			CODE[CX1].A = CX;
			if (SYM == ELSESYM)
			{
				GetSym();
				STATEMENT(FSYS, LEV, TX);
			}     
			CODE[CX2].A = CX;
			break;
		case BEGINSYM:
			GetSym();
			STATEMENT(SymSetUnion(SymSetNew(SEMICOLON, ENDSYM), FSYS), LEV, TX);
			while (SymIn(SYM, SymSetAdd(SEMICOLON, STATBEGSYS))) {
				if (SYM == SEMICOLON) GetSym();
				//else Error(10);
				STATEMENT(SymSetUnion(SymSetNew(SEMICOLON, ENDSYM), FSYS), LEV, TX);
			}
			if (SYM == ENDSYM) GetSym();
			else Error(17);
			break;
		case WHILESYM:
			CX1 = CX; GetSym(); CONDITION(SymSetAdd(DOSYM, FSYS), LEV, TX);
			CX2 = CX; GEN(JPC, 0, 0);
			if (SYM == DOSYM) GetSym();
			else Error(18);
			STATEMENT(FSYS, LEV, TX);
			GEN(JMP, 0, CX1);
			CODE[CX2].A = CX;
			break;
		case FORSYM:
			//i:=E
			GetSym();
			if (SYM == IDENT)
			{
				i = POSITION(ID, TX); //保存变量在符号表中的位置，便于下面引用
				STATEMENT(SymSetAdd(STEPSYM, FSYS), LEV, TX); //处理并赋值到i
			}  
			else
			{
				Error(33);
			}
			CX1 = CX;
			GEN(JMP, 0, 0);
			//i:=E + i,计算E
			CX2 = CX;
			if (SYM == STEPSYM)
			{
				GetSym();
				EXPRESSION(SymSetAdd(UNTILSYM, FSYS), LEV, TX);
				//将变量i存储到栈顶
				GEN(LOD, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR);
				//相加
				GEN(OPR, 0, 2);
				//将栈顶放入变量i
				GEN(STO, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR);
			}
			else
			{
				Error(34);
			}
			//until E
			CODE[CX1].A = CX;
			if (SYM == UNTILSYM)
			{
				GetSym();
				GEN(LOD, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR);
				EXPRESSION(SymSetAdd(DOSYM, FSYS), LEV, TX);
				GEN(OPR, 0, 13);
				CX3 = CX;
				GEN(JPC, 0, 0);
			}
			else
			{
				Error(35);
			}
			if (SYM == DOSYM)
			{
				GetSym();
				STATEMENT(FSYS, LEV, TX);
			}
			else
			{
				Error(36);
			}
			GEN(JMP, 0, CX2);
			CODE[CX3].A = CX;
			break;
	}
	TEST(FSYS, SymSetNULL(), 19);
} /*STATEMENT*/
//---------------------------------------------------------------------------
void Block(int LEV, int TX, SYMSET FSYS) {
	int DX = 3;    /*DATA ALLOCATION INDEX*/
	int TX0 = TX;  /*INITIAL TABLE INDEX*/
	int CX0 = CX;  /*INITIAL CODE INDEX*/
	TABLE[TX].vp.ADR = CX; GEN(JMP, 0, 0);
	if (LEV > LEVMAX) Error(32);
	do {
		if (SYM == CONSTSYM) {
			GetSym();
			do {
				ConstDeclaration(LEV, TX, DX);
				while (SYM == COMMA) {
					GetSym();  ConstDeclaration(LEV, TX, DX);
				}
				if (SYM == SEMICOLON) GetSym();
				else Error(5);
			} while (SYM == IDENT);
		}
		if (SYM == VARSYM) {
			GetSym();
			do {
				VarDeclaration(LEV, TX, DX);
				while (SYM == COMMA) { GetSym(); VarDeclaration(LEV, TX, DX); }
				if (SYM == SEMICOLON) GetSym();
				else Error(5);
			} while (SYM == IDENT);
		}
		//CHAR变量的定义方法为CHAR A,B;C,D;
		if(SYM == CHARSYM)
		{
			GetSym();
			do{
				CharDeclaration(LEV, TX, DX);
				while(SYM == COMMA)
				{
					GetSym();
					CharDeclaration(LEV, TX, DX);
				}
				if(SYM == SEMICOLON)
					GetSym();
				else
					Error(5);
			}while(SYM == IDENT);
		}
		//DOUBLE类型
		if(SYM == DOUBLESYM)
		{
			GetSym();
			do{
				DoubleDeclaration(LEV,TX,DX);
				while(SYM == COMMA)
				{
					GetSym();
					DoubleDeclaration(LEV, TX, DX);
				}
				if(SYM == SEMICOLON)
					GetSym();
				else
					Error(5);
			}while(SYM == IDENT);
		}
		while (SYM == PROCSYM) {
			GetSym();
			if (SYM == IDENT) { ENTER(PROCEDUR, LEV, TX, DX); GetSym(); }
			else Error(4);
			if (SYM == SEMICOLON) GetSym();
			else Error(5);
			Block(LEV + 1, TX, SymSetAdd(SEMICOLON, FSYS));
			if (SYM == SEMICOLON) {
				GetSym();
				TEST(SymSetUnion(SymSetNew(IDENT, PROCSYM), STATBEGSYS), FSYS, 6);
			}
			else Error(5);
		}
		TEST(SymSetAdd(IDENT, STATBEGSYS), DECLBEGSYS, 7);
	} while (SymIn(SYM, DECLBEGSYS));
	CODE[TABLE[TX0].vp.ADR].A = CX;
	TABLE[TX0].vp.ADR = CX;   /*START ADDR OF CODE*/
	TABLE[TX0].vp.SIZE = DX;  /*SIZE OF DATA SEGMENT*/
	GEN(INI, 0, DX);
	STATEMENT(SymSetUnion(SymSetNew(SEMICOLON, ENDSYM), FSYS), LEV, TX);
	GEN(OPR, 0, 0);  /*RETURN*/
	TEST(FSYS, SymSetNULL(), 8);
	ListCode(CX0);
} /*Block*/
//---------------------------------------------------------------------------
int BASE(int L, int B, double S[]) {
	int B1 = B; /*FIND BASE L LEVELS DOWN*/
	while (L > 0) { B1 = S[B1]; L = L - 1; }
	return B1;
} /*BASE*/
//---------------------------------------------------------------------------
void Interpret() {
	const int STACKSIZE = 500;
	int P, B, T; 		/*PROGRAM BASE TOPSTACK REGISTERS*/
	char ch;
	INSTRUCTION I;
	double S[STACKSIZE];  	/*DATASTORE*/
	cout << "~~~ RUN PL0 ~~~" << endl;
	fprintf(FOUT, "~~~ RUN PL0 ~~~\n");
	T = 0; B = 1; P = 0;
	S[1] = 0; S[2] = 0; S[3] = 0;
	do {
		I = CODE[P]; P = P + 1;
		switch (I.F) {
			case LIT: T++; S[T] = I.A; break;
			case OPR:
					  switch ((int)I.A) { /*OPERATOR*/
						  case 0: /*RETURN*/ T = B - 1; P = S[T + 3]; B = S[T + 2]; break;
						  case 1: S[T] = -S[T];  break;
						  case 2: T--; S[T] = S[T] + S[T + 1];   break;
						  case 3: T--; S[T] = S[T] - S[T + 1];   break;
						  case 4: T--; S[T] = S[T] * S[T + 1];   break;
						  case 5: T--; S[T] = S[T] / S[T + 1]; break;
						  case 6: S[T] = ((int)S[T] % 2 != 0);        break;
						  case 8: T--; S[T] = S[T] == S[T + 1];  break;
						  case 9: T--; S[T] = S[T] != S[T + 1];  break;
						  case 10: T--; S[T] = S[T]<S[T + 1];   break;
						  case 11: T--; S[T] = S[T] >= S[T + 1];  break;
						  case 12: T--; S[T] = S[T]>S[T + 1];   break;
						  case 13: T--; S[T] = S[T] <= S[T + 1];  break;
						  case 14: cout << S[T] << endl; fprintf(FOUT, "%d\n", (int)S[T]); T--;
								   break;
						  case 15: /*Form1->printfs(""); fprintf(FOUT,"\n"); */ break;
						  case 16: T++;  cout << "please input:" << endl; cin >> S[T];
								   fprintf(FOUT, "? %d\n", (int)S[T]);
								   break;
						  case 17: S[T] = !S[T]; break;
						  case 18: ch = S[T];cout << ch << endl; T--;
					  }
					  break;
			case LOD: T++; S[T] = S[BASE(I.L, B, S) + (int)I.A]; break;
			case STO: S[BASE(I.L, B, S) + (int)I.A] = S[T]; T--; break;
			case CAL: /*GENERAT NEW Block MARK*/
					  S[T + 1] = BASE(I.L, B, S); S[T + 2] = B; S[T + 3] = P;
					  B = T + 1; P = I.A; break;
			case INI: T = T + I.A;  break;
			case JMP: P = I.A; break;
			case JPC: if (S[T] == 0) P = I.A;  T--;  break;
		} /*switch*/
	} while (P != 0);
	cout << "~~~ END PL0 ~~~";
	fprintf(FOUT, "~~~ END PL0 ~~~\n");
} /*Interpret*/
//---------------------------------------------------------------------------
void run() {
	string EditNamein;
	string EditNameout;
	int i = 1;
#ifdef DEBUG
	cout << "please input PL0 file name(format is *.PL0)" << endl;
	cin >> EditNamein;
	cout << "please input output file name(format is *.txt)" << endl;
	cin >> EditNameout;
#else
	EditNamein = "E01.PL0";
	EditNameout = "E01.txt";
#endif

	for (CH = ' '; CH <= '^'; CH++)
		SSYM[CH] = NUL;

	//关键字单词，共NORW个
	i = 1;
	strcpy(KWORD[i++],"ARRAY");
	strcpy(KWORD[i++], "BEGIN");    strcpy(KWORD[i++], "CALL");
	strcpy(KWORD[i++], "CHAR");
	strcpy(KWORD[i++], "CONST");    strcpy(KWORD[i++], "DO");
	strcpy(KWORD[i++], "DOUBLE");
	strcpy(KWORD[i++], "ELSE");     strcpy(KWORD[i++], "END");
	strcpy(KWORD[i++], "FOR");      strcpy(KWORD[i++], "IF");
	strcpy(KWORD[i++], "ODD");      strcpy(KWORD[i++], "PROCEDURE");
	strcpy(KWORD[i++], "PROGRAM");  strcpy(KWORD[i++], "READ");
	strcpy(KWORD[i++], "STEP");     strcpy(KWORD[i++], "THEN");
	strcpy(KWORD[i++], "UNTIL");    strcpy(KWORD[i++], "VAR");
	strcpy(KWORD[i++], "WHILE");    strcpy(KWORD[i++], "WRITE");

	//关键字
	i = 1;
	WSYM[i++] = ARRAYSYM;
	WSYM[i++] = BEGINSYM;   WSYM[i++] = CALLSYM;
	WSYM[i++] = CHARSYM;
	WSYM[i++] = CONSTSYM;   WSYM[i++] = DOSYM;
	WSYM[i++] = DOUBLESYM;
	WSYM[i++] = ELSESYM;    WSYM[i++] = ENDSYM;
	WSYM[i++] = FORSYM;     WSYM[i++] = IFSYM;
	WSYM[i++] = ODDSYM;     WSYM[i++] = PROCSYM;
	WSYM[i++] = PROGSYM;    WSYM[i++] = READSYM;
	WSYM[i++] = STEPSYM;    WSYM[i++] = THENSYM;
	WSYM[i++] = UNTILSYM;   WSYM[i++] = VARSYM;
	WSYM[i++] = WHILESYM;   WSYM[i++] = WRITESYM;

	//运算符(加减)和分界符（逗号,分号,括号）
	SSYM['+'] = PLUS;      SSYM['-'] = MINUS;
	SSYM['*'] = TIMES;     SSYM['/'] = SLASH;
	SSYM['('] = LPAREN;    SSYM[')'] = RPAREN;
	SSYM['='] = EQL;       SSYM[','] = COMMA;
	SSYM['.'] = PERIOD;    SSYM['#'] = NEQ;
	SSYM[';'] = SEMICOLON; SSYM['&'] = AND;       
	SSYM['!'] = NOT;	   SSYM['['] = LEFTSQ;
	SSYM[']'] = RIGHTSQ;

	//目标代码指令
	strcpy(MNEMONIC[LIT], "LIT");   strcpy(MNEMONIC[OPR], "OPR");
	strcpy(MNEMONIC[LOD], "LOD");   strcpy(MNEMONIC[STO], "STO");
	strcpy(MNEMONIC[CAL], "CAL");   strcpy(MNEMONIC[INI], "INI");
	strcpy(MNEMONIC[JMP], "JMP");   strcpy(MNEMONIC[JPC], "JPC");

	//SYMBOLNUM代表符号个数
	DECLBEGSYS = (int*)malloc(sizeof(int)* SYMBOLNUM);
	STATBEGSYS = (int*)malloc(sizeof(int)* SYMBOLNUM);
	FACBEGSYS = (int*)malloc(sizeof(int)* SYMBOLNUM);

	for (int j = 0; j < SYMBOLNUM; j++)
	{
		DECLBEGSYS[j] = 0;  STATBEGSYS[j] = 0;  FACBEGSYS[j] = 0;
	}

	//声明
	DECLBEGSYS[CONSTSYM] = 1;
	DECLBEGSYS[VARSYM] = 1;
	DECLBEGSYS[PROCSYM] = 1;
	DECLBEGSYS[CHARSYM] = 1;
	DECLBEGSYS[DOUBLESYM] = 1;
	STATBEGSYS[BEGINSYM] = 1;
	STATBEGSYS[CALLSYM] = 1;
	STATBEGSYS[IFSYM] = 1;
	STATBEGSYS[WHILESYM] = 1;
	STATBEGSYS[WRITESYM] = 1;
	FACBEGSYS[IDENT] = 1;
	FACBEGSYS[NUMBER] = 1;
	FACBEGSYS[DOUBLENUM] = 1;
	FACBEGSYS[LPAREN] = 1;

	if ((FIN = fopen(EditNamein.c_str(), "r")) != 0)
	{
		FOUT = fopen(EditNameout.c_str(), "w");
		cout << "=== COMPILE PL0 ===" << endl;
		fprintf(FOUT, "=== COMPILE PL0 ===\n");

		//
		ERR = 0;
		CC = 0; CX = 0; LL = 0; CH = ' ';

		GetSym();

		if (SYM != PROGSYM)
		{
			Error(0);
		}
		else
		{
			GetSym();
			if (SYM != IDENT)
				Error(0);
			else
			{
				GetSym();
				if (SYM != SEMICOLON)
					Error(5);
				else
					GetSym();
			}
		}

		//PERIOD为句号
		Block(0, 0, SymSetAdd(PERIOD, SymSetUnion(DECLBEGSYS, STATBEGSYS)));

		//不是句号结束，则报错
		if (SYM != PERIOD)
			Error(9);

		//
		if (ERR == 0)
		{
			Interpret();
		}
		else
		{
			cout << "ERROR IN PL/0 PROGRAM" << endl;
			fprintf(FOUT, "ERROR IN PL/0 PROGRAM");
		}
		fprintf(FOUT, "\n"); fclose(FOUT);
	}
	else
	{
		perror("Error opening file");
	}
}
//---------------------------------------------------------------------------

