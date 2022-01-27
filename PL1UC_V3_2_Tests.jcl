//PL1UTST1 JOB  ,'ME',MSGCLASS=A,REGION=512K,
//    USER=HERC01,PASSWORD=CUL8TR
//*****************************************************************
//*
//*    TEST EXECUTION OF THE PL1UF PROC CLASSIC MODE
//*
//*****************************************************************
//STEP1   EXEC PL1UF
//PL1U.PL1LIB DD DSN=HERC01.PL1UC.INSTALL,DISP=SHR
//PL1U.SYSIN DD DSN=HERC01.PL1UC.INSTALL(TESTER),DISP=SHR
//*
//PL1UTST2 JOB  ,'ME',MSGCLASS=A,REGION=512K,
//    USER=HERC01,PASSWORD=CUL8TR
//*****************************************************************
//*
//*    TEST EXECUTION OF THE PL1UF PROC PLUS MODE
//*
//*****************************************************************
//STEP1   EXEC PL1UF,MODE=PLUS
//PL1U.PL1LIB DD DSN=HERC01.PL1UC.INSTALL,DISP=SHR
//PL1U.SYSIN DD DSN=HERC01.PL1UC.INSTALL(TESTER),DISP=SHR
//*
//PL1UTST3 JOB  ,'ME',MSGCLASS=A,REGION=512K,
//    USER=HERC01,PASSWORD=CUL8TR
//*****************************************************************
//*
//*    TEST EXECUTION OF THE PL1UFC PROC PLUS MODE
//*
//*****************************************************************
//STEP1   EXEC PL1UFC,MODE=PLUS
//PL1U.PL1LIB DD DSN=HERC01.PL1UC.INSTALL,DISP=SHR
//PL1U.SYSIN DD DSN=HERC01.PL1UC.INSTALL(TESTER),DISP=SHR
//*
//PL1UTST4 JOB  ,'ME',MSGCLASS=A,REGION=512K,
//    USER=HERC01,PASSWORD=CUL8TR
//*****************************************************************
//*
//*    TEST EXECUTION OF THE PL1UFCL PROC PLUS MODE
//*
//*****************************************************************
//STEP1   EXEC PL1UFCL,MODE=PLUS
//PL1U.PL1LIB DD DSN=HERC01.PL1UC.INSTALL,DISP=SHR
//PL1U.SYSIN DD DSN=HERC01.PL1UC.INSTALL(TESTER),DISP=SHR
//*
//PL1UTST5 JOB  ,'ME',MSGCLASS=A,REGION=512K,
//    USER=HERC01,PASSWORD=CUL8TR
//*****************************************************************
//*
//*    TEST EXECUTION OF THE PL1UFCG PROC PLUS MODE
//*
//*****************************************************************
//STEP1   EXEC PL1UFCG,MODE=PLUS,PARM.GO='/PL1UFCG.'
//PL1U.PL1LIB DD DSN=HERC01.PL1UC.INSTALL,DISP=SHR
//PL1U.SYSIN DD DSN=HERC01.PL1UC.INSTALL(TESTER),DISP=SHR
//*
//PL1UTST6 JOB  ,'ME',MSGCLASS=A,REGION=512K,
//    USER=HERC01,PASSWORD=CUL8TR
//*****************************************************************
//*
//*    TEST EXECUTION OF THE PL1UFCLG PROC PLUS MODE
//*
//*****************************************************************
//STEP1   EXEC PL1UFCLG,MODE=PLUS,PARM.GO='PL1UFCLG.'
//PL1U.PL1LIB DD DSN=HERC01.PL1UC.INSTALL,DISP=SHR
//PL1U.SYSIN DD DSN=HERC01.PL1UC.INSTALL(TESTER),DISP=SHR
//*
