//COMPILE JOB 'ME','TOO',CLASS=S,MSGCLASS=A,MSGLEVEL=(1,1),
//   USER=HERC01,PASSWORD=CUL8TR
//********************************************************************
//*                                                                  *
//*   COMPILE THE INSTALL FILES FOR PL1UC V3.3                       *
//*                                                                  *
//********************************************************************
//STEP1  EXEC PL1LFCL
//PL1L.SYSIN DD DSN=HERC01.PL1UC.INSTALL(PL1UC),DISP=SHR
//LKED.SYSLMOD DD DSN=HERC01.PL1UC.LOADLIB(PL1UC),DISP=SHR
//LKED.SYSIN  DD *
 INCLUDE MYLIB(GETMEM)
//LKED.SYSLIB DD DSN=SYS1.PL1LIB,DISP=SHR
//LKED.MYLIB  DD DSN=HERC01.PL1UC.LOADLIB,DISP=SHR
