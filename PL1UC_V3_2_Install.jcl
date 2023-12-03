//INSTALL JOB 'ME','TOO',CLASS=S,MSGCLASS=A,MSGLEVEL=(1,1),
//   USER=HERC01,PASSWORD=CUL8TR
//********************************************************************
//*                                                                  *
//*  -A UTILITY CALL PACKER WILL USED TO 'COMPRESS' AN UNCOMPRESSED  *
//*   OBJECT DECK FOR INCLUSION IN THE PL1UC PROGRAM.                *
//*                                                                  *
//********************************************************************
//STEP2  EXEC ASMFCG
//ASM.SYSIN DD DSN=HERC01.PL1UC.INSTALL(PACKER),DISP=SHR
//GO.SYSPRINT  DD SYSOUT=*
//GO.DDI  DD  DSN=HERC01.PL1UC.INSTALL(GETMEM),DISP=SHR
//GO.DDO  DD  DSN=&&GETMEM,DISP=(NEW,PASS),UNIT=VIO,
//        SPACE=(TRK,(2,2))
//********************************************************************
//*                                                                  *
//*  -COMPILE AND LINK THE PL1UC PROGRAM.                            *
//*                                                                  *
//********************************************************************
//STEP3  EXEC PL1LFCL,REGION.PL1L=128K   <===REGION NEEDED FOR TK3
//PL1L.SYSIN DD DSN=HERC01.PL1UC.INSTALL(PL1UC),DISP=SHR
//LKED.SYSLMOD DD DSN=HERCEL.TEST.LOADLIB(PL1UC),DISP=SHR
//LKED.SYSIN DD DSN=&&GETMEM,DISP=(OLD,DELETE)
//*
