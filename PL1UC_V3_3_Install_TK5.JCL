//INSTALL JOB 'ME','TOO',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//   USER=HERC01,PASSWORD=CUL8TR
//********************************************************************
//*                                                                  *
//*   INSTALL PL1UC V3.3 FOR TK5 SYSTEMS                             *
//*                                                                  *
//*  -THIS JOB CREATES TWO DATASETS TSO001 FOR PL1UC V3.3 CALLED     *
//*   'HERC01.PL1UC.INSTALL' AND 'HERC01.PL1UC.LOADLIB'              *
//*   USING THE NJE-- RECEIVE COMMAND.                               *
//*                                                                  *
//*  -NOTE ==========> THIS JCL IS FOR TK5 <==================       *
//*   TO USE FOR OTHER OTHER, YOU NEED TO CHANGE THE VOL SERS        *
//*                                                                  *
//********************************************************************
//STEP1 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN  DD *
 DELETE 'HERC01.PL1UC.INSTALL'  NONVSAM PURGE
 DELETE 'HERC01.PL1UC.LOADLIB'  NONVSAM PURGE
 SET LASTCC = 0
 SET MAXCC = 0
//********************************************************************
//*                                                                  *
//*  - RECEIVE THE 'HERC01.PL1UC.XMI'  WHICH WILL                    *
//*    CREATE THE 'HERC01.PL1UC.INSTALL AND                          *
//*               'HERC01.PL1UC.LOADLIB                              *
//*                                                                  *
//********************************************************************
//STEP2  EXEC PGM=IKJEFT01,REGION=4096K
//SYSTSPRT DD  SYSOUT=*
//SYSTSIN  DD  *
 RECEIVE  DATASET('HERC01.PL1UC.INSTALL') VOL(TSO001) UNIT(SYSDA) -
          DIR(2) INDATASET('HERC01.PL1UC.XMI')
 RECEIVE  DATASET('HERC01.PL1UC.LOADLIB') VOL(TSO001) UNIT(SYSDA) -
          DIR(2) INDATASET('HERC01.PL1UC.INSTALL(LOADLIB)')
//********************************************************************
//*                                                                  *
//*  -IF ALL STEPS COMPLETED WITH CC 0, UPDATE THE SYSTEM DATASETS   *
//*   SYS2.LINKLIB AND SYS2.PROCLIB.  NOTE - COMPRESS IS ***NOT***   *
//*   RECOMENDED UNLESS NO OTHER ACTIVITY IS GOING ON.               *
//*                                                                  *
//********************************************************************
//STEP3  EXEC PGM=IEBCOPY,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//PROCIN   DD DSN=HERC01.PL1UC.INSTALL,DISP=SHR
//PROCOUT  DD DSN=SYS2.PROCLIB,DISP=SHR
//LOADIN   DD DSN=HERC01.PL1UC.LOADLIB,DISP=SHR
//LOADOUT  DD DSN=SYS2.LINKLIB,DISP=SHR
//SYSIN    DD *
 COPY INDD=PROCIN,OUTDD=PROCOUT
 SELECT MEMBER=((PL1UF,,R))
 SELECT MEMBER=((PL1UFC,,R))
 SELECT MEMBER=((PL1UFCG,,R))
 SELECT MEMBER=((PL1UFCL,,R))
 SELECT MEMBER=((PL1UFCLG,,R))
 COPY INDD=LOADIN,OUTDD=LOADOUT
 SELECT MEMBER=((PL1UC,,R))
//********************************************************************
//*                                                                  *
//*  -IF ALL STEPS COMPLETED WITH CC 0, DELETE THE XMI FILE          *
//*                                                                  *
//********************************************************************
//STEP4  EXEC PGM=IEFBR14,COND=(0,NE)
//DD1    DD  DSN=HERC01.PL1UC.XMI,DISP=(OLD,DELETE)
