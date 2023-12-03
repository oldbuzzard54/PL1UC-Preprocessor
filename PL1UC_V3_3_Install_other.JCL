//INSTALL JOB 'ME','TOO',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//   USER=HERC01,PASSWORD=CUL8TR
//********************************************************************
//*                                                                  *
//*   INSTALL PL1UC V3.3 FOR NON TK5 SYSTEMS                         *
//*     OR FOR USING RECV370 TO RECEIVE THE XMI FILE.                *
//*                                                                  *
//*  -THIS JOB CREATES TWO DATASETS TSO001 FOR PL1UC V3.3 CALLED     *
//*   'HERC01.PL1UC.INSTALL' AND 'HERC01.PL1UC.LOADLIB'              *
//*   USING THE RECV370 PROGRAM.                                     *
//*                                                                  *
//*  -NOTE ==========> THIS JCL IS FOR TK5 <==================       *
//*   TO USE FOR OTHER, YOU NEED TO CHANGE THE SOME THINGS           *
//*                                                                  *
//********************************************************************
//STEP0 EXEC PGM=IDCAMS
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
//STEP1A   EXEC PGM=RECV370
//RECVLOG   DD SYSOUT=*
//SYSPRINT  DD SYSOUT=*                     IEBCOPY OUTPUT MESSAGES
//SYSIN     DD DUMMY                        IEBCOPY REQUIRES
//SYSUT2    DD DDNAME=XMITOUT
//SYSUT1    DD UNIT=SYSDA,                  WORK DATASET
//             SPACE=(CYL,(10,10)),
//             DCB=BLKSIZE=5600
//XMITIN  DD DSN=HERC01.PL1UC.XMI,DISP=SHR
//XMITOUT DD DSN=HERC01.PL1UC.INSTALL,DISP=(NEW,CATLG,DELETE),
//           UNIT=TSO,SPACE=(TRK,(10,10,10),RLSE),
//           DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)
//STEP1B   EXEC PGM=RECV370
//RECVLOG   DD SYSOUT=*
//SYSPRINT  DD SYSOUT=*                     IEBCOPY OUTPUT MESSAGES
//SYSIN     DD DUMMY                        IEBCOPY REQUIRES
//SYSUT2    DD DDNAME=XMITOUT
//SYSUT1    DD UNIT=SYSDA,                  WORK DATASET
//             SPACE=(CYL,(10,10)),
//             DCB=BLKSIZE=5600
//XMITIN  DD DSN=HERC01.PL1UC.INSTALL(LOADLIB),DISP=SHR
//XMITOUT DD DSN=HERC01.PL1UC.LOADLIB,DISP=(NEW,CATLG,DELETE),
//           UNIT=TSO,SPACE=(TRK,(10,10,10),RLSE),
//           DCB=(RECFM=U,BLKSIZE=19060)
//********************************************************************
//*                                                                  *
//*  -IF ALL STEPS COMPLETED WITH CC 0, UPDATE THE SYSTEM DATASETS   *
//*   SYS2.LINKLIB AND SYS2.PROCLIB.  NOTE - COMPRESS IS ***NOT***   *
//*   RECOMENDED UNLESS NO OTHER ACTIVITY IS GOING ON.               *
//*                                                                  *
//********************************************************************
//STEP2  EXEC PGM=IEBCOPY,COND=(0,NE)
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
//STEP3  EXEC PGM=IEFBR14,COND=(0,NE)
//DD1    DD  DSN=HERC01.PL1UC.XMI,DISP=(OLD,DELETE)
