//INSTALL JOB 'ME','TOO',CLASS=S,MSGCLASS=A,MSGLEVEL=(1,1),
//   USER=HERC01,PASSWORD=CUL8TR
//********************************************************************
//*                                                                  *
//*   INSTALL PL1UC V3.2                                             *
//*                                                                  *
//*  -THIS JOB CREATES A DATASET CALLED 'HERC01.PL1UC.INSTALL'       *
//*   ON DISK VOLUME PUB000                                          *
//*  -A UTILITY CALL PACKER WILL USED TO 'COMPRESS' AN UNCOMPRESSED  *
//*   OBJECT DECK FOR INCLUSION IN THE PL1UC PROGRAM.                *
//*                                                                  *
//*                                                                  *
//********************************************************************
//STEP0 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN  DD *
 DELETE 'HERC01.PL1UC.INSTALL'  NONVSAM PURGE
 SET LASTCC = 0
 SET MAXCC = 0
//*
//STEP1 EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD SYSOUT=*
//SYSUT2   DD DSN=HERC01.PL1UC.INSTALL,DISP=(NEW,CATLG),
//         UNIT=DISK,VOL=SER=PUB000,SPACE=(TRK,(10,10,10)),
//         DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)
//SYSIN  DD DATA,DLM='$$'
./ ADD NAME=GETMEM
*UNPACKER HEADER 01 FB 00080 00080                                             *
02C5E2C4404040404040002040400001C7C5E3D4C5D44040000000004000032CC7C5E3D4C5D4D740
0100000840000001404040404040404040404040404040404040404040404040C7C5E3D4C5D4F0F1
02E3E7E340000000404000124040000192D5F01247FF000C92E8F00A05F047FF008A404040404040
4040404040404040404040404040404040404040404040404040404040404040C7C5E3D4C5D4F0F2
02E3E7E340000013404000384040000184C7C5E3D4C5D44040F14BF04BF040F0F161F0F561F2F240
F0F94BF4F0406040C7C5E3D4C5D4406040C1C3C3C5E2E240D7C4E240D4C5D4C2C7C5E3D4C5D4F0F3
02E3E7E34000004B4040000340400001C5D9E2404040404040404040404040404040404040404040
4040404040404040404040404040404040404040404040404040404040404040C7C5E3D4C5D4F0F4
02E3E7E340000098404000384040000190ECD00C185D41DF00425050D00450D0500805B095E8F004
4770B020982510005820200058303000584040005870500047F0B02898241000C7C5E3D4C5D4F0F5
02E3E7E3400000D040400038404000014170B24841C0B1E858F2000049F0B2784780B05449F0B27A
4780B0BC49F0B27C4780B0DE49F0B27E4780B18247F0B1D29110C0304710B1D2C7C5E3D4C5D4F0F6
02E3E7E3400001084040003840400001D5073000B25C4780B070D207B210300047F0B076D207B210
B26407004510B080800002940A139110C0304780B1BE9502C01A4770B09A916BC7C5E3D4C5D4F0F7
02E3E7E3400001404040003840400001C0244780B0AA07004510B0A4800002940A1447F0B1D24800
C03E4510B0B20A0A5010B24C47F0B1AA9110C0304780B1D2181C180313110A12C7C5E3D4C5D4F0F8
02E3E7E340000178404000384040000112FF4770B1BE947FB2409640B24047F0B1AA9110C0304780
B1D29180B2404710B1D25850B24C5860B2509140B2404780B15407004510B118C7C5E3D4C5D4F0F9
02E3E7E3400001B04040003840400001000000008080000000000000000000000000000050C10008
5051000C58F1000858F0F03005EF4110B10458E0100858F0E03405EF48A0C03EC7C5E3D4C5D4F1F0
02E3E7E3400001E840400038404000015810C0444BA100164780B17A1AA550A0B25494BFB2401865
48F0C05206F044F0B242416F60015960B25447B0B1725060B25047F0B1AA9640C7C5E3D4C5D4F1F1
02E3E7E3400002204040003840400001B24047F0B1AA9680B24047F0B1BE9110C0304780B1D24800
C03E5850B24C411050000A0A4510B1A0800002940A1447F0B1AA47F0B1BED203C7C5E3D4C5D4F1F2
02E3E7E34000025840400038404000017000B26C58D0D00498ECD00C41F0000007FED2037000B270
58D0D00498ECD00C41F0000407FED2037000B27458D0D00498ECD00C41F00008C7C5E3D4C5D4F1F3
02E3E7E340000290404000384040000107FE00000000000000000000000000000000000000000000
0000000100000200000000010000022600000000D7C4E2404040404002002400C7C5E3D4C5D4F1F4
02E3E7E3400002C84040002C40400001000000010000000100000000000000000000000100000001
0000000100000000000000010000D20040006000404040404040404040404040C7C5E3D4C5D4F1F5
02E3E7E34000030840400024404000014040404040404040D7C4E240404040400000000000000004
00000008000000040008000C4040404040404040404040404040404040404040C7C5E3D4C5D4F1F6
02D9D3C440404040404000144040404000010001090001290900014D09000249080002B540404040
4040404040404040404040404040404040404040404040404040404040404040C7C5E3D4C5D4F1F7
02C5D5C440404040404040404040404040404040404040404040404040404040F1F5F7F4F1E2C3F1
F0F340F0F2F0F1F2F2F0F0F54040404040404040404040404040404040404040C7C5E3D4C5D4F1F8
*UNPACKER TRAILER 01 0000018 0000036                                           *
./ ADD NAME=PACKER
         TITLE 'PACKER - PACK UNPACKED BINARY DECKS'
**
**
**   PACKER - PROGRAM TO PACK UNPACKED "DECKS". CARD DECKS WITH
**       BINARY DATA CAN BE TRANSMITTED AS TEXT.
**       ONLY THE CHARACTERS 0-9 AND A-F ARE IN THE DDI INPUT
**       "CARDS".  THE CARDS CAN BE READ BY PACKED AND
**       THE ORIGINAL FILE WILL BE REPRODUCED.
**
**   DDNAMES:
**   DDI       UNPACKED INPUT FILE.
**       PACKER VERSION 1 ONLY PROCESSES "CARD" IMAGES AS INPUT
**       RECFM F AND LRECL 80 ARE THE ONLY ACCEPTABLE INPUT FORMATS.
**   DDO       OUTPUT FILE THAT IS UNPACKED
**
PACKER   START
         SAVE   (14,12)
         BALR   R12,R0
         USING  *,R12
         LR     R5,R13
         LA     R13,SAVEAREA
         ST     R5,4(,R13)
         ST     R13,8(,R5)
         USING IHADCB,R11
         OPEN  (PRINTER,(OUTPUT))
         OPEN  (DDI,(INPUT))
**
         LA    R11,DDI
         CLI   DCBDSORG,DCBDSGPS          IS THE DSORG PS?
         BE    DSORGOK
         WTO   'PACKER-INVALID DSORG',ROUTCDE=(2,11)
         ABEND 0001
DSORGOK  EQU   *
         MVC   IHLDRFM,DCBRECFM
         TM    DCBRECFM,DCBRECBR          RECFM _B?
         BZ    BLKED
         MVI   IRECFM+1,C'B'
BLKED    EQU   *
         TM    DCBRECFM,DCBRECF           RECFM F?
         BZ    RECFM1
         MVI   IRECFM,C'F'
         B     RECFMOK
RECFM1   EQU   *
******** TM    DCBRECFM,DCBRECV           RECFM V?
******** BZ    RECFMBAD                   RECFM V NOT SUPPORTED
******** MVI   IRECFM,C'V'                  IN VERSION 1
*******  BO    RECFMOK
RECFMBAD EQU   *
         WTO   'PACKER-INVALID RECFM',ROUTCDE=(2,11)
         ABEND 0002
RECFMOK  EQU   *
         MVC   ILN1REC,IRECFM
         LH    R2,DCBLRECL
         CH    R2,=H'80'                 ONLY LRECL 80 IN VERSION 1
         BE    LRECLOK
         WTO   'PACKER-DDI LRECL MUST BE 80',ROUTCDE=(2,11)
         ABEND 0003
LRECLOK  EQU   *
         STH   R2,ILRECL
         CVD   R2,DWORD
         UNPK  ILN1LREC,DWORD+5(3)
         OI    ILN1LREC+4,X'F0'
         LH    R2,DCBBLKSI
         STH   R2,IBLKSIZE
         CVD   R2,DWORD
         UNPK  ILN1BLK,DWORD+5(3)
         OI    ILN1BLK+4,X'F0'
         PUT   PRINTER,HLN1
         PUT   PRINTER,ILN1
**
**     DETERMINE IF THE DATA IN DDI HAS BEEN UNPACKED.
**        IF SO, SET UP DDO TO RECONSTRUCT ORIGINAL FILE.
**        IF NOT, ABORT
**
         GET   DDI,AREC
         CLC   AREC(L'HDRID),HDRID
         BE    MAKEUNPK
         WTO   'PACKER-DDI HAS NOT BEEN UNPACKED',ROUTCDE=(2,11)
         ABEND 0004
**
**     SETUP DDO TO RECONSTRUCT ORIGINAL FILE.
**
MAKEUNPK EQU   *
         MVC   DLN1T,AREC
         PUT   PRINTER,DLN1
         MVC   HDR,AREC
*
         CLI   HDRRECFM,C'F'
         BE    HDRRECOK
         WTO   'PACKER-ONLY FIXED RECORDS SUPPORTED',ROUTCDE=(2,11)
         ABEND 0005
HDRRECOK EQU   *
         LA    R11,DDI                    SET EOF ADDR
         LA    R2,EOF2                       FOR UNZIPPING
         ST    R2,DCBEODAD
*
         LA    R11,DDO
         CLI   HDRRECFM+1,C'B'
         BNE   HDRBLOCK
         OI    DCBRECFM,DCBRECBR           SET RECFM=?B
HDRBLOCK EQU   *
         OI    DCBRECFM,DCBRECF            SET RECFM=F?
         PACK  DWORD,HDRLRECL
         CVB   R2,DWORD
         STH   R2,DCBLRECL
         PACK  DWORD,HDRBLKSI
         CVB   R2,DWORD
         STH   R2,DCBBLKSI
         OPEN  (DDO,(OUTPUT))
UNZIPREC EQU   *
         GET   DDI,UREC
         GET   DDI,UREC+80
         AP    RECSIN,=P'2'
         LA    R2,OREC
         LA    R3,UREC
         LA    R4,20
         TR    UREC,PKTABLE-X'C0'
PKLP     PACK  0(5,R2),0(9,R3)
         LA    R2,4(,R2)
         LA    R3,8(,R3)
         BCT   R4,PKLP
         LA    R5,OREC
         LA    R6,OREC+L'OREC
         PUT   DDO,OREC
         AP    RECSOUT,=P'1'
         B     UNZIPREC
EOF2     EQU   *
         CLC   UREC(L'TRLID),TRLID         CHECK FOR TRAILER IN CARD
         BE    EOF3                           2ND TO LAST CARD MUST
*                                              BE TRAILER
         WTO   'PACKER-CORRUPTED DDI FILE',ROUTCDE=(2,11)
         ABEND 0005
EOF3     EQU   *
         MVC   TRL,UREC                    MOVE TRAILER RECORD
         UNPK  TRLRECIN,RECSIN
         OI    TRLRECIN+6,X'F0'
         UNPK  TRLRECOT,RECSOUT
         OI    TRLRECOT+6,X'F0'
         MVC   ILN2IN,TRLRECIN
         MVC   ILN2OUT,TRLRECOT
         PUT   PRINTER,ILN2
EOJ      EQU   *
         CLOSE (PRINTER,,DDI,,DDO)
         L     13,4(,13)
         RETURN (14,12),RC=0
OHOH     DC    CL80'MISSMATCH'
         BR    R10
RECSIN   DC    PL5'0'
RECSOUT  DC    PL5'0'
DWORD    DS    D
ILRECL   DC    H'0'
IBLKSIZE DC    H'0'
IRECFM   DC    CL2'  '
IHLDRFM  DS    C
**
**   PRINT LINES
**
HLN1     DC    AL2(HLN1L)
         DC    AL2(0)
         DC    C'1PACKER - VERSION 1'
HLN1L    EQU   *-HLN1
**
ILN1     DC    AL2(ILN1L)
         DC    AL2(0)
         DC    CL19'0DDI INPUT   FILE: RECFM='
ILN1REC  DS    CL2
         DC    CL7' LRECL='
ILN1LREC DS    CL5
         DC    CL9' BLKSIZE='
ILN1BLK  DS    CL5
ILN1L    EQU   *-ILN1
**
**
ILN2     DC    AL2(ILN2L)
         DC    AL2(0)
         DC    CL20'0DDI RECORDS READ:'
ILN2IN   DS    CL7
         DC    CL23' DDO RECORDS WRITTEN:'
ILN2OUT  DS    CL7
ILN2L    EQU   *-ILN2
**
**
DLN1     DC    AL2(DLN1L)
         DC    AL2(0)
         DC    C' '
DLN1T    DS    CL80
DLN1L    EQU   *-DLN1
**
**
HDR      DS    0CL80
HDRID    DC    CL17'*UNPACKER HEADER '
HDRVER   DC    CL2'01'
         DC    CL1' '
HDRRECFM DS    CL2
         DC    CL1' '
HDRLRECL DS    CL5
         DC    CL1' '
HDRBLKSI DS    CL5
         DC    CL45' '
         DC    C'*'
TRL      DS    0CL80
TRLID    DC    CL18'*UNPACKER TRAILER'
TRLVER   DC    CL2'01'
         DC    CL1' '
TRLRECIN DS    CL7
         DC    CL1' '
TRLRECOT DS    CL7
         DC    CL43' '
         DC    C'*'
*
         DS    0D             FORCE DOUBLE WORD ALIGNMENT FOR SNAP
AREC     DS    CL80
OREC     DS    CL80
UREC     DS    CL160
         DS    CL1
UNTABLE  DC    C'0123456789ABCDEF'
PKTABLE  DC    X'FF0A0B0C0D0E0FFFFFFFFFFFFFFFFFFF' X'C0'-CF'
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' X'D0'-DF'
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' X'E0'-EF'
         DC    X'00010203040506070809FFFFFFFFFFFF' X'F0'-FF'
SAVEAREA DS    18F
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         PRINT NOGEN
DDI      DCB   DDNAME=DDI,MACRF=(GM),DSORG=PS
DDO      DCB   DDNAME=DDO,MACRF=(PM),DSORG=PS
PRINTER  DCB   DDNAME=SYSPRINT,MACRF=(PM),DSORG=PS,                    *
               RECFM=VBA,LRECL=125,BLKSIZE=1290
         DCBD  DSORG=PS,DEVD=DA
         PRINT GEN
         END   PACKER
./ ADD NAME=PL1UC
 PL1UC :  /* Convert PL/I source code from lower to upper case */
   PROCEDURE (PARMS) OPTIONS(MAIN) ;

     /****************************************************************
     * A programme which converts PL/I source code from lower to     *
     * upper case, and converts '^' and '¬' to hex 5F (not symbol)   *
     * and '|', '¦', and '×' to hex 4F (or symbol)                   *
     * optionally, %includes can be resolved and processed in this   *
     * new version.
     *****************************************************************
     *          Version : 3.2                                        *
     *             Date : January 3, 2022                            *
     *       Programmer : Ed Liss                                    *
     *         COMPUTER : IBM S/370 (HERCULES EMULATOR/WINDOWS 10)   *
     * Operating System : MVS 3.8j  (TurnKey System TK4-)            *
     *****************************************************************
     * GPL type of Copyright by Ed Liss 2022                         *
     ****************************************************************/

     /* original */

     /****************************************************************
     * A programme which converts PL/I source code from lower to     *
     * upper case, and converts '^' and '¬' to hex 5F (not symbol)   *
     * and '|', '¦', and '×' to hex 4F (or symbol)                   *
     *****************************************************************
     *          Version : 3.1a                                       *
     *             Date : April 16, 2017                             *
     *       Programmer : Markus Loew                                *
     *         Computer : IBM S/370 (Hercules Emulator/Windows 8.1)  *
     * Operating System : MVS 3.8j  (TurnKey System TK4-)            *
     *****************************************************************
     * GPL type of Copyright by Markus Loew 2017                     *
     ****************************************************************/
1

     /****************************************************************
     *    D E C L A R E    S P E C I F I C A T I O N S               *
     ****************************************************************/

     DECLARE

     ( ADDR ,
       SUBSTR ,
       TRANSLATE )                                BUILTIN ,

       PARMS                                      CHARACTER(100) ,
       IN_BUFF                                    CHARACTER(80) ,

     ( IN_CHAR ,
       PREVIOUS     INITIAL(' ') )                CHARACTER(1) ,

       P                                          POINTER ,

     ( NOT_X5F      BASED(P) /* -> hex 5F5F4F4F4F */ ,
       NOTS         INITIAL('¬^|¦×') )            CHARACTER(5) ,

       CAR_X5FB(5)  INITIAL((2)(1)'01011111'B , /* hex 5F5F */
                            (3)(1)'01001111'B ) /* hex 4F4F4F */
                    /* hex 5F5F */  UNALIGNED     BIT(8) ,

     ( CAPITAL      INITIAL ('ABCDEFGHIJKLMNOPQRSTUVWXYZ') ,
       SMALL        INITIAL ('abcdefghijklmnopqrstuvwxyz') )
                                                  CHARACTER(26) ,

     ((SYSUT1       INPUT  ,
       SYSUT2       OUTPUT )        RECORD ,
       SYSPRINT                     PRINT  )      FILE ,

     ((IS_COMMENT ,
       IS_STRING ,
       EOF ,
       CODE ,
       FALSE )      INITIAL('0'B) ,
       TRUE         INITIAL('1'B) )               BIT(1) ,

     ( IN_POS ,
       LINE_COUNT   INITIAL(0) ,
       BUFF_SIZE    INITIAL(80) ,
       START_POS    INITIAL(2) ,
       END_POS      INITIAL(72) )   FIXED         BINARY(31) ;


     /****************************************************************
     *    DECLARES ADDED TO SUPPORT PDSMEM                           *
     ****************************************************************/

     DECLARE
      (IS_INCLUDE,
       MODE_CLASSIC,
       MODE_PLUS,
       MAYBE_INCLUDE)                INITIAL('0'B) BIT(1),
       INCLUDE_POS                   INITIAL(0)  FIXED BINARY(31),
       HOLD_BUFF                     INITIAL((80)' ') CHARACTER(80);

     DECLARE
        1  PDSMEM_PARAMETERS   ALIGNED,
            2  PDSMEM_REQUEST_OPEN   INITIAL(0)  FIXED BINARY(31),
            2  PDSMEM_REQUEST_LOCATE INITIAL(4)  FIXED BINARY(31),
            2  PDSMEM_REQUEST_READ   INITIAL(8)  FIXED BINARY(31),
            2  PDSMEM_REQUEST_CLOSE  INITIAL(12) FIXED BINARY(31);
     DECLARE
        1   PDSMEM_REQUEST_1      STATIC,
             2  PDSMEM_REQUEST     FIXED BINARY(31),
        1   PDSMEM_MEMBER_1       STATIC,
             2  PDSMEM_MEMBER      CHAR(8),
        1   PDSMEM_RECORD80_1     STATIC,
             2  PDSMEM_RECORD80    CHAR(80),
        1   PDSMEM_RETURN_CODE_1  STATIC,
             2  PDSMEM_RETURN_CODE FIXED BINARY(31);
     DECLARE
        1  PDSMEM_RETURN_CODES    ALIGNED,
            2  PDSMEM_RC_NORMAL      INITIAL(0)  FIXED BINARY(31),
            2  PDSMEM_RC_NOT_FOUND   INITIAL(4)  FIXED BINARY(31),
            2  PDSMEM_RC_END_MEMBER  INITIAL(4)  FIXED BINARY(31),
            2  PDSMEM_RC_ERROR       INITIAL(8)  FIXED BINARY(31);

     DECLARE PAGE_NUMBER             INITIAL(0)  FIXED BINARY(31);
     ON ENDPAGE(SYSPRINT)
     BEGIN;
         IF PAGE_NUMBER > 0 THEN PUT FILE(SYSPRINT) PAGE;
         PAGE_NUMBER=PAGE_NUMBER+1;
         PUT FILE(SYSPRINT) EDIT('P L 1 U C   VERSION 3.2') (A)
                    ('PAGE ',PAGE_NUMBER) (COLUMN(80),A,P'999');
         PUT FILE(SYSPRINT) SKIP(2);
     END;
     SIGNAL ENDPAGE(SYSPRINT);

1    /****************************************************************
     *    M A I N    L I N E                                         *
     ****************************************************************/

     P = ADDR(CAR_X5FB(1)) ;

     OPEN FILE(SYSUT1) ,
          FILE(SYSUT2) ;
     ON ENDFILE(SYSUT1)
       EOF = TRUE ;
     CODE = INDEX(PARMS,'CODE')>0 ;
     MODE_CLASSIC = INDEX(PARMS,'CLASSIC')>0 ;                 /*V3.2*/
     MODE_PLUS = INDEX(PARMS,'PLUS')>0 ;                       /*V3.2*/
     IF MODE_PLUS THEN                                         /*V3.2*/
        MODE_CLASSIC=FALSE;                                    /*V3.2*/
     ELSE                                                      /*V3.2*/
        MODE_CLASSIC=TRUE;                                     /*V3.2*/
     CALL READ_SOURCE_LINE;
     DO WHILE(EOF=FALSE) ;
       LINE_COUNT = LINE_COUNT+1 ;
       DO IN_POS = START_POS TO END_POS ;
         IN_CHAR = SUBSTR(IN_BUFF,IN_POS,1) ;
         IF CODE THEN
           ;
         ELSE
           SUBSTR(IN_BUFF,IN_POS,1) = TRANSLATE(IN_CHAR,NOT_X5F,NOTS) ;
         IF IS_STRING THEN
           IF IN_CHAR='''' THEN
             IS_STRING = FALSE ;
           ELSE ;
         ELSE
           IF IS_COMMENT THEN
             IF PREVIOUS='*' & IN_CHAR='/' THEN
             DO;
               IS_COMMENT = FALSE ;
             END;
             ELSE ;
           ELSE
             IF IN_CHAR='''' THEN
               IS_STRING = TRUE ;
             ELSE
               IF PREVIOUS='/' & IN_CHAR='*' THEN
               DO;
                 IS_COMMENT = TRUE ;
               END;
               ELSE
               DO;                                             /*V3.2*/
                 SUBSTR(IN_BUFF,IN_POS,1) =
                  TRANSLATE(TRANSLATE(IN_CHAR,NOT_X5F,NOTS),
                                              CAPITAL,SMALL) ;
                 IF IN_CHAR='%' THEN                           /*V3.2*/
                    MAYBE_INCLUDE=TRUE;                        /*V3.2*/
               END;                                            /*V3.2*/
         PREVIOUS = IN_CHAR ;
       END ;
     /****************************************************************
     *    START OF CODE ADDED TO SUPPORT PROCESSING OF INCLUDES      *
     ****************************************************************/
       IF MAYBE_INCLUDE THEN
       DO;
         IF IS_STRING | IS_COMMENT THEN ;
         ELSE
         DO;
           INCLUDE_POS=INDEX(IN_BUFF,'%');
           IF INCLUDE_POS > 0  & MODE_PLUS THEN                /*V3.2*/
              CALL PROCESS_INCLUDE;
         END;
       END;
       MAYBE_INCLUDE=FALSE;
     /****************************************************************
     *      END OF CODE ADDED TO SUPPORT PROCESSING OF INCLUDES      *
     ****************************************************************/
       WRITE FILE(SYSUT2) FROM(IN_BUFF) ;
       PUT FILE(SYSPRINT) SKIP
        EDIT(LINE_COUNT,' <',IN_BUFF,'>')(F(6),3 A) ;
       CALL READ_SOURCE_LINE;
     END ; /* MAIN LINE LOOP */

     CLOSE FILE(SYSUT1) ,
           FILE(SYSUT2) ;

1    /****************************************************************
     *    START OF CODE ADDED TO SUPPORT PROCESSING OF INCLUDES      *
     ****************************************************************/
0  PROCESS_INCLUDE:PROC;
   /*****************************************************************
   *                                                                *
   *   FROM: PL/1 (F) LANGUAGE REFERENCE MANUAL  (PAGE 389)         *
   *         GC28-8201-2  October 1969                              *
   *                                                                *
   *   % [label:]... INCLUDE {ddname-1 (member-name-1)}             *
   *                         {member-name-1}                        *
   *                        ,ddname-n (member-name-n)               *
   *                        ,member-name-n                          *
   *                         ;                                      *
   *                                                                *
   *                                                                *
   *                                                                *
   *  INCLUDES MAY BE FOUND WITH CODE BEFORE OR AFTER THEM ON THE   *
   *  SAME LINE.                                                    *
   *     EXAMPLE:                                                   *
   *        X=0;  %INCLUDE XRTN;   IF X>0 THEN                      *
   *  THE BEFORE AND AFTER CODE MUST BE PRESERVED.                  *
   *                                                                *
   *  FIRST, THE CODE BEFORE THE % MUST BE WRITTEN TO SYSUT2        *
   *  SECOND, THE %INCLUDE PARSED AND THE COPIED USING PDSMEMP      *
   *  THIRD, THE CODE AFTER THE ; MUST BE WRITTEN TO SYSUT2         *
   *                                                                *
   *  ASSUMPTIONS MADE: -THERE WILL BE NO MORE THAN 1 INCLUDE       *
   *                     PER LINE                                   *
   *                    -LABEL OPTION WILL BE IGNORED               *
   *                    -INCLUDES WILL BE CONTAINED ON 1 LINE       *
   *                    -INCLUDE SYNTAX IS %INCLUDE MEMBER;         *
   *                      OR %INCLUDE DDNAME(MEMBER);               *
   *                    -SPACES ARE IGNORED WITHIN INCLUDES         *
   *                    -ALL INCLUDES WILL BE PROCESSED.            *
   *                     CONDITIONAL INCLUDES ARE NOT SUPPORTED.    *
   *                    -NO OTHER PREPROCESSOR INSTRUCTIONS WILL    *
   *                     BE PROCESSED.                              *
   *                                                                *
   *****************************************************************/

     DECLARE CODE_SEGMENT              CHAR(80),
             MEMBER_NAME               CHAR(8) INITIAL((8)' '),
             DDNAME                    CHAR(8) INITIAL((8)' '),
             A_WORD                    CHAR(80) VARYING,
             GENERATED_LINE            CHAR(80) INITIAL((80)' ');

     DECLARE (POS_SEMI,
              POS_RPAREN,
              NEXT_POS,
              MEM_LEN)                 FIXED BINARY(31);

     SUBSTR(IN_BUFF,END_POS+1) = ' ';   /* BLANK OUT SEQ NUMBER */

     CALL GET_NEXT_WORD(INCLUDE_POS+1,A_WORD,NEXT_POS);

     IF SUBSTR(A_WORD,1,7) = 'INCLUDE' THEN
     DO;
        GENERATED_LINE = ' /****************START INSERT BY PL1UC****';
        WRITE FILE(SYSUT2) FROM(GENERATED_LINE);
        WRITE FILE(SYSUT2) FROM(IN_BUFF);
        GENERATED_LINE = ' *******************END INSERT BY PL1UC***/';
        WRITE FILE(SYSUT2) FROM(GENERATED_LINE);
     END;

     IF INCLUDE_POS > START_POS THEN
     DO;
        CODE_SEGMENT = SUBSTR(IN_BUFF,1,INCLUDE_POS-START_POS+1);
        IF CODE_SEGMENT = (80)' ' THEN;
        ELSE
           WRITE FILE(SYSUT2) FROM(CODE_SEGMENT);
    /*  IN_BUFF=SUBSTR(IN_BUFF,INCLUDE_POS);
        INCLUDE_POS=1; */
     END;

     IF SUBSTR(A_WORD,1,7) = 'INCLUDE' THEN
     DO;
        IF SUBSTR(A_WORD,LENGTH(A_WORD),1) = ';' THEN
        DO;
           DDNAME = 'PL1LIB  ';
           MEM_LEN = LENGTH(A_WORD)-8;
           IF MEM_LEN < 1 | MEM_LEN > 8 THEN
           DO;
              PUT FILE(SYSPRINT) SKIP LIST('INVALID MEMBER NAME');
              PUT FILE(SYSPRINT) SKIP LIST(IN_BUFF);
              STOP;
           END;
           MEMBER_NAME = SUBSTR(A_WORD, 8, MEM_LEN);
           CALL PDSMEMP_INIT;
           RETURN;
        END;
        IF SUBSTR(A_WORD,LENGTH(A_WORD),1) = '(' THEN
        DO;
           MEM_LEN = LENGTH(A_WORD)-8;
           IF MEM_LEN < 1 | MEM_LEN > 8 THEN
           DO;
              PUT FILE(SYSPRINT) SKIP LIST('INVALID DDNAME');
              PUT FILE(SYSPRINT) SKIP LIST(IN_BUFF);
              STOP;
           END;
           DDNAME = SUBSTR(A_WORD, 8, MEM_LEN);
           CALL GET_NEXT_WORD(NEXT_POS,A_WORD,POS_RPAREN);
           IF SUBSTR(A_WORD,LENGTH(A_WORD),1) = ')' THEN
           DO;
              MEM_LEN = LENGTH(A_WORD)-1;
              IF MEM_LEN < 1 | MEM_LEN > 8 THEN
              DO;
                 PUT FILE(SYSPRINT) SKIP LIST('INVALID MEMBER NAME');
                 PUT FILE(SYSPRINT) SKIP LIST(IN_BUFF);
                 STOP;
              END;
              MEMBER_NAME = SUBSTR(A_WORD, 1, MEM_LEN);
           END;
           CALL GET_NEXT_WORD(POS_RPAREN,A_WORD,POS_SEMI);
           IF SUBSTR(A_WORD,1,1) = ';' THEN ;
           ELSE
              IF SUBSTR(A_WORD,1,1) = ',' THEN
              DO;
                 PUT FILE(SYSPRINT) SKIP
                         LIST('ONLY 1 MEMBER PER INCLUDE');
                 PUT FILE(SYSPRINT) SKIP LIST(IN_BUFF);
                 STOP;
              END;
              ELSE
              DO;
                 PUT FILE(SYSPRINT) SKIP
                          LIST('SYNTAX ERROR IN INCLUDE');
                 PUT FILE(SYSPRINT) SKIP LIST(IN_BUFF);
                 STOP;
              END;
           CALL PDSMEMP_INIT;
           RETURN;
        END;
        IF SUBSTR(A_WORD,LENGTH(A_WORD),1) = ',' THEN
        DO;
           PUT FILE(SYSPRINT) SKIP
                   LIST('ONLY 1 MEMBER PER INCLUDE');
           PUT FILE(SYSPRINT) SKIP LIST(IN_BUFF);
           STOP;
        END;
     END;

   PDSMEMP_INIT:PROC;
     PDSMEM_REQUEST     = PDSMEM_REQUEST_OPEN;
     PDSMEM_MEMBER      = DDNAME;
     PDSMEM_RECORD80    = (80)' ';
     CALL CALL_GETMEMP;
     IF PDSMEM_RETURN_CODE > 0 THEN
     DO;
        PUT FILE(SYSPRINT)
            SKIP LIST('PROBLEM OPENING DDNAME ',DDNAME);
        STOP;
     END;
     PDSMEM_REQUEST     = PDSMEM_REQUEST_LOCATE;
     PDSMEM_MEMBER      = MEMBER_NAME;
     PDSMEM_RECORD80    = (80)' ';
     CALL CALL_GETMEMP;
     IF PDSMEM_RETURN_CODE > 0 THEN
     DO;
        PUT FILE(SYSPRINT)
            SKIP LIST('PROBLEM LOCATING MEMBER ',MEMBER_NAME);
        STOP;
     END;
     IS_INCLUDE=TRUE;
  /* IF HOLD_BUFF = (80)' ' THEN ;
     ELSE
        CALL READ_SOURCE_LINE;
  */ PUT FILE(SYSPRINT) SKIP EDIT('START OF INCLUDE FROM ') (A)
                                 (DDNAME,'(')               (A,A)
                                 (MEMBER_NAME,')')          (A,A);
     IN_BUFF = ' ';
   END PDSMEMP_INIT;
   END PROCESS_INCLUDE;

   READ_SOURCE_LINE:PROC;
     IF IS_INCLUDE THEN
     DO;
        PDSMEM_REQUEST = PDSMEM_REQUEST_READ;
        CALL CALL_GETMEMP;
        IF PDSMEM_RETURN_CODE = PDSMEM_RC_NORMAL THEN
            IN_BUFF=PDSMEM_RECORD80;
        ELSE
        IF PDSMEM_RETURN_CODE = PDSMEM_RC_END_MEMBER THEN
        DO;
           PUT FILE(SYSPRINT) SKIP LIST('END OF INCLUDE');
           IS_INCLUDE=FALSE;
           PDSMEM_REQUEST     = PDSMEM_REQUEST_CLOSE;
           PDSMEM_MEMBER      = (8)' ';
           PDSMEM_RECORD80    = (80)' ';
           CALL CALL_GETMEMP;
           IF PDSMEM_RETURN_CODE > 0 THEN
           DO;
              PUT FILE(SYSPRINT)
                  SKIP LIST('PROBLEM CLOSING DDNAME ',DDNAME);
              STOP;
           END;
        END;
        ELSE
        DO;
           PUT FILE(SYSPRINT) SKIP LIST
                   ('PROBLEM READING PDSMEM, RC=',PDSMEM_RETURN_CODE);
           STOP;
        END;
        IN_BUFF=PDSMEM_RECORD80;
     END;
     IF IS_INCLUDE THEN;
     ELSE
        IF HOLD_BUFF = (80)' ' THEN
           READ FILE(SYSUT1) INTO(IN_BUFF) ;
        ELSE
        DO;
           IN_BUFF = HOLD_BUFF;
           HOLD_BUFF = (80)' ';
        END;
   END READ_SOURCE_LINE;
   CALL_GETMEMP:PROC;
       CALL GETMEMP (PDSMEM_REQUEST_1,
                     PDSMEM_MEMBER_1,
                     PDSMEM_RECORD80_1,
                     PDSMEM_RETURN_CODE_1);
   END CALL_GETMEMP;
   GET_NEXT_WORD:PROC(FIRST,RET,NEXT_CH);
       DECLARE (FIRST,
                DELIM,
                NEXT_CH,
                NEXT_CHAR)   FIXED BINARY(31);
       DECLARE RET           CHAR(80) VARYING;
       DECLARE A_WORD        CHAR(80) VARYING;
       DECLARE A_CHAR        CHAR(1);

       A_WORD = '';
       NEXT_CHAR = FIRST;
       DELIM = 0;
       DO WHILE(DELIM < 2 & NEXT_CHAR < END_POS);
          A_CHAR = SUBSTR(IN_BUFF,NEXT_CHAR,1);
          DELIM = INDEX(' ;(),',A_CHAR);
          IF DELIM = 1 THEN ;
          ELSE
             A_WORD = A_WORD || A_CHAR;
          NEXT_CHAR = NEXT_CHAR + 1;
       END;
       RET = A_WORD;
       NEXT_CH = NEXT_CHAR;
    END GET_NEXT_WORD;
     /****************************************************************
     *      END OF CODE ADDED TO SUPPORT PROCESSING OF INCLUDES      *
     ****************************************************************/
   END PL1UC;
./ ADD NAME=PL1UF
//PL1UF  PROC  SOUT='*',MODE='CLASSIC'
//PL1U   EXEC  PGM=PL1UC,PARM='&MODE'
//STEPLIB  DD  DSN=SYS1.PL1LIB,DISP=SHR
//SYSPRINT DD  SYSOUT=&SOUT
//SYSUT1   DD  DDNAME=SYSIN
//SYSUT2   DD  DSN=&PL1OUT,DISP=(MOD,PASS),UNIT=SYSSQ,                 *
//             SPACE=(TRK,(20,20),RLSE),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)
//PL1LIB   DD  DUMMY
./ ADD NAME=PL1UFC
//PL1UFC PROC  SOUT='*',MODE='CLASSIC'
//PL1U   EXEC  PGM=PL1UC,PARM='&MODE'
//STEPLIB  DD  DSN=SYS1.PL1LIB,DISP=SHR
//SYSPRINT DD  SYSOUT=&SOUT,DCB=(BLKSIZE=1330,LRECL=133,RECFM=FBA)
//PL1LIB   DD  DUMMY
//SYSUT2   DD  DSN=&&SOURCE,DCB=(BLKSIZE=3120,LRECL=80,RECFM=FB),
//    DISP=(,PASS),SPACE=(3120,(10,10),RLSE),UNIT=SYSDA
//SYSUT1   DD  DDNAME=SYSIN
//PL1L   EXEC  PGM=IEMAA,PARM='LOAD,NODECK'
//SYSPRINT DD  SYSOUT=&SOUT
//SYSLIN   DD  DSNAME=&&LOADSET,DISP=(MOD,PASS),UNIT=SYSSQ,            *
//             SPACE=(80,(250,100))
//SYSUT3  DD  DSNAME=&&SYSUT3,UNIT=SYSDA,SPACE=(80,(250,250)),         *
//             DCB=BLKSIZE=80
//SYSUT1   DD  DSNAME=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(60,60),,CONTIG),*
//             SEP=(SYSUT3,SYSLIN),DCB=BLKSIZE=1024
//SYSIN    DD  DSN=&&SOURCE,DISP=(OLD,DELETE)
./ ADD NAME=PL1UFCG
//PL1UFC  PROC SOUT='*',MODE='CLASSIC'
//PL1U   EXEC  PGM=PL1UC,PARM='&MODE'
//STEPLIB  DD  DSN=SYS1.PL1LIB,DISP=SHR
//SYSPRINT DD  SYSOUT=&SOUT,DCB=(BLKSIZE=1330,LRECL=133,RECFM=FBA)
//PL1LIB   DD  DUMMY
//SYSUT2   DD  DSN=&&SOURCE,DCB=(BLKSIZE=3120,LRECL=80,RECFM=FB),
//    DISP=(,PASS),SPACE=(3120,(10,10),RLSE),UNIT=SYSDA
//SYSUT1   DD  DDNAME=SYSIN
//PL1L   EXEC  PGM=IEMAA,PARM='LOAD,NODECK'
//SYSPRINT DD  SYSOUT=&SOUT
//SYSLIN   DD  DSNAME=&&LOADSET,DISP=(MOD,PASS),UNIT=SYSSQ,            *
//             SPACE=(80,(250,100))
//SYSUT3  DD  DSNAME=&&SYSUT3,UNIT=SYSDA,SPACE=(80,(250,250)),         *
//             DCB=BLKSIZE=80
//SYSUT1   DD  DSNAME=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(60,60),,CONTIG),*
//             SEP=(SYSUT3,SYSLIN),DCB=BLKSIZE=1024
//SYSIN    DD  DSN=&&SOURCE,DISP=(OLD,DELETE)
//GO     EXEC  PGM=LOADER,PARM='MAP,PRINT',                            *
//             COND=(9,LT,PL1L)
//SYSLIB   DD  DSNAME=SYS1.PL1LIB,DISP=SHR
//SYSLIN   DD  DSNAME=&&LOADSET,DISP=(OLD,DELETE)
//SYSLOUT  DD  SYSOUT=&SOUT
//SYSPRINT DD  SYSOUT=&SOUT
./ ADD NAME=PL1UFCL
//PL1UFCL PROC SOUT='*',MODE='CLASSIC'
//PL1U   EXEC  PGM=PL1UC,PARM='&MODE'
//STEPLIB  DD  DSN=SYS1.PL1LIB,DISP=SHR
//SYSPRINT DD  SYSOUT=&SOUT,DCB=(BLKSIZE=1330,LRECL=133,RECFM=FBA)
//PL1LIB   DD  DUMMY
//SYSUT2   DD  DSN=&&SOURCE,DCB=(BLKSIZE=3120,LRECL=80,RECFM=FB),
//    DISP=(,PASS),SPACE=(3120,(10,10),RLSE),UNIT=SYSDA
//SYSUT1   DD  DDNAME=SYSIN
//PL1L   EXEC  PGM=IEMAA,PARM='LOAD,NODECK'
//SYSPRINT DD  SYSOUT=&SOUT
//SYSLIN   DD  DSNAME=&&LOADSET,DISP=(MOD,PASS),UNIT=SYSSQ,            *
//             SPACE=(80,(250,100))
//SYSUT3  DD  DSNAME=&&SYSUT3,UNIT=SYSDA,SPACE=(80,(250,250)),         *
//             DCB=BLKSIZE=80
//SYSUT1   DD  DSNAME=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(60,60),,CONTIG),*
//             SEP=(SYSUT3,SYSLIN),DCB=BLKSIZE=1024
//SYSIN    DD  DSN=&&SOURCE,DISP=(OLD,DELETE)
//LKED   EXEC  PGM=IEWL,PARM='XREF,LIST',COND=(9,LT,PL1L)
//SYSLIB   DD  DSNAME=SYS1.PL1LIB,DISP=SHR
//SYSLMOD  DD  DSNAME=&&GOSET(GO),DISP=(MOD,PASS),                     *
//             UNIT=SYSDA,SPACE=(1024,(50,20,1),RLSE)
//SYSUT1   DD  DSNAME=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(200,20)),       *
//             SEP=(SYSLMOD,SYSLIB),DCB=BLKSIZE=1024
//SYSPRINT DD  SYSOUT=&SOUT
//SYSLIN   DD  DSNAME=&&LOADSET,DISP=(OLD,DELETE)
//         DD  DDNAME=SYSIN
./ ADD NAME=PL1UFCLG
//PL1UFCLG PROC SOUT='*',MODE='CLASSIC'
//PL1U   EXEC  PGM=PL1UC,PARM='&MODE'
//STEPLIB  DD  DSN=SYS1.PL1LIB,DISP=SHR
//SYSPRINT DD  SYSOUT=&SOUT,DCB=(BLKSIZE=1330,LRECL=133,RECFM=FBA)
//PL1LIB   DD  DUMMY
//SYSUT2   DD  DSN=&&SOURCE,DCB=(BLKSIZE=3120,LRECL=80,RECFM=FB),
//    DISP=(,PASS),SPACE=(3120,(10,10),RLSE),UNIT=SYSDA
//SYSUT1   DD  DDNAME=SYSIN
//PL1L   EXEC  PGM=IEMAA,PARM='LOAD,NODECK'
//SYSPRINT DD  SYSOUT=&SOUT
//SYSLIN   DD  DSNAME=&&LOADSET,DISP=(MOD,PASS),UNIT=SYSSQ,            *
//             SPACE=(80,(250,100))
//SYSUT3  DD  DSNAME=&&SYSUT3,UNIT=SYSDA,SPACE=(80,(250,250)),         *
//             DCB=BLKSIZE=80
//SYSUT1   DD  DSNAME=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(60,60),,CONTIG),*
//             SEP=(SYSUT3,SYSLIN),DCB=BLKSIZE=1024
//SYSIN    DD  DSN=&&SOURCE,DISP=(OLD,DELETE)
//LKED   EXEC  PGM=IEWL,PARM='XREF,LIST',COND=(9,LT,PL1L)
//SYSLIB   DD  DSNAME=SYS1.PL1LIB,DISP=SHR
//SYSLMOD  DD  DSNAME=&&GOSET(GO),DISP=(MOD,PASS),                     *
//             UNIT=SYSDA,SPACE=(1024,(50,20,1),RLSE)
//SYSUT1   DD  DSNAME=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(200,20)),       *
//             SEP=(SYSLMOD,SYSLIB),DCB=BLKSIZE=1024
//SYSPRINT DD  SYSOUT=&SOUT
//SYSLIN   DD  DSNAME=&&LOADSET,DISP=(OLD,DELETE)
//         DD  DDNAME=SYSIN
//GO     EXEC  PGM=*.LKED.SYSLMOD,COND=((9,LT,LKED),(9,LT,PL1L))
//STEPLIB  DD  DSNAME=SYS1.PL1LIB,DISP=SHR
//SYSPRINT DD  SYSOUT=&SOUT
./ ADD NAME=TESTER
 exer01 :
   procedure (parm) options (main) ;

     declare
       parm          character(100) ,
       mod           builtin ,
       sysprint      file print ,
       hello         character(13) initial('Just for fun!') ,
       is_prim(1000) bit(1) unaligned initial((1000)(1)'1'b) ,
     ( i ,
       j ,
       n )           binary (31) fixed initial (0) ;

     open file (sysprint) linesize(130) pagesize(63) ;

     PUT FILE (SYSPRINT) LIST ('TESTING PROCEDURE '||
                                substr(parm,1,index(parm,'.'))) ;
     put file (sysprint) skip(2)
                         list ('Sieve of Eratosthenes 1 to 1000') ;
     put file (sysprint) skip(2) ;
     %include test2;
     put file (sysprint) skip(2) list (n,' Prime Numbers Found') ;

     put file (sysprint) skip(2) list ('*** Happy New Year ***') ;
     put file (sysprint) skip(3) edit((i,hello,substr(hello,1,(i<50)*13)
       do i = 50,40 to 20 by -2,50,50,50,50))(a,column(i),a,
         column(100-i+(i=50)*20),a,skip) ;

   end exer01 ;
./ ADD NAME=TEST2
     do i = 2 to 31 ;
       if is_prim(i) then
         do j = i*i to 1000 by i ;
           is_prim(j) = ^'1'b ;
         end ;
     end ;
     do i = 2 to 1000 ;
       if ^is_prim(i) then
         do ;
           put file(sysprint) edit(i)(column(mod(n,25)*5+1),f(5)) ;
           n = n+1 ;
         end ;
     end ;
./ ENDUP
$$
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
//PL1L.SYSLMOD DD DCB=(SYS2.LINKLIB),UNIT=3350,
//      SPACE=(TRK,(10,10,1),RLSE)
//LKED.SYSIN DD DSN=&&GETMEM,DISP=(OLD,DELETE)
//********************************************************************
//*                                                                  *
//*  -IF ALL STEPS COMPLETED WITH CC 0, UPDATE THE SYSTEM DATASETS   *
//*   SYS2.LINKLIB AND SYS2.PROCLIB.  NOTE - COMPRESS IS ***NOT***   *
//*   RECOMENDED UNLESS NO OTHER ACTIVITY IS GOING ON.               *
//*                                                                  *
//********************************************************************
//STEP4  EXEC PGM=IEBCOPY,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//PROCIN   DD DSN=HERC01.PL1UC.INSTALL,DISP=SHR
//PROCOUT  DD DSN=SYS2.PROCLIB,DISP=SHR
//LOADIN   DD DSN=&&GOSET,DISP=(OLD,PASS)
//LOADOUT  DD DSN=SYS2.LINKLIB,DISP=SHR
//SYSIN    DD *
 COPY INDD=PROCIN,OUTDD=PROCOUT
 SELECT MEMBER=((PL1UF,,R))
 SELECT MEMBER=((PL1UFC,,R))
 SELECT MEMBER=((PL1UFCG,,R))
 SELECT MEMBER=((PL1UFCL,,R))
 SELECT MEMBER=((PL1UFCLG,,R))
 COPY INDD=LOADIN,OUTDD=LOADOUT
 SELECT MEMBER=((GO,PL1UC,R))
//*
