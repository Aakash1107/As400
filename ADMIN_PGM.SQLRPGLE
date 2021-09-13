     **Free
      **================HEADER=================================**
      *
     HOption(*Nodebugio)
      *
      **================DISPLAY FILE DECLARATION===============**
      *
     FAdmin_scr CF   E             WORKSTN Sfile(AD_AGENSFL:RRN)
     F                                     INDDS(INDDS3)
      *
      **================PROCEDURE DECLARATION==================**
      *
     DINSU_400         PR                  EXTPGM('INSU_400')
     DPOINTER          PI
     DUsridds                        10a
     DUsridds1         s             10a
      *================Indicator Data Structure===================*
      *
     D INDDS3          DS
     D Key_Exit                       1N   Overlay(INDDS3:03)
     D Key_Back                       1N   Overlay(INDDS3:12)
     D Rev_img                        1N   Overlay(INDDS3:21)
      *
      *===========Subfile indicators=======================*
      *
     D Clr_in                         1N   Overlay(INDDS3:53)
     D Dsp_in                         1N   Overlay(INDDS3:51)
     D End_in                         1N   Overlay(INDDS3:54)
     D Dspctl_in                      1N   Overlay(INDDS3:52)
      *
      *===========Qualified Data Structure=================*
      *
     DDs1              DS                  QUALIFIED
     D AGEN_ID1                       5s 0
     D AGEN_NAME                     15A
     D AGEN_CON                      10s 0
     D AGEN_CTY                      15A
      *
      *===========Variable Declaration=====================*
      *
     D RRN             s              4s 0
      **================MAIN PROGRAMM==========================**
                 Dow Key_Exit = *off;
                    usridds1 = usridds;
                    AD_nam = 'AAKASH';
                    clear  AD_comline;
                    Exfmt AD_Hscr;
                    AD_comline = %trim(AD_comline);
                 If  Key_Back = *on;
                     Key_Back = *off;
                 Leave;
                 Endif;
                     Select;
                 When  AD_comline = '1';
                     ExSr $AD_Agendtl;
               //When  AD_comline = '2';
                   //ExSr $AD_Custdtl;
                 Endsl;
                 Enddo;
                     *inlr = *on;
      *
      *===============Manage Agent Details=======================**
      *
                 BegSr $AD_Agendtl;
                    Exsr Clear;
                    Exsr Load;
                    Exsr Display;
                 EndSr;
      *
      *===============Clear Subroutine for Agent Detail==========**
      *
                 BegSr Clear;
                    Clr_in = *on;
                    Clear   rrn;
                    Write   AD_AGENctl;
                    Clr_in = *oFf;
                 EndSr;
      *===============Load Subroutine for Agent Detail==========**
      *
                 BegSr Load;
                    Exec Sql Set Option Commit = *None;
                    Exec Sql Declare C_Agent Cursor for
                       Select AGEN_ID1,AGEN_NAME,AGEN_CON,AGEN_CTY
                            from Agent_mast;
                    Exec Sql Open C_Agent;
                    Exec Sql Fetch Next from C_Agent into :ds1;
                 Dow Sqlcode = 0;
                     AGEN_ID1 = ds1.AGEN_ID1;
                     AGEN_NAME = ds1.AGEN_NAME;
                     AGEN_CON =  ds1.AGEN_CON;
                     AGEN_cty =  ds1.AGEN_CTY;
                     RRN = RRn + 1;
                     Write AD_AGENSFL;
                     Exec Sql Fetch Next from C_Agent into :ds1;
                 Enddo;
                     Exec Sql close C_agent;
                 EndSr;
      *=====================Display Subroutine for Agent Details=======*
      *
                 BegSr Display;
                    Dsp_in  = *on;
                    End_in   = *on;
                    Dspctl_in  = *on;
                    Dow Key_exit = *off;
                    Write Footer;
                    Exfmt AD_AGENCTL;
                    Readc AD_AGENSFL;
                    Dow Not %eof(Admin_scr);
                    select;
                 When AD_Aopt = 2;
                    Exsr $UpDAgen;
               //When AD_Aopt = 5;
                  //Exsr $DspAgen;
                    other;
                    Endsl;
                 If Key_back =*on;
                    Key_back =*off;
                    Leave;
                    Endif;
                    Enddo;
                    Enddo;
                 EndSr;
      *
      *____________________Update Agent Details_____________________*
      *
              BegSr $Updagen;
                 Dow Key_exit = *Off;
                    Exfmt UPD_AGEN;
                    Rev_img = *off;
                    Select;
                 When Agen_name = *blanks;
                      Rev_img = *on;
                      Other;
                 EndSl;
                 If Key_Back = *on;
                    Key_Back = *off;
                    Leave;
                 EndIf;
                 EndDo;
              EndSr;
