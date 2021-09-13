     **Free
      *************************************************************
      *   NAME     :   INSU_400
      *   AUTHOR   :   PROGRAMMERS.IO
      *   PROJECT  :   INSURANCE MANAGEMENT SYSTEM
      *   DATE     :   31/08/2021 (DD/MM/YYYY)
      *************************************************************
      *
      *======================Header===============================*
      *
          CTL-OPT OPTION(*NODEBUGIO);
                 //DFTACTGRP(*No);
      *
      *================Display File Declaration===================*
      *
          DCL-F login_scr Workstn;
          DCL-F Agent_scr Workstn INDDS(Indds2) Sfile(A_C_SFL:RRN)
                                                sfile(A_polsfl:rrn1);
      *=================Physical File Declaration=================*
      *
          DCL-F Agent_mast keyed usage(*update:*output:*delete);
          DCL-F Cust_mast keyed usage(*update:*output:*delete);
          DCL-F login_pf keyed usage(*update:*output:*delete);
          DCL-F Policy_Dtl keyed usage(*update:*output:*delete);
      *
      *=================Procedure Define==========================*
      *
        //DCL-PR Insu_400 EXTPGM;
        //DCL-PI;
        //DCL Usrid2 char(10);
        //Dcl_s usrids Char(10);
        //ENd_PR;
     DInsu_400         pr                  EXTPGM('INSU_400')
     dpointer          pi
     DUsrid2                         10a
     DUsrids           s             10a
      *
      *=================Indicator Data Structure==================*
      *
            DCL-DS Indds2;
               Key_Exit INd pos(03);
               Key_Back Ind pos(12);
      *
      *=================Subfile Indicator=========================*
      *
               Clr_In   Ind Pos(43);
               Dsp_In   Ind Pos(41);
               End_In   Ind Pos(44);
               DspCtl_In  Ind Pos(42);
            End-Ds;
      *=================Variable Declaration======================*
      *
            DCL-S RRN zoned(4:0);
            DCL-S RRN1 zoned(4:0);
            DCL-S D_POsnam1 packed(7:0);
            DCL-s D_fstnam char(15);
      *
      *=================Main program==============================*
      *
                Dow Key_Exit = *off;
                    usrids = Usrid2;
                    Chain D_usrid Login_pf;
      *
                If %found(Login_Pf);
                    Chain usr_id Agent_mast;
                If %found(Agent_mast);
                     D_posnam1 = %scan(' ':Agen_name:1);
                     D_fstnam  = %subst(Agen_name:1:D_posnam1);
                     D_AUSR   = D_Fstnam;
                Endif;
                Endif;
      *

                ExFmt A_hscr;
      *
      *
                   A_errmsg = *blanks;
                   ACom_line = %trim(ACom_Line);
      *
                Select;

                When  Key_back = *on;
                   Key_back = *off;
                Leave;

                When  Acom_line = '1';
                   Exsr $VC_dtlRES;

                When  Acom_line = '2';
                   Exsr $VA_poldtl;

                Other;
                   A_errmsg ='Wrong Option Selection';
                Endsl;

                Enddo;
                *Inlr = *ON;
      *
      *==============View customer detail and registration==========*
      *
                Begsr $VC_dtlRes;
                   Exsr  $Clear;
                   Exsr  $load;
                   Exsr  $Disply;
                Endsr;
      *
      *==============Clear Subroutine==============================*
      *
                Begsr $Clear;
                   clr_In = *on;
                   Clear RRN;
                   Write A_C_ctl;
                   Clr_in = *Off;
                EndSr;
      *
      *==============Load Subroutine===============================*
      *
                BegSr $Load;
                   Setll *loval Cust_mast;
                   Read  Cust_mast;
                Dow Not %Eof(Cust_Mast);
                   RRN = RRN + 1;
                   Write  A_C_Sfl;
                   Read  Cust_Mast;
                Enddo;
                If  RRN <= 9999;
                   Dsp_in = *on;
                Else;
                   Dsp_in = *Off;
                EndIf;
                EndSr;
      *
      *==============Display subroutine=======================*
      *
                BegSr $Disply;
                   End_In = *on;
                   dspctl_In = *on;
                   Write Footer;
                Dow Key_Exit = *off;
                   clear Acom_line;
                   ExFmt A_C_Ctl;
              //If key_add = *on;
                 //Exsr $Add_Cus;
              //Endif;
                IF Key_Back = *oN;
                   Key_Back = *off;
                Leave;
                Endif;
                Enddo;
                EndSr;
      *==================Add Customer registration=============*
      *
         //Begsr $Add_Cus;
           //DCL-PR Reg_Cus;

           //dcl-pi;
           //dcl-e  reg_cus;
           //End-Pr;

         //EndSr;
      *==================View Policy Details===================*
      *
             Begsr $VA_poldtl;
                ExSr Clear2;
                ExSr load2;
                ExSr display2;
             EndSr;
      *
      *=================Clear 2 subroutine=====================*
      *
             BegSr Clear2;
                clr_In = *on;
                Clear RRN1;
                Write A_polctl;
                Clr_in = *Off;
             EndSr;
      *
      *=================load 2 subroutine=====================*
      *
                BegSr  Load2;
                   Setll *loval Policy_dtl;
                   Read  Policy_dtl;
                Dow Not %Eof(Policy_dtl);
                   RRN1 = RRN1 + 1;
                   Write  A_POLSFL;
                   Read  Policy_Dtl;
                Enddo;
                If  RRN1 <= 9999;
                   Dsp_in = *on;
                Else;
                   Dsp_in = *Off;
                EndIf;
                EndSr;
      *==============Display subroutine=======================*
      *
                BegSr  Display2;
                   End_In = *on;
                   dspctl_In = *on;
                   Write Footer;
                Dow Key_Exit = *off;
                   clear Acom_line;
                   ExFmt A_POLCTL;
                   ReadC A_PolSFl;
                DOW Not %EOf(Agent_scr);
                IF  A_popt = 5;
                   Exsr $V_Apoldtl;
                Endif;
                   Readc A_polSfl;
                Enddo;
                IF    Key_Back = *oN;
                   Key_Back = *off;
                Leave;
                Endif;
                Enddo;
                EndSr;
      *=============Display policy detail Subroutine==========*
      *
                BegSr $V_Apoldtl;
                Dow Key_exit =*off;
                   ExFmt D_APOLDtl;
                If Key_back =*on;
                   Key_back =*off;
                Leave;
                Endif;
                Enddo;
                EndSr;
