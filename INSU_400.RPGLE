      *************************************************************
      *   NAME     :   INSU_400
      *   AUTHOR   :   PROGRAMMERS.IO
      *   PROJECT  :   INSURANCE MANAGEMENT SYSTEM
      *   DATE     :   19/08/2021 (DD/MM/YYYY)
      *************************************************************
      *
      *======================Header===============================*
      *
     HOPTION(*NODEBUGIO)
      *
      *================Display File Declaration===================*
      *
     FLOGIN_SCR CF   E             WORKSTN
     F                                     INDDS(Indds1)
     FCUST_SCR  CF   E             WORKSTN sfile(pol_sfl:rrn)
     F                                     sfile(Req_sfl:rrn1)
     F                                     sfile(A_namewsfl:rrn2)
     F                                     INDDS(Indds1)
      *
      *================PF Declaration=============================*
      *
     FLOGIN_PF  UF A E           K DISK
     FCUSt_Mast UF A E           K DISK
     FPolicy_DtlUF A E           K DISK
     FReq_dtl   UF A E           K DISK
     FAGENT_MASTUF A E           K DISK
     DAGENT_pgmp       pr                  Extpgm('AAKASH/AGENT_PGM')
     D                               10a
     D usrid2          s             10a
     DAdmin_pgmp       pr                  Extpgm('AAKASH/ADMIN_PGM')
     D                               10a
     D usridds         s             10a
      *
      *===============Variable Declaration=======================*
      *
     D SCom_line       s             20A
     D RRn             s              4s 0
     D RRn1            s              4s 0
     D RRn2            s              4s 0
     D D_posnam        s              7  0
     D D_fstnam        s             15A
     D D_tmpvar        s             10A
      *
      *================Indicator Data Structure===================*
      *
     D INDDS1          DS
     D Key_Exit                       1N   Overlay(INDDS1:03)
     D Rev_Img                        1N   Overlay(INDDS1:41)
     D Refresh                        1N   Overlay(INDDS1:05)
     D Pos_Cur                        1N   overlay(INDDS1:42)
     D Key_Chgpwd                     1N   overlay(INDDS1:02)
     D Key_Back                       1N   overlay(INDDS1:12)
     D Pos_Cur1                       1N   overlay(INDDS1:51)
     D Pos_Cur2                       1N   overlay(INDDS1:52)
     D pos_cur3                       1N   overlay(INDDS1:53)
     D Chg_Upd                        1N   overlay(INDDS1:06)
     D Key_Upd                        1N   overlay(INDDS1:07)
     D prompt                         1N   Overlay(INDDS1:04)
     D Key_und                        1N   Overlay(INDDS1:13)
      *
      *===========Subfile indicators=======================*
      *
     D Clr_in                         1N   Overlay(INDDS1:33)
     D Dsp_in                         1N   Overlay(INDDS1:31)
     D End_in                         1N   Overlay(INDDS1:34)
     D Dspctl_in                      1N   Overlay(INDDS1:32)
      *
      *================Main Program===============================*
      *
     c                   Dow       Key_EXit = *off
     c                   ExFmt     L_SCRREC
     C                   Exsr      $Reset
     c                   clear                   Errmsg
     c
     c     D_UsrId       Chain     L_rec
     c                   select
     c                   when      not %found(Login_PF)
     c                   Eval      Rev_Img = *On
     c                   Eval      Errmsg ='Invalid User ID'
     c
     c                   when      Key_chgpwd = *on
     c                   Exsr      $Chg_pwd
     c
     c                   when      D_pwd <> Usr_psw
     C                   Eval      Pos_Cur = *On
     c                   Eval      Errmsg ='Invalid Password'
     c
     c                   when      Refresh = *on
     c                   clear                   L_SCRREC
     c                   When      Usr_typ ='c'
     C     Usr_ID        chain     Cust_Rec
     C                   IF        %found(Cust_mast)
     C                   Eval      D_POsnam = %scan(' ':Cust_name:1)
     C                   Eval      D_fstnam = %subst(Cust_name:1:D_posnam)
     C                   Eval      D_Usrname = D_Fstnam
     C                   Endif
     c                   Exsr      $Customer
     c                   When      Usr_Typ ='A'
     C     Usr_ID        chain     Agent_Rec
     C                   IF        %found(Agent_mast)
     C                   Eval      D_POsnam = %scan(' ':Agen_name:1)
     C                   Eval      D_fstnam = %subst(Agen_name:1:D_posnam)
     C*                  Eval      D_Ausr  = D_Fstnam
     C                   Endif
     c                   eval      usriD2 = D_usrId
     c                   clear                   L_SCRREC
     C                   callp     Agent_pgmp(usrid2)
     C                   If        Key_exit =*on
     C                   eval      Key_exit =*off
     C                   Leave
     C                   Endif
     C                   When      Usr_Typ ='M'
     c                   eval      usriDds = D_usrId
     C                   callp     Admin_pgmp(usridds)
     c                   other
     c                   endsl
     c                   Enddo
     c                   Eval      *Inlr = *On
      *************************************************************
      *==================Change Password==========================*
      *
     c     $Chg_pwd      Begsr
     C                   Dow       Key_Exit = *off
     C                   Eval      D_UsrID = *Blanks
     C                   Eval      D_pwd   = *Blanks
     c                   Exfmt     CHG_SCRREC
     c                   If        Key_Back = *oN
     c                   Eval      Key_Back = *off
     C                   Leave
     C                   Endif
     c                   Exsr      $Reset
     c                   Select
     c                   when      Old_pwd <> Usr_psw
     c                   Eval      pos_cur1 = *On
     c                   Eval      Chg_Err = 'Old Password not Correct'
     c                   when      New_pwd <> con_pwd
     c                   Eval      pos_cur2 = *On
     c                   Eval      pos_cur3 = *On
     c                   Eval      Chg_Err = 'New and confirm password not same'
     C                   When      Refresh = *on
     C                   Clear                   CHG_SCRREC
     c                   when      Chg_upd = *on
     c                   Eval      Usr_psw = New_pwd
     C                   Update    L_REC
     c                   Eval      Errmsg ='Password Change Successfully'
     C                   Leave
     c                   Other
     c                   Clear                   Chg_Err
     C                   Endsl
     C                   Enddo
     c                   Endsr
      *******************************Reset Indicator********************
      *
     c     $Reset        Begsr
     c                   Eval      Rev_Img = *off
     c                   Eval      pos_cur1 = *off
     c                   Eval      pos_cur2 = *off
     c                   Eval      pos_cur3 = *off
     c                   Endsr
      *******************************$customer PGM=====================*
      *
     c     $Customer     Begsr
     C                   Dow       Key_Exit = *OFF
     C                   Eval      D_UsrID = *Blanks
     C                   Eval      D_pwd   = *Blanks
     C                   clear                   Com_line
     C                   Exfmt     C_HSCR
     C                   Eval      Key_Und = *OFF
     c                   Eval      sCom_line = %trim(Com_Line)
     C                   If        Key_Back = *on
     C                   Eval      Key_Back = *off
     c                   Leave
     c                   Endif
     c                   Select
     c                   When      sCom_line = '1'
     c                   Exsr      $view_Pdtl
     c                   When      sCom_line = '2'
     c                   Exsr      $POl_Enq
     c                   When      sCom_line = '3'
     c                   Exsr      $View_Ownpol
     c                   When      sCom_line = '4'
     c                   Exsr      $View_Req
     c                   When      sCom_line = '5'
     c                   Exsr      $Raise_com
     c                   When      sCom_line = '6'
     c                   Exsr      $Chg_pwd
     c                   When      sCom_line = '90' or scom_line ='SIGN OFF'
     c                                 or  Scom_line = 'SIGNOFF'
     C                   Eval      Key_Exit = *on
     c                   when      scom_line = *blanks
     C                   Eval      C_herrmsg = *blanks
     C                   other
     c                   Eval      C_Herrmsg ='Wrong option selection'
     c                   Endsl
     C                   Enddo
     c                   Endsr
      *
      *===================Customer View Personal Details===============*
      *
     c     $view_Pdtl    BegSr
     C                   Dow       Key_Exit = *OFF
     C     Usr_ID        Chain     Cust_rec
     C                   IF        %found(Cust_Mast)
     c                   Exfmt     CUST_PDTL
     C                   Endif
     c                   Select
     c                   When      Key_back = *on
     c                   Eval      Key_back = *off
     c                   Leave
     C                   When      Key_upd = *on
     C                   Exsr      $Upd_dTl
     C                   Endsl
     c                   EndDo
     c                   EndSr
      *
      *=====================Update personal Detail Request=========*
      *
     C     $Upd_dtl      BegSr
     c                   Dow       Key_exit = *off
     C     Usr_ID        Chain     Cust_rec
     C                   IF        %found(Cust_Mast)
     C                   Endif
     c                   Exfmt     CUST_UPDTL
     c                   Select
     c                   When      Key_back = *on
     c                   Eval      Key_back = *off
     c                   Leave
     C                   Endsl
     c                   EndDo
     c                   EndSr
      *
      *=====================Policy Enquiry SubRoutine==============*
      *
     c     $POl_Enq      BegSr
     c                   Exsr      Clear
     c                   Exsr      load
     c                   Exsr      display
     c                   EndSr
     **************************************************************
      *
     c     clear         Begsr
     c                   Eval      Clr_in = *on
     C                   clear                   RRN
     C                   write     Pol_ctl
     c                   Eval      Clr_in = *off
     c                   EndSr
     **************************************************************
     C     load          Begsr
     c     *loval        Setll     POlicy_dtl
     C                   Read      Policy_dtl
     C                   Dow       Not %Eof(Policy_dtl)
     C                   Eval      Rrn = rrn + 1
     C                   write     Pol_sfl
     C                   Read      Policy_dtl
     C                   Enddo
     C                   IF        Rrn <= 9999
     c                   Eval      dsp_In = *on
     c                   Else
     c                   Eval      dsp_In = *off
     C                   Endif
     c                   Endsr
     *********************************************************
     C     Display       BegSr
     C                   Eval      End_in  = *on
     C                   Eval      Dspctl_in  = *on
     C                   Dow       Key_Exit = *off
     c                   write     Footer
     C                   Eval      C_opt = *zeros
     C                   Exfmt     Pol_ctl
     C                   Readc     Pol_SFL
     C                   Dow       Not %Eof(Cust_Scr)
     C                   If        C_opt = 5
     C                   Exsr      $dsply_POl
     C                   Endif
     C                   clear                   C_opt
     C                   Update    POl_sfl
     C                   ReadC     Pol_SFL
     C                   Enddo
     C                   Select
     C                   When      Key_Back = *on
     C                   Eval      Key_BacK = *off
     C                   Leave
     C                   Endsl
     C                   Enddo
     C                   Endsr
      *========================Display Policy Details==============*
      *
     c     $dsply_pol    BegSr
     c                   Dow       Key_Exit = *off
     c                   Exfmt     DIS_POLCY
     C                   If        Key_Back = *on
     C                   Eval      Key_BacK = *off
     C                   Leave
     C                   EndIf
     c                   Enddo
     C                   EndSr
      *==================View Own Policy Details====================*
      *
     C     $View_Ownpol  Begsr
     c                   Dow       Key_Exit = *off
     C                   Exfmt     C_VWOWNPOL
     c                   If        Key_BacK = *on
     C                   Eval      Key_BacK = *off
     C                   Leave
     C                   Endif
     c                   Enddo
     C                   Endsr
      *
      *==================View_Req Status============================*
      *
     c     $View_Req     Begsr
     C                   Exsr      Clear1
     c                   Exsr      load1
     C                   exsr      Display1
     c                   Endsr
      *
      *==================Clear request Details======================*
      *
     c     Clear1        Begsr
     c                   Eval      Clr_in = *on
     C                   clear                   RRN1
     C                   write     Req_ctl
     c                   Eval      Clr_in = *off
     C                   Endsr
      *==================Load request Details======================*
      *
     c     Load1         Begsr
     c     *loval        Setll     Req_dtl
     C                   Read      req_dtl
     C                   Dow       Not %Eof(Req_dtl)
     C                   Eval      Rrn1 = rrn1 + 1
     C                   write     req_sfl
     C                   Read      req_dtl


     C                   Enddo
     C                   IF        Rrn1 <= 9999
     c                   Eval      dsp_In = *on
     c                   Else
     c                   Eval      dsp_In = *off
     C                   Endif
     C                   Endsr
      *=======================Display request Details===================*
      *
     c     Display1      BegSr
     C                   Eval      End_in  = *on
     C                   Eval      Dspctl_in  = *on
     C                   Dow       Key_Exit = *off
     c                   write     Footer
     C                   Exfmt     req_ctl
     c                   readc     Req_sfl
     c                   dow       Not %eof(Cust_scr)
     C                   If        Req_opt = 5
     C                   Exsr      D_req
     C                   Endif
     C                   eval      Req_opt = *zeros
     C                   Update    Req_sfl
     C                   readc     Req_sfl
     C                   Enddo
     C                   Select
     C                   When      Key_Back = *on
     C                   Eval      Key_BacK = *off
     C                   Leave
     C                   Endsl
     C                   Enddo
     C                   Endsr
      *==================View request status window=====================*
      *
     c     D_req         Begsr
     C                   Dow       Key_exit = *off
     C                   Exfmt     D_reqdtl
     C                   If        Key_back = *on
     C                   Eval      Key_back = *off
     C                   Leave
     C                   Endif
     C                   Enddo
     C                   Endsr
      *==================Raise Complaint================================*
      *
     c     $raise_com    BegSr
     C                   Dow       Key_Exit =*off
     C                   Eval      S_RCUSTNAM = Cust_name
     C                   Exfmt     Raise_com
     C                   Select
     C                   When      Key_back = *on
     C                   Eval      Key_back = *off
     C                   Leave
     C                   when      Refresh = *on
     C                   Clear                   Raise_com
     C                   When      prompt = *on
     C                   Exsr      $W_ANAME
     C*                  When
     C*                  Exsr
     C                   Endsl
     C                   Enddo
     C                   EndSr
      *
      *===================WIndow for Agent name==============*
      *
     C     $W_AName      Begsr
     C                   Exsr      WA_Clear
     C                   Exsr      WA_load
     C                   Exsr      WA_disply
     C                   Endsr
      ******************************************************
     c     WA_clear      BegSr
     c                   Eval      Clr_in = *on
     C                   clear                   RRN2
     C                   write     A_NAMECTL
     c                   Eval      Clr_in = *off
     C                   Endsr
      *==================Load window agent Names======================*
      *
     c     WA_load       Begsr
     c     *loval        Setll     Agent_mast
     C                   Read      Agent_mast
     C                   Dow       Not %Eof(Agent_mast)
     C                   Eval      Rrn2 = rrn2 + 1
     C                   write     A_NAMEWSFL
     C                   Read      Agent_mast
     C                   Enddo
     C                   IF        Rrn2 <= 9999
     c                   Eval      dsp_In = *on
     c                   Else
     c                   Eval      dsp_In = *off
     C                   Endif
     C                   Endsr
      *********************************************************************
     c     WA_disply     BegSr
     C                   Eval      End_in  = *on
     C                   Eval      Dspctl_in  = *on
     C                   Dow       Key_Exit = *off
     C                   Exfmt     A_NAMECTL
     C                   IF        Key_Back = *on
     C                   Eval      Key_Back = *off
     c                   Leave
     C                   Endif
     C                   Readc     A_NAMEWSFL
     C                   Dow       Not %eof(Cust_Scr)
     C                   If        SA_opt = 1
     C                   Eval      S_agenam = Agen_Name
     c                   Leave
     C                   EndIf
     C                   ReadC     A_NAMEWSFL
     C                   Enddo
     C                   Enddo
     c                   Endsr
