     FCust_mast UF A E           k Disk
     FAgent_Scr CF   E             workstn
     D REG_CUS         PR
     c
     P Reg_Cus         B                   Export
     D Reg_Cus         PI
     C                   Exfmt     A_ADDCust
     c*                  Eval      Cust_ID   = D1Cust_Id
     c*                  Eval      CUST_NAME = D1CUST_NAME
     c*                  Eval      C_ADDR    = D1C_ADDR
     c*                  Eval      C_LANMRK  = D1C_LANMRK
     c*                  Eval      C_CITY    = D1C_CITY
     c*                  Eval      C_PINCDE  = D1C_PINCDE
     c*                  Eval      C_STATE   = D1C_STATE
     c*                  Eval      C_MOBNO   = D1C_MOBNO
     c*                  Eval      C_ALTMOB  = D1C_ALTMOB
     c*                  Eval      C_COUNTRY = D1C_COUNTRY
     c*                  Eval      C_GENDER  = D1C_GENDER
     c*                  Eval      C_EMAIL   = D1C_EMAIL
     c*                  Eval      C_AADHAR  = D1C_AADHAR
     C                   Eval      *inlr = *on
     P ReG_Cus         E
