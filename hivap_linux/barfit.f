      SUBROUTINE BARFIT(IZ,IA,IL,SBFIS,SEGS,SELMAX)                             
C
C Programm zur Berechnung der Liquid-Drop Barrieren nach Sierk
C Stand: 3.5.1994
C
C     VERSION FOR 32BIT COMPUTER                                                
C     THIS SUBROUTINE RETURNS THE BARRIER HEIGHT BFIS, THE                      
C     GROUND-STATE ENERGY SEGS, IN MEV, AND THE ANGULAR MOMENTUM                
C     AT WHICH THE FISSION BARRIER DISAPPEARS, LMAX, IN UNITS OF                
C     H-BAR, WHEN CALLED WITH INTEGER AGUMENTS IZ, THE ATOMIC                   
C     NUMBER, IA, THE ATOMIC MASS NUMBER, AND IL, THE ANGULAR                   
C     MOMENTUM IN UNITS OF H-BAR. (PLANCK'S CONSTANT DIVIDED BY                 
C     2*PI).                                                                    
C                                                                               
C        THE FISSION BARRIER FO IL = 0 IS CALCULATED FROM A 7TH                 
C     ORDER FIT IN TWO VARIABLES TO 638 CALCULATED FISSION                      
C     BARRIERS FOR Z VALUES FROM 20 TO 110. THESE 638 BARRIERS ARE              
C     FIT WITH AN RMS DEVIATION OF 0.10 MEV BY THIS 49-PARAMETER                
C     FUNCTION.                                                                 
C     IF BARFIT IS CALLED WITH (IZ,IA) VALUES OUTSIDE THE RANGE OF              
C     THE BARRIER HEIGHT IS SET TO 0.0, AND A MESSAGE IS PRINTED                
C     ON THE DEFAULT OUTPUT FILE.                                               
C                                                                               
C        FOR IL VALUES NOT EQUAL TO ZERO, THE VALUES OF L AT WHICH              
C     THE BARRIER IS 80% AND 20% OF THE L=0 VALUE ARE RESPECTIVELY              
C     FIT TO 20-PARAMETER FUNCTIONS OF Z AND A, OVER A MORE                     
C     RESTRICTED RANGE OF A VALUES, THAN IS THE CASE FOR L = 0.                 
C     THE VALUE OF L WHERE THE BARRIER DISAPPEARS, LMAX IS FIT TO               
C     A 24-PARAMETER FUNCTION OF Z AND A, WITH THE SAME RANGE OF                
C     Z AND A VALUES AS L-80 AND L-20.                                          
C        ONCE AGAIN, IF AN (IZ,IA) PAIR IS OUTSIDE OF THE RANGE OF              
C     VALIDITY OF THE FIT, THE BARRIER VALUE IS SET TO 0.0 AND A                
C     MESSAGE IS PRINTED. THESE THREE VALUES (BFIS(L=0),L-80, AND               
C     L-20) AND THE CONSTRINTS OF BFIS = 0 AND D(BFIS)/DL = 0 AT                
C     L = LMAX AND L=0 LEAD TO A FIFTH-ORDER FIT TO BFIS(L) FOR                 
C     L>L-20. THE FIRST THREE CONSTRAINTS LEAD TO A THIRD-ORDER FIT             
C     FOR THE REGION L < L-20.                                                  
C                                                                               
C        THE GROUND STATE ENERGIES ARE CALCULATED FROM A                        
C     120-PARAMETER FIT IN Z, A, AND L TO 214 GROUND-STATE ENERGIES             
C     FOR 36 DIFFERENT Z AND A VALUES.                                          
C     (THE RANGE OF Z AND A IS THE SAME AS FOR L-80, L-20, AND                  
C     L-MAX)                                                                    
C                                                                               
C        THE CALCULATED BARRIERS FROM WHICH THE FITS WERE MADE WERE             
C     CALCULATED IN 1983-1984 BY A. J. SIERK OF LOS ALAMOS                      
C     NATIONAL LABORATORY GROUP T-9, USING YUKAWA-PLUS-EXPONENTIAL              
C     DOUBLE FOLDED NUCLEAR ENERGY, EXACT COULOMB DIFFUSENESS                   
C     CORRECTIONS, AND DIFFUSE-MATTER MOMENTS OF INERTIA.                       
C     THE PARAMETERS OF THE MODEL R-0 = 1.16 FM, AS 21.13 MEV,                  
C     KAPPA-S = 2.3, A = 0.68 FM.                                               
C     THE DIFFUSENESS OF THE MATTER AND CHARGE DISTRIBUTIONS USED               
C     CORRESPONDS TO A SURFACE DIFFUSENESS PARAMETER (DEFINED BY                
C     MYERS) OF 0.99 FM. THE CALCULATED BARRIERS FOR L = 0 ARE                  
C     ACCURATE TO A LITTLE LESS THAN 0.1 MEV; THE OUTPUT FROM                   
C     THIS SUBROUTINE IS A LITTLE LESS ACCURATE. WORST ERRORS MAY BE            
C     AS LARGE AS 0.5 MEV; CHARACTERISTIC UNCERTAINY IS IN THE RANGE            
C     OF 0.1-0.2 MEV. THE RMS DEVIATION OF THE GROUND-STATE FIT                 
C     FROM THE 214 INPUT VALUES IS 0.20 MEV. THE MAXIMUM ERROR                  
C     OCCURS FOR LIGHT NUCLEI IN THE REGION WHERE THE GROUND STATE              
C     IS PROLATE, AND MAY BE GREATER THAN 1.0 MEV FOR VERY NEUTRON              
C     DEFICIENT NUCLEI, WITH L NEAR LMAX. FOR MOST NUCLEI LIKELY TO             
C     BE ENCOUNTERED IN REAL EXPERIMENTS, THE MAXIMUM ERROR IS                  
C     CLOSER TO 0.5 MEV, AGAIN FOR LIGHT NUCLEI AND L NEAR LMAX.                
C                                                                               
C     WRITTEN BY A. J. SIERK, LANL T-9                                          
C     VERSION 1.0 FEBRUARY, 1984                                                
C                                                                               
C     THE FOLLOWING IS NECESSARY FOR 32-BIT MACHINES LIKE DEC VAX,              
C     IBM, ETC                                                                  
C                                                                               
C                                                                               
      IMPLICIT REAL*8 (A-H,O-Z)                                                 
C     REAL*4 SBFIS,SEGS,SELMAX                                                  
      DIMENSION  ELZCOF(7,7),ELMCOF(5,4),EMNCOF(5,4),PA(7),PZ(7),PL(10)         
      DIMENSION  EMXCOF(6,4),EGSCOF(5,6,4),EGS1(5,6),EGS2(5,6),                 
     1           EGS3(5,6),EGS4(5,6)                                            
      EQUIVALENCE (EGS1(1,1),EGSCOF(1,1,1)), (EGS2(1,1),EGSCOF(1,1,2)),         
     1            (EGS3(1,1),EGSCOF(1,1,3)), (EGS4(1,1),EGSCOF(1,1,4))          
      DATA EMNCOF                                                               
     1/-9.01100D+2,-1.40818D+3, 2.77000D+3,-7.06695D+2, 8.89867D+2,             
     2  1.35355D+4,-2.03847D+4, 1.09384D+4,-4.86297D+3,-6.18603D+2,             
     3 -3.26367D+3, 1.62447D+3, 1.36856D+3, 1.31731D+3, 1.53372D+2,             
     4  7.48863D+3,-1.21581D+4, 5.50281D+3,-1.33630D+3, 5.05367D-2/             
      DATA ELMCOF                                                               
     1 /1.84542D+3,-5.64002D+3, 5.66730D+3,-3.15150D+3, 9.54160D+2,             
     2 -2.24577D+3, 8.56133D+3,-9.67348D+3, 5.81744D+3,-1.86997D+3,             
     3  2.79772D+3,-8.73073D+3, 9.19706D+3,-4.91900D+3, 1.37283D+3,             
     4 -3.01866D+1, 1.41161D+3,-2.85919D+3, 2.13016D+3,-6.49072D+2/             
      DATA EMXCOF /                                                             
     19.43596D4,-2.241997D5,2.223237D5,-1.324408D5,4.68922D4,-8.83568D3,        
     2-1.655827D5,4.062365D5,-4.236128D5,2.66837D5,-9.93242D4,1.90644D4,        
     3 1.705447D5,-4.032D5,3.970312D5,-2.313704D5,7.81147D4,-1.322775D4,        
     4-9.274555D4,2.278093D5,-2.422225D5,1.55431D5,-5.78742D4,9.97505D3/        
      DATA ELZCOF                                                               
     1 /5.11819909D+5,-1.30303186D+6, 1.90119870D+6,-1.20628242D+6,             
     2  5.68208488D+5, 5.48346483D+4,-2.45883052D+4,                            
     3 -1.13269453D+6, 2.97764590D+6,-4.54326326D+6, 3.00464870D+6,             
     4 -1.44989274D+6,-1.02026610D+5, 6.27959815D+4,                            
     5  1.37543304D+6,-3.65808988D+6, 5.47798999D+6,-3.78109283D+6,             
     6  1.84131765D+6, 1.53669695D+4,-6.96817834D+4,                            
     7 -8.56559835D+5, 2.48872266D+6,-4.07349128D+6, 3.12835899D+6,             
     8 -1.62394090D+6, 1.19797378D+5, 4.25737058D+4,                            
     9  3.28723311D+5,-1.09892175D+6, 2.03997269D+6,-1.77185718D+6,             
     A  9.96051545D+5,-1.53305699D+5,-1.12982954D+4,                            
     B  4.15850238D+4, 7.29653408D+4,-4.93776346D+5, 6.01254680D+5,             
     C -4.01308292D+5, 9.65968391D+4,-3.49596027D+3,                            
     D -1.82751044D+5, 3.91386300D+5,-3.03639248D+5, 1.15782417D+5,             
     E -4.24399280D+3,-6.11477247D+3, 3.66982647D+2/                            
      DATA EGS1 /                                                               
     2 1.927813D5, 7.666859D5, 6.628436D5, 1.586504D5,-7.786476D3,              
     3-4.499687D5,-1.784644D6,-1.546968D6,-4.020658D5,-3.929522D3,              
     4 4.667741D5, 1.849838D6, 1.641313D6, 5.229787D5, 5.928137D4,              
     5-3.017927D5,-1.206483D6,-1.124685D6,-4.478641D5,-8.682323D4,              
     6 1.226517D5, 5.015667D5, 5.032605D5, 2.404477D5, 5.603301D4,              
     7-1.752824D4,-7.411621D4,-7.989019D4,-4.175486D4,-1.024194D4/              
      DATA EGS2 /                                                               
     1-6.459162D5,-2.903581D6,-3.048551D6,-1.004411D6,-6.558220D4,              
     2 1.469853D6, 6.564615D6, 6.843078D6, 2.280839D6, 1.802023D5,              
     3-1.435116D6,-6.322470D6,-6.531834D6,-2.298744D6,-2.639612D5,              
     4 8.665296D5, 3.769159D6, 3.899685D6, 1.520520D6, 2.498728D5,              
     5-3.302885D5,-1.429313D6,-1.512075D6,-6.744828D5,-1.398771D5,              
     6 4.958167D4, 2.178202D5, 2.400617D5, 1.167815D5, 2.663901D4/              
      DATA EGS3 /                                                               
     1 3.117030D5, 1.195474D6, 9.036289D5, 6.876190D4,-6.814556D4,              
     2-7.394913D5,-2.826468D6,-2.152757D6,-2.459553D5, 1.101414D5,              
     3 7.918994D5, 3.030439D6, 2.412611D6, 5.228065D5, 8.542465D3,              
     4-5.421004D5,-2.102672D6,-1.813959D6,-6.251700D5,-1.184348D5,              
     5 2.370771D5, 9.459043D5, 9.026235D5, 4.116799D5, 1.001348D5,              
     6-4.227664D4,-1.738756D5,-1.795906D5,-9.292141D4,-2.397528D4/              
      DATA EGS4 /                                                               
     1-1.072763D5,-5.973532D5,-6.151814D5, 7.371898D4, 1.255490D5,              
     2 2.298769D5, 1.265001D6, 1.252798D6,-2.306276D5,-2.845824D5,              
     3-2.093664D5,-1.100874D6,-1.009313D6, 2.705945D5, 2.506562D5,              
     4 1.274613D5, 6.190307D5, 5.262822D5,-1.336039D5,-1.115865D5,              
     5-5.715764D4,-2.560989D5,-2.228781D5,-3.222789D3, 1.575670D4,              
     6 1.189447D4, 5.161815D4, 4.870290D4, 1.266808D4, 2.069603D3/              
C-----------------------------------------------------------------------        
C     THE PROGRAM STARTS HERE                                                   
C                                                                               
C     I.Giese 2007
      SAVE
C
      IF (IZ.LT.19 .OR. IZ.GT.111) GOTO 900                                     
      IF(IZ.GT.102  .AND. IL.GT.0) GOTO 902                                     
      Z=FLOAT(IZ)                                                               
      A=FLOAT(IA)                                                               
      EL=FLOAT(IL)                                                              
      AMIN= 1.2D0*Z + 0.01D0*Z*Z                                                
      AMAX= 5.8D0*Z - 0.024D0*Z*Z                                               
      IF(A .LT. AMIN .OR. A .GT. AMAX) GOTO 910                                 
C-----------------------------------------------------------------------        
C                                ANGUL.MOM.ZERO BARRIER                         
      AA=2.5D-3*A                                                               
      ZZ=1.D-2*Z                                                                
      ELL=1.D-2*EL                                                              
      BFIS0=0.D0                                                                
      CALL LPOLY(ZZ,7,PZ)                                                       
      CALL LPOLY(AA,7,PA)                                                       
      DO 10 I=1,7                                                               
      DO 10 J=1,7                                                               
      BFIS0=BFIS0+ELZCOF(J,I)*PZ(J)*PA(I)                                       
 10   CONTINUE                                                                  
      BFIS=BFIS0                                                                
      SBFIS=BFIS                                                                
      EGS=0.D0                                                                  
      SEGS=EGS                                                                  
C-----------------------------------------------------------------------        
C                               VALUES OF L AT WHICH THE BARRIER                
C                            IS 20%(EL20) AND 80%(EL80) OF L=0 VALUE            
      AMIN2=1.4D0*Z + 0.009D0*Z*Z                                               
      AMAX2=20.D0 + 3.0D0*Z                                                     
      IF((A.LT.AMIN2-5.D0 .OR. A.GT.AMAX2+10.D0).AND. IL.GT.0) GOTO 920         
      CALL LPOLY(ZZ,5,PZ)                                                       
      CALL LPOLY(AA,4,PA)                                                       
      EL80=0.D0                                                                 
      EL20=0.D0                                                                 
      ELMAX=0.D0                                                                
      DO 20 I=1,4                                                               
      DO 20 J=1,5                                                               
      EL80=EL80+ELMCOF(J,I)*PZ(J)*PA(I)                                         
      EL20=EL20+EMNCOF(J,I)*PZ(J)*PA(I)                                         
 20   CONTINUE                                                                  
      SEL80=EL80                                                                
      SEL20=EL20                                                                
C     WRITE(6,25) SEL80,SEL20                                                   
 25   FORMAT(' EL80=',F8.1,4X,'EL20=',F8.1)                                     
C-----------------------------------------------------------------------        
C                               VALUE OF L (ELMAX) WHERE BARRIER DISAPP.        
      CALL LPOLY(ZZ,6,PZ)                                                       
      CALL LPOLY(ELL,9,PL)                                                      
      DO 30 I= 1,4                                                              
      DO 30 J=1,6                                                               
      ELMAX=ELMAX+EMXCOF(J,I)*PZ(J)*PA(I)                                       
 30   CONTINUE                                                                  
      SELMAX=ELMAX                                                              
C----------------------------------------------------------------------         
C                              VALUE OF BARRIER AT ANG.MOM.  L                  
      IF(IL.LT.1) RETURN                                                        
      X=SEL20/SELMAX                                                            
      Y=SEL80/SELMAX                                                            
      IF(EL.GT.SEL20) GOTO 40                                                   
C-------------------                                 LOW L                      
C                                                                               
      Q=0.2D0/(SEL20**2*SEL80**2*(SEL20-SEL80))                                 
      QA=Q*(4.D0*SEL80**3 - SEL20**3)                                           
      QB=-Q*(4.D0*SEL80**2 - SEL20**2)                                          
      BFIS=BFIS*(1.D0 + QA*EL**2 + QB*EL**3)                                    
      GOTO 50                                                                   
C-------------------                                 HIGH L                     
C                                                                               
 40   AJ=(-20.D0*X**5 + 25.D0*X**4 - 4.D0)*(Y-1.D0)**2*Y*Y                      
      AK=(-20.D0*Y**5 + 25.D0*Y**4 - 1.D0) * (X-1.D0)**2*X*X                    
      Q= 0.2D0/((Y-X)*((1.D0-X)*(1.D0-Y)*X*Y)**2)                               
      QA=Q*(AJ*Y - AK*X)                                                        
      QB=-Q*(AJ*(2.D0*Y+1.D0) - AK*(2.D0*X+1.D0))                               
      Z=EL/SELMAX                                                               
      A1=4.D0*Z**5 - 5.D0*Z**4 + 1.D0                                           
      A2=QA*(2.D0*Z+1.D0)                                                       
      BFIS=BFIS*(A1 + (Z-1.D0)*(A2 + QB*Z)*Z*Z*(Z-1.D0))                        
 50   IF(BFIS.LE.0.0D0) BFIS=0.0D0                                              
      IF(EL.GT.SELMAX) BFIS=0.0D0                                               
      SBFIS=BFIS                                                                
C---------------------------------------------------------------                
C     NOW CALCULATE ROTATING GROUND STATE ENERGY                                
C                                                                               
      IF(EL.GT.SELMAX) RETURN                                                   
      DO 70 K=1,4                                                               
         DO 70 L=1,6                                                            
            DO 70 M=1,5                                                         
            EGS=EGS+EGSCOF(M,L,K)*PZ(L)*PA(K)*PL(2*M-1)                         
 70   CONTINUE                                                                  
      SEGS=EGS                                                                  
      IF(SEGS.LT.0.0) SEGS=0.0                                                  
      RETURN                                                                    
 900  WRITE(6,1000)                                                             
      SBFIS=0.0                                                                 
      SEGS=0.0                                                                  
      SELMAX=0.0                                                                
      RETURN                                                                    
 902  WRITE(6,1002)                                                             
      SBFIS=0.0                                                                 
      SEGS=0.0                                                                  
      SELMAX=0.0                                                                
      RETURN                                                                    
 910  WRITE(6,1010) IA                                                          
      SBFIS=0.0                                                                 
      SEGS=0.0                                                                  
      SELMAX=0.0                                                                
      RETURN                                                                    
 920  WRITE(6,1020) IA,IL                                                       
      SBFIS=0.0                                                                 
      SEGS=0.0                                                                  
      SELMAX=0.0                                                                
      RETURN                                                                    
C                                                                               
 1000 FORMAT(/10X,'**** BARFIT CALLED WITH Z LESS THAN 19 OR',                  
     1        ' GREATER THAN 111; BFIS IS SET 0.0 ****'/)                       
 1002 FORMAT(/10X,'**** BARFIT CALLED WITH Z GREATER THAN 102',                 
     1        ' AND L NOT EQUAL TO ZERO; BFIS IS SET 0.0 ****'/)                
 1010 FORMAT(/10X,'**** BARFIT CALLED WITH A =',I3,' OUTSIDE ',                 
     1        ' THE ALLOWED VALUES FOR Z = ',I3,' ****'/)                       
 1020 FORMAT(/10X,'**** BARFIT CALLED WITH A =',I3,' OUTSIDE ' ,                
     1        'THE ALLOWED VALUES FOR Z = ',I3/26X,'FOR NONZERO L =',I3         
     2         ,' ****'/)                                                       
      END                                                                       
C                                                                               
C----------------------------------------------------------------------         
C                                                                               
      SUBROUTINE LPOLY(X,N,PL)                                                  
C                                                                               
C     THIS SUBROUTINE CALCULATES THE ORDINARY LEGENDRE POLYNOMIALS OF           
C     ORDER 0 TO N-1 OF ARGUMENT X AND STORES THEM IN THE VECTOR PL.            
C     THEY ARE CALCULATED BY RECURSION RELATION FROM THE FIRST TWO              
C     POLYNOMIALS.                                                              
C     WRITTEN BY A.J.SIERK  LANL  T-9  FEBRUARY, 1984                           
C                                                                               
C     NOTE: PL AND X MUST BE DOUBLE PRECISION ON 32-BIT COMPUTERS!              
C                                                                               
      DOUBLE PRECISION PL,X                                                     
C                                                                               
       DIMENSION PL(20)                                                         
       PL(1)=1.0                                                                
       PL(2)=X                                                                  
       DO 10 I=3,N                                                              
       PL(I) = ((2*I-3)*X*PL(I-1)-(I-2)*PL(I-2))/(I-1)                          
 10    CONTINUE                                                                 
       RETURN                                                                   
       END                                                                      
