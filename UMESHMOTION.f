C      ######################################################################
C      #################      CAE Assistant Company          ################
C      ##############         www CAEassistant com              #############
C      ###########   Copy right by CAE Assistant Company    ###############
C      ######################################################################
C      ONLY the BUYER  of this package has permission to use its codes.
C	 Any distribution of this subroutine is illegal and will be prosecuted 
C      ######################################################################
C      ######################################################################
C      CAE Assisitant Services: 
C      Toturial Packages,Consultancy,Articles,Q&A,Video Gallery,Online Course
C      ######################################################################
C      Need help with your project? 
C      You can get initial free consultation from (Support CAEassistant com)
C      ######################################################################
      SUBROUTINE UMESHMOTION(UREF,ULOCAL,NODE,NNDOF,
     *    LNODETYPE,ALOCAL,NDIM,TIME,DTIME,PNEWDT,
     *    KSTEP,KINC,KMESHSWEEP,JMATYP,JGVBLOCK,LSMOOTH)
C
      INCLUDE 'ABA_PARAM.INC'
C
      DIMENSION ULOCAL(NDIM),JELEMLIST(*)
      DIMENSION ALOCAL(NDIM,*),TIME(2)
      DIMENSION JMATYP(*),JGVBLOCK(*)
C
C
C//////////////////////////////////////////////////Start of the user code////////////////////////////////////////////////////////////////////////////
C
C////////////////////////////////Using GETNODETOELEMCONN to retrieve a list of elements connected to a specified node//////////////////////////////      
      PARAMETER (MAXNELEMS = 100)
C      
      DIMENSION JELEMLIST(MAXNELEMS) !Array of element numbers for elements connected to NODE. 
      DIMENSION JELEMTYPE(MAXNELEMS) !Array describing the element types for each element entry in JELEMLIST
C
      NELEMS = MAXNELEMS !Actual length of JELEMLIST
C
C     GETNODETOELEMCONN can be called from user subroutine UMESHMOTION to retrieve a list of elements connected to a specified node     
      CALL GETNODETOELEMCONN(NODE, NELEMS, JELEMLIST, JELEMTYPE,
     $                       JRCD, JGVBLOCK)
      !Returns JELEMLIST(Array of element numbers for elements connected to NODE),
      !JELEMTYPE(Array of element type designators describing the element types corresponding to each element entry in JELEMLIST),
      !NELEMS(Actual length of the JELEMLIST and JELEMTYPE arrays),
      !JRCD(Return code (0 indicates no error, 1 indicates an output request error))
C
C  
C      
C////////////////////////////////Using GETPARTINFO to obtain part instance information given global node/element number//////////////////////////////
      LOCNUM = 0 !The part-local node or element label to be looked up 
C      
      JTYPE= 0 !An integer flag indicating whether LOCNUM is a node or element label.
      !Set JTYP=0 to look up a node number, and set JTYP=1 to look up an element number.
C      
C      
      CHARACTER*80 CPNAME !The name of the part instance that contains INTNUM
      CPNAME = ' '  !An empty part instance name indicates that the node/element is at the assembly level and is not included in part instance
C 
C     Using GETPARTINFO to obtain part instance information given global node/element number      
      CALL GETPARTINFO(NODE, JTYP, CPNAME, LOCNUM, JRCD)
      !Returns JRCD Return code (0–no error, 1–error),
      !INTNUM (The internal (global) node or element label corresponding to LOCNUM in part instance CPNAME)
C
C   
C      
C////////////////////////////////Using GETVRMAVGATNODE to obtain part instance information given global node/element number//////////////////////////////
      DIMENSION ARRAY(6000) !Real array containing individual components of the output variable, returned from the Utility routine GETVRMAVGATNODE 
C
C     Utility routine GETVRMAVGATNODE can be called from UMESHMOTION to access material integration point information averaged at a node. 
      CALL GETVRMAVGATNODE(NODE,JTYP,'CSTRESS',ARRAY,JRCD,JELEMLIST,
     $                     NELEMS,JMATYP,JGVBLOCK)
      !Returns JRCD Return code (0 – no error, 1 – output request error or all components of output request are zero),
      !ARRAY (Real array containing individual components of the output variable)
C
C
C      
C////////////////////////////////Pressure and Velocity of nodal removal Calculations//////////////////////////////      
      REAL Pressure !Saves the contact pressure between the node on the slave surface and the master surface it interacts with.
      REAL Velocity !Velocity of nodal removal needed to define the wear
C      
      Pressure = ARRAY(1) !CSTRESS ARRAY=(CPRESS, CSHEAR1, CSHEAR2)
C
      Velocity = Pressure * UREF
      !Uref is the wear coefficient specified by the user, defined in the *ADAPTIVE MESH CONSTRAINT
C
C
C
C////////////////////////////////Applying the Wear//////////////////////////////        
      DO J=1, NDIM
          DO K=1, NDIM
              ULOCAL(J) = ULOCAL(J) - Velocity * ALOCAL(K,J) !ALOCAL denotes the local coordinate system aligned with
     $                                                 !the tangent of the adaptive mesh domain at the node.
          ENDDO
      ENDDO
C
      RETURN
      END
C
C
C
C//////////////////////////////////////////////////End of the user code////////////////////////////////////////////////////////////////////////////
      RETURN
      END