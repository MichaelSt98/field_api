! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_DEVICE_MEM_POOL

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE FIELD_ABORT_MODULE
        USE FIELD_DEFAULTS_MODULE
        USE DEV_ALLOC_MODULE, ONLY : DEVICE_POOL
        USE HOST_ALLOC_MODULE, ONLY : HOST_POOL
        USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : INT64
        IMPLICIT NONE
        CLASS(FIELD_1RD), POINTER :: F1 => NULL()
        CLASS(FIELD_1RM), POINTER :: F2 => NULL()
        CLASS(FIELD_1RD), POINTER :: F3 => NULL()
        CLASS(FIELD_2RD), POINTER :: F4 => NULL()
        INTEGER :: PADDED_F2_SIZE, F1_SIZE
        REAL(KIND=JPRD), POINTER, CONTIGUOUS :: PTR1(:) => NULL()
        REAL(KIND=JPRM), POINTER, CONTIGUOUS :: PTR2(:) => NULL()
        REAL(KIND=JPRD), POINTER, CONTIGUOUS :: PTR3(:) => NULL()
        REAL(KIND=JPRD), POINTER :: PTR4(:,:) => NULL()
        REAL(KIND=JPRD), POINTER :: HOST4(:,:) => NULL()
        REAL(KIND=JPRM), ALLOCATABLE :: DATA2(:)
        INTEGER :: I, J

        POOL_BLOCK_SIZE = 1024_INT64*SIZEOF(REAL(1., KIND=JPRD))
        DEV_POOL_BLOCK_SIZE = 1024_INT64*SIZEOF(REAL(1., KIND=JPRD))
#if defined(_CUDA) || defined(_HIP)
        USE_DEVICE_MEM_POOL = .TRUE.
#endif
        POOL_OWNED_FIELDS = .TRUE.

        !... Check creation of host and device memory pools
        CALL FIELD_NEW(F1, UBOUNDS=[256], PERSISTENT=.TRUE.)
        CALL F1%SYNC_HOST_RDWR()
        CALL F1%GET_DEVICE_DATA_RDWR(PTR1)
#if defined(_CUDA) || defined(_HIP)
        IF(.NOT. ASSOCIATED(DEVICE_POOL%START_BLK)) CALL FIELD_ABORT("ERROR")
#endif
        IF(.NOT. ASSOCIATED(HOST_POOL%START_BLK)) CALL FIELD_ABORT("ERROR")
        IF(F1%BLKID /= 1) CALL FIELD_ABORT("ERROR")
#if defined(_CUDA) || defined(_HIP)
        IF(F1%DEV_BLKID /= 1) CALL FIELD_ABORT("ERROR")
#endif

        !... Wrapped field will only be placed in device memory pool
        ALLOCATE(DATA2(511))
        CALL FIELD_NEW(F2, DATA=DATA2)
        CALL F2%SYNC_HOST_RDWR()
        CALL F2%GET_DEVICE_DATA_RDWR(PTR2)
        IF(F2%BLKID /= 0) CALL FIELD_ABORT("ERROR")
#if defined(_CUDA) || defined(_HIP)
        IF(F2%DEV_BLKID /= 1) CALL FIELD_ABORT("ERROR")
#endif

        CALL FIELD_NEW(F3, UBOUNDS=[1024], PERSISTENT=.TRUE.)
        CALL F3%SYNC_HOST_RDWR()
        CALL F3%GET_DEVICE_DATA_RDWR(PTR3)
        IF(.NOT. ASSOCIATED(HOST_POOL%START_BLK%NEXT)) CALL FIELD_ABORT("ERROR")
#if defined(_CUDA) || defined(_HIP)
        IF(.NOT. ASSOCIATED(DEVICE_POOL%START_BLK%NEXT)) CALL FIELD_ABORT("ERROR")
#endif
        IF(F3%BLKID /= 2) CALL FIELD_ABORT("ERROR")
#if defined(_CUDA) || defined(_HIP)
        IF(F3%DEV_BLKID /= 2) CALL FIELD_ABORT("ERROR")
#endif

        IF(HOST_POOL%START_BLK%NUMFLDS /= 1) CALL FIELD_ABORT("ERROR")
        IF(HOST_POOL%START_BLK%NEXT%NUMFLDS /= 1) CALL FIELD_ABORT("ERROR")
#if defined(_CUDA) || defined(_HIP)
        IF(DEVICE_POOL%START_BLK%NUMFLDS /= 2) CALL FIELD_ABORT("ERROR")
        IF(DEVICE_POOL%START_BLK%NEXT%NUMFLDS /= 1) CALL FIELD_ABORT("ERROR")
#endif

        !... Check allocation sizes
        IF(SIZEOF(F1%PTR) /= 256*SIZEOF(REAL(1, KIND=JPRD))) CALL FIELD_ABORT("ERROR")
        IF(SIZEOF(F2%PTR) /= 511*SIZEOF(REAL(1, KIND=JPRM))) CALL FIELD_ABORT("ERROR")
        IF(SIZEOF(F1%DEVPTR) /= 256*SIZEOF(REAL(1, KIND=JPRD))) CALL FIELD_ABORT("ERROR")
        IF(SIZEOF(F2%DEVPTR) /= 511*SIZEOF(REAL(1, KIND=JPRM))) CALL FIELD_ABORT("ERROR")

        F1_SIZE = SIZEOF(F1%PTR)
        PADDED_F2_SIZE = SIZEOF(F2%DEVPTR) + MODULO(-SIZEOF(F2%DEVPTR), INT(POOL_ALLOC_PADDING_FACTOR, KIND=8))

        IF(HOST_POOL%START_BLK%POS /= F1_SIZE) CALL FIELD_ABORT("ERROR")
#if defined(_CUDA) || defined(_HIP)
        IF(DEVICE_POOL%START_BLK%POS /= F1_SIZE + PADDED_F2_SIZE) CALL FIELD_ABORT("ERROR")
#endif
        IF(SIZEOF(F3%PTR) /= 1024*SIZEOF(REAL(1, KIND=JPRD))) CALL FIELD_ABORT("ERROR")
        IF(SIZEOF(F3%DEVPTR) /= 1024*SIZEOF(REAL(1, KIND=JPRD))) CALL FIELD_ABORT("ERROR")

        !... Check memory pool teardown
        CALL FIELD_DELETE(F1)
        IF(HOST_POOL%START_BLK%POS /= 0) CALL FIELD_ABORT("ERROR")
#if defined(_CUDA) || defined(_HIP)
        IF(DEVICE_POOL%START_BLK%POS /= F1_SIZE + PADDED_F2_SIZE) CALL FIELD_ABORT("ERROR")
#endif
        CALL FIELD_DELETE(F2)
#if defined(_CUDA) || defined(_HIP)
        IF(DEVICE_POOL%START_BLK%POS /= 0) CALL FIELD_ABORT("ERROR")
#endif
        CALL FIELD_DELETE(F3)
        IF(HOST_POOL%START_BLK%NEXT%POS /= 0) CALL FIELD_ABORT("ERROR")
#if defined(_CUDA) || defined(_HIP)
        IF(DEVICE_POOL%START_BLK%NEXT%POS /= 0) CALL FIELD_ABORT("ERROR")
#endif

#if defined(OMPGPU) && defined(_HIP)
        ! Exercise an associated device-pool slice through the normal mapped-pointer path.
        CALL FIELD_NEW(F4, LBOUNDS=[-2,1], UBOUNDS=[4,3], PERSISTENT=.TRUE.)
        CALL F4%GET_HOST_DATA_RDWR(HOST4)
        HOST4 = 7.0_JPRD
        CALL F4%GET_DEVICE_DATA_RDWR(PTR4)
        !$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO COLLAPSE(2) MAP(TO:PTR4)
        DO J = 1, 3
          DO I = -2, 4
            PTR4(I,J) = PTR4(I,J) + REAL(I + J, JPRD)
          ENDDO
        ENDDO
        !$OMP END TARGET TEAMS DISTRIBUTE PARALLEL DO
        CALL F4%GET_HOST_DATA_RDONLY(HOST4)
        DO J = 1, 3
          DO I = -2, 4
            IF (HOST4(I,J) /= 7.0_JPRD + REAL(I + J, JPRD)) CALL FIELD_ABORT('POOLED DATA MISMATCH')
          ENDDO
        ENDDO
        ! Deallocation must use the allocation mode, even if the global setting changes.
        USE_DEVICE_MEM_POOL = .FALSE.
        CALL FIELD_DELETE(F4)
        USE_DEVICE_MEM_POOL = .TRUE.
#endif

        CALL FIELD_MEM_POOL_DELETE()
        DEALLOCATE(DATA2)

END PROGRAM TEST_DEVICE_MEM_POOL
