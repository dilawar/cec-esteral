CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(cec)

find_package( EXPAT REQUIRED )
include_directories( ${EXPAT_DIRECTORIES} )

add_subdirectory( libantlr )
add_subdirectory( src )

SET(CECTARGETS cec-astgrc cec-blifutil cec-dismantle 
    cec-eec cec-expandmodules cec-grc3val
    cec-grcbal cec-grcc2 cec-grcdot cec-grcpdg 
    cec-grcsim cec-pdgccfg cec-scfgc cec-smblif 
    cec-strlxml cec-v5-cmain cec-xmlstrl
    )

include_directories( ${CMAKE_SOURCE_DIR}/libantlr )

foreach( EXE ${CECTARGETS} )
    message( STATUS "Adding executable for ${EXE}" ) 
    add_executable( ${EXE} ${CMAKE_SOURCE_DIR}/src/${EXE}.cpp )
    target_link_libraries( ${EXE} cec ${EXPAT_LIBRARIES} )
endforeach( EXE )

add_executable( cec-grcopt ${CMAKE_SOURCE_DIR}/src/GRCOpt.cpp )

target_link_libraries( cec-grcopt cec ${EXPAT_LIBRARIES} )


# Copy cec to build directory
execute_process(
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/src/cec ${CMAKE_BINARY_DIR}
    )

# Install 
install( TARGETS ${CECTARGETS} cec-grcopt DESTINATION bin )

install( PROGRAMS ${CMAKE_SOURCE_DIR}/src/cec 
    DESTINATION bin
    )

### Tests 
enable_testing( )

set(TEST_DIR ${CMAKE_SOURCE_DIR}/tests )
file( GLOB TEST_SRCS "${TEST_DIR}/test-*.strl" )
foreach( FILEPATH ${TEST_SRCS} )
    get_filename_component( TESTNAME ${FILEPATH} NAME )
    string( REPLACE ".strl" "" NAME_WITHOUT_EXT ${FILEPATH} )
    set( C_SRCS ${NAME_WITHOUT_EXT}.c ${NAME_WITHOUT_EXT}.main.c )

    add_test( NAME test-${TESTNAME}
        COMMAND ${CMAKE_BINARY_DIR}/cec ${FILEPATH}
        WORKING_DIRECTORY ${TEST_DIR}
        )

    add_test( NAME test-${TESTNAME}-compile 
        COMMAND ${CMAKE_C_COMPILER} ${C_SRCS} -o ${NAME_WITHOUT_EXT}
        WORKING_DIRECTORY ${TEST_DIR}
        )

    #add_test( NAME test-${TESTNAME}-run 
    #    COMMAND ${TEST_DIR}/${NAME_WITHOUT_EXT}.out
    #    WORKING_DIRECTORY ${TEST_DIR}
    #    )
endforeach( FILEPATH )


