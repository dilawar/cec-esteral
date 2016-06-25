CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(cec)

find_package( EXPAT REQUIRED )
include_directories( ${EXPAT_DIRECTORIES} )

add_subdirectory( libantlr )
add_subdirectory( src )

SET(CECTARGETS astgrc blifutil dismantle eec expandmodules grc3val
    grcbal grcc2 grcdot grcpdg grcsim pdgccfg scfgc smblif 
    strlxml v5-cmain xmlstrl
    )

include_directories( ${CMAKE_SOURCE_DIR}/libantlr )

foreach( EXE ${CECTARGETS} )
    message( STATUS "Adding executable for cec-${EXE}" ) 
    add_executable( cec-${EXE} ${CMAKE_SOURCE_DIR}/src/cec-${EXE}.cpp )
    target_link_libraries( cec-${EXE} cec cecantlr ${EXPAT_LIBRARIES} )
endforeach( EXE )

add_executable( cec-grcopt ${CMAKE_SOURCE_DIR}/src/GRCOpt.cpp )
target_link_libraries( cec-grcopt cec cecantlr ${EXPAT_LIBRARIES} )


# Copy cec to build directory
execute_process(
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/src/cec ${CMAKE_BINARY_DIR}
    )

### Tests 
enable_testing( )

file( GLOB TEST_SRCS "${CMAKE_SOURCE_DIR}/tests/*.strl" )
foreach( TEST ${TEST_SRCS} )
    add_test( NAME test-${TEST}
        COMMAND ./cec ${TEST}
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
        )
endforeach( TEST )


