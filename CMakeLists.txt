CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(cec)

find_package( EXPAT REQUIRED )
include_directories( ${EXPAT_DIRECTORIES} )

add_subdirectory( libantlr )
add_subdirectory( src )

add_executable( cec-astgrc ${CMAKE_SOURCE_DIR}/src/cec-astgrc.cpp )
target_link_libraries( cec-astgrc cec cecantlr ${EXPAT_LIBRARIES} )
