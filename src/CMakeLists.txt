file( GLOB SRCS ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp )
include_directories( ${CMAKE_CURRENT_SOURCE_DIR} )
add_library(cec ${SRCS})
