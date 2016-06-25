find_program(NOWEB_FOUND noweb)
if(NOWEB_FOUND)
    message( STATUS "noweb found" )
    FILE( GLOB NOWEB_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/*.nw" )

    foreach( NOSRC ${NOWEB_SRCS})
        message( STATUS "Converting noweb to cpp ${NOSRC}" )
        execute_process( 
            COMMAND noweb ${NOSRC} 
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
            )
    endforeach( NOSRC )
endif()

file( GLOB SRCS ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp )
include_directories( ${CMAKE_CURRENT_SOURCE_DIR} )
include_directories( ${CMAKE_SOURCE_DIR}/libantlr )
add_library(cec ${SRCS})
