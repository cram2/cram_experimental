cmake_minimum_required(VERSION 2.8.3)
project(cram_sherpa_blue_wasp)
find_package(catkin REQUIRED COMPONENTS roslisp)
catkin_package(CATKIN_DEPENDS roslisp)

add_lisp_executable(blue_wasp cram-sherpa-blue-wasp blue-wasp:main)