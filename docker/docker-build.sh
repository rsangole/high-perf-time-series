#!/usr/bin/env bash
      
DOCKER_NAME=hatmatrix/ds4b_highperfts:latest

docker build . -t $DOCKER_NAME

if [[ $? = 0 ]] ; then
    echo "Pushing image"
    docker push $DOCKER_NAME
    else
      echo "Docker build failed"
    fi