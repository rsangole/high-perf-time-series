## Docker Image

Build the image using:

```sh
docker build . -t hatmatrix/ds4b_highperfTS:latest
docker push hatmatrix/ds4b_highperfTS:latest
```

Run the image using:

```sh
docker run 
    -d 
    -e PASSWORD=1234 
    -v ~/github/:/home/rstudio/projects/ 
    -p 3838:3838 
    -p 8787:8787 
    hatmatrix/ds4b_highperfTS:latest
```
