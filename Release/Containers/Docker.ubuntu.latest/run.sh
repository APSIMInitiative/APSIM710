#! /bin/sh

id=`docker build .`

docker tag $id apsimbuilder2

# run on port 2500, share apache2 html dir:
docker run -p 2500:2500 -v /var/www/html:/var/www/html -d -i apsimbuilder2

# curl -i localhost:2500/status

