FROM haskell:8.0.2

WORKDIR /app

ADD . /app

RUN apt-get update && apt-get install libpq-dev -y && stack install

EXPOSE 80

ENV NAME World

CMD ["server"]
