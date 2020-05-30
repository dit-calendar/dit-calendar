FROM nginx:stable-alpine

RUN rm /etc/nginx/conf.d/default.conf
COPY nginx.conf /etc/nginx/conf.d
COPY src/index.html src/Main.js src/styles.css /usr/local/nginx/html/
