FROM roffe/kubectl:latest

ENV oktetoKubeConfig value

COPY deployment /deployment

RUN apk add nginx
COPY default.conf.template /etc/nginx/conf.d/default.conf.template
COPY nginx.conf /etc/nginx/nginx.conf

CMD echo "$oktetoKubeConfig" > okteto-kube.config; export KUBECONFIG=okteto-kube.config; kubectl apply -f deployment; /bin/sh -c "envsubst '\$PORT' < /etc/nginx/conf.d/default.conf.template > /etc/nginx/conf.d/default.conf" && nginx -g 'daemon off;'
