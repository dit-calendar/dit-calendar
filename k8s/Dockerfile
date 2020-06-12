FROM roffe/kubectl:latest

ENV oktetoKubeConfig value

COPY deployment /deployment

CMD echo "$oktetoKubeConfig" > okteto-kube.config; export KUBECONFIG=okteto-kube.config; kubectl apply -f deployment
