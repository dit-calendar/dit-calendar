FROM roffe/kubectl:latest

ENV oktetoToken value


RUN wget https://github.com/okteto/okteto/releases/download/1.8.10/okteto-Linux-x86_64
RUN chmod +x okteto-Linux-x86_64
COPY deployment /deployment

CMD ./okteto-Linux-x86_64 login --token $oktetoToken; ./okteto-Linux-x86_64 namespace vitab; kubectl delete -f deployment; sleep 30; kubectl apply -f deployment
