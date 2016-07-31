deploy:
	rhc apps -v | grep "SSH.*kirosauth" | awk '{print $$2;}' | xargs -I{} scp -r target/scala-2.11/kiros-auth* {}:~/app-root/data/.
