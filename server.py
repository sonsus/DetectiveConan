#!/usr/bin/python3.6

import time
import socket

#refer to https://ruslanspivak.com/lsbaws-part1/

PORT = 8002

#socket AF_INET (using ipv4), SOCK_STREAM (using tcp)
listen_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

listen_sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)

#socket is reachable by any address the machine happens to have
#listen_sock.bind(('', 80))
#only visible to the same machine that server is running
listen_sock.bind(('127.0.0.1', PORT))
#server is visible to outside world
#listen_sock.bind((socket.gethostname(), PORT))

#the argument is queue limit
#5 active participants that can wait for a connection

#setting it's the listening socket 
#is never used for sending and reciving
#is used by the server only as a way to get new sockets
listen_sock.listen(5)

print("Serving HTTP on port ", PORT, " ...\n")

for i in range(1,10):
    client_sock, address = listen_sock.accept()

    request = client_sock.recv(1024)

    print("got request ...")
    print(request.decode("utf-8"))

    http_header = b"""\
HTTP/1.1 200 OK
Content-Type: text/html
Content-Length: 15967
Connection: close

"""
#Server: AkamaiGHost
#Date Wed, 24 May 2017 12:15:57 GMT
#Expires: Wed, 24 May 2017 12:15:57 GMT
    with open("scoreboard.html", 'rb') as htmlfile:
        client_sock.sendall(http_header)
        client_sock.sendfile(htmlfile)
    htmlfile.close()

    print("sent scoreboard.html")
    print("")

    client_sock.close()
