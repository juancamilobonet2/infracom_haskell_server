import socket

# Set the server's IP address and port
ip_address = "192.168.202.135"
port = 8000

# Create a TCP socket and connect to the server
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.connect((ip_address, port))

# # Send a message to the server
# message = "Hello, server!"
# sock.sendall(message.encode())

# # Receive a response from the server
# response = sock.recv(1024)
# print(f"Server response: {response.decode()}")

print("Connected!")

# Close the socket
sock.close()