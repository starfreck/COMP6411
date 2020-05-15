import socket
import sys

HOST, PORT = "localhost", 9999

data = " ".join(sys.argv[1:])

# Menu for Client
def menu():
    print("""Python DB Menu
1. Find customer
2. Add customer
3. Delete customer
4. Update customer age
5. Update customer address
6. Update customer phone
7. Print report
8. Exit
""")
    return input("Select:")




if __name__ == "__main__":

    while True:
        # Create a socket (SOCK_STREAM means a TCP socket)
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
            # Connect to server and send data
            sock.connect((HOST, PORT))
            choice = menu()
            sock.sendall(bytes(str(choice) + "\n", "utf-8"))

            # Receive data from the server and shut down
            received = str(sock.recv(1024), "utf-8")
        print()
        #print("Sent:     {}".format(data))
        print("Server Response >>> {}".format(received))