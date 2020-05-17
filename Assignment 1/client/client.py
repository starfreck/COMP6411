import sys
import json
import socket
from collections import OrderedDict

# Global Vars
HOST, PORT, MAX_SIZE = "localhost", 9999, 1024

# Menu for Client
def menu():
    print("Python DB Menu\n1. Find customer\n2. Add customer\n3. Delete customer\n4. Update customer age\n5. Update customer address\n6. Update customer phone\n7. Print report\n8. Exit\n")
    return input("Select:").strip()   
    
# 1. Find customer
def find_customer():
    # Get customer name
    customer_name = input("Enter a customer's name :").strip()
    # Validate customer_name
    if not customer_name or is_int(customer_name):
        print_error("Customer's name cannot be empty and/or Integer")
        return

    # Preparing Data to send   
    data = {"action":"find_customer","name":customer_name}
    # send customer_name to server
    received = communicate(json.dumps(data))
    # Receive data from the server
    print_server_msg(received)

# 2. Add customer
def add_customer():    
    # Get customer details
    customer_name    = input("Enter a new customer's name:").strip()
    # Validate customer_name
    if not customer_name or is_int(customer_name):
        print_error("Customer's name cannot be empty and/or Integer")
        return

    customer_age     = input("Enter a new customer's age:").strip()
    customer_address = input("Enter a new customer's address:").strip()
    customer_phone   = input("Enter a new customer's phone:").strip()

    # Create user's dictionary
    data = {"action":"add_customer","name":customer_name,"age":customer_age,"address":customer_address,"phone":customer_phone}
    # send customer_name to server
    received = communicate(json.dumps(data))
    # Receive data from the server
    print_server_msg(received)


# 3. Delete customer
def delete_customer():
    # Get customer name
    customer_name = input("Enter a customer's name :").strip()
    # Validate customer_name
    if not customer_name or is_int(customer_name):
        print_error("Customer's name cannot be empty and/or Integer")
        return

    # Preparing Data to send   
    data = {"action":"delete_customer","name":customer_name}
    # send customer_name to server
    received = communicate(json.dumps(data))
    # Receive data from the server
    print_server_msg(received)

# 4. Update customer age
def update_customer_age():
    # Get customer name
    customer_name = input("Enter a customer's name :").strip()
    # Validate customer_name
    if not customer_name or is_int(customer_name):
        print_error("Customer's name cannot be empty and/or Integer")
        return

    # Get new age of customer
    customer_age = input("Enter new age for {}:".format(customer_name)).strip()
    # Validate customer_age
    if not customer_age or not is_int(customer_age):
        print_error("Customer's age cannot be empty and/or String")
        return

    # Preparing Data to send   
    data = {"action":"update_customer_age","name":customer_name,"age":customer_age}
    # send customer_name to server
    received = communicate(json.dumps(data))
    # Receive data from the server
    print_server_msg(received)

# 5. Update customer address
def update_customer_address():
    #Get customer name
    customer_name = input("Enter a customer's name :").strip()
    # Validate customer_name
    if not customer_name or is_int(customer_name):
        print_error("Customer's name cannot be empty and/or Integer")
        return
    # Get new address of customer
    customer_address = input("Enter new address for {}:".format(customer_name)).strip()
    # Validate customer_address
    if not customer_address or is_int(customer_address):
        print_error("Customer's age cannot be empty and/or String")
        return

    # Preparing Data to send   
    data = {"action":"update_customer_address","name":customer_name,"address":customer_address}
    # send customer_name to server
    received = communicate(json.dumps(data))
    # Receive data from the server
    print_server_msg(received)

# 6. Update customer phone
def update_customer_phone():
    # Get customer name
    customer_name = input("Enter a customer's name :").strip()
    # Validate customer_name
    if not customer_name or is_int(customer_name):
        print_error("Customer's name cannot be empty and/or Integer")
        return
    # Get new phone of customer
    customer_phone = input("Enter new phone for {}:".format(customer_name)).strip()
    # Validate customer_phone
    if not customer_phone:
        print_error("Customer's phone cannot be empty")
        return

    # Preparing Data to send   
    data = {"action":"update_customer_phone","name":customer_name,"phone":customer_phone}
    # send customer_name to server
    received = communicate(json.dumps(data))
    # Receive data from the server
    print_server_msg(received)

# 7. Print report
def print_report():
    # Preparing Data to send   
    data = {"action":"print_report"}
    # send customer_name to server
    received = communicate(json.dumps(data))
    # Converting into 'dict'
    customers = json.loads(received)    
    
    print ("\n{:<10} {:<10} {:<20} {:<25}".format('Name','Age','Address','Phone'))
    for customer in customers.values():
        print("{:<10} {:<10} {:<20} {:<25}".format(customer['name'],customer['age'],customer['address'],customer['phone']))
    print()

# Conver String to Bytes
def str_to_byte(string):
    return bytes(string + "\n", "utf-8")

# Check if given value is Integer
def is_int(val):
    try:
        num = int(val)
    except ValueError:
        return False
    return True

# Print Error message 
def print_error(message):
    print("\n\033[91m Error >>> {}\033[00m\n".format(message))

# Print Server message 
def print_server_msg(message):
    print("\n\033[92m Server Response >>> {}\033[00m\n".format(message))

def communicate(data):
    try:
        # Create a socket (SOCK_STREAM means a TCP socket)
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
            # Connect to server and send data
            sock.connect((HOST, PORT))
            sock.sendall(bytes(data + "\n", "utf-8"))

            # Receive data from the server and shut down
            received = str(sock.recv(1024), "utf-8")
        return received

    except Exception as e:
        print_error("Server is not running "+e)
    finally:
        # Closing Socket
        sock.close()

if __name__ == "__main__":

    while True:
        # build Switch case here
        try:
            choice = int(menu())
            # 1. Find customer
            if choice == 1 :
                find_customer()
            elif choice == 2 :
                add_customer()
            elif choice == 3 :
                delete_customer()
            elif choice == 4 :
                update_customer_age()
            elif choice == 5 :
                update_customer_address()
            elif choice == 6 :
                update_customer_phone()
            elif choice == 7 :
                print_report()
            elif choice == 8 :
                print("\nGood bye")
                break
            else:
                print_error("Please, select valid option")
        except ValueError:
            print_error("Only \"Integer\" values are accepted")
        except Exception as e:
            print_error(e)