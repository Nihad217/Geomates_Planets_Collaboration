import socket
from dStarSearch import DStarLite

planner = DStarLite()

HOST = "127.0.0.1"
PORT = 5005

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
s.bind((HOST, PORT))
s.listen(1) 
print("Planner ready") 
print(planner.goal) 
print(planner.s_start)

conn, addr = s.accept()
print("ACT-R connected") 
print(">>> RPC SERVER STARTED look in the loop for debug")

while True:
    try:
        data = conn.recv(1024)
        if not data:
            print("Client disconnected")
            break
        data = data.decode()
    except ConnectionResetError:
        print("Connection reset by peer")
        break

    parts = data.strip().split()
    if len(parts) < 6:
        conn.send(b"WAIT\n")
        continue

    x, y = int(parts[1]), int(parts[2])
    gx, gy = int(parts[4]), int(parts[5])

    planner.s_start = (x, y)
    planner.goal = (gx, gy) 
    planner.g.clear()
    planner.rhs.clear()
    planner.U.clear()
    planner.km = 0
    planner.compute_path()

    moves = []
    for dx, dy in [(-1,0),(1,0),(0,-1),(0,1)]:
        v = (x+dx, y+dy)
        moves.append((planner.g.get(v, 1e9), dx, dy))

    _, dx, dy = min(moves)

    # action **vor print setzen**
    if dx == -1: action = "LEFT"
    elif dx == 1: action = "RIGHT"
    elif dy == -1: action = "DOWN"
    elif dy == 1: action = "UP"
    else: action = "WAIT"

    # print immer nach action-Berechnung
    print(">>> RECV FROM ACT-R:", data.strip())
    print(">>> SEND TO ACT-R:", action)

    conn.send((action + "\n").encode())

