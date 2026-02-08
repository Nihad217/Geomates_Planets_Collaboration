import socket 
import json 
import time 
import math 
from typing import List, Tuple, Dict 
import heapq
# dstar_server.py 
import json



class DStarLite: 

# implementing the dstarLine algorithm 

    def __init__(self, width=80, height=40): 
        self.grid={} 
        for x in range(width):
            for y in range(height):
                self.grid[(x, y)] = False

        self.g = {}
        self.rhs = {}
        self.U = []
        self.km = 0 
        self.s_start=(10,23)    # implementation based on the first level 
        self.s_goal = [(20, 26)]
        self.goal = self.s_goal[0] 


    def heuristic(self,s1:Tuple[int,int],s2:Tuple[int,int])->float: 
        # manhattan distance as heuristic, could be used for our agent while approaching diamonds
        return abs(s1[0]-s2[0])+abs(s1[1]-s2[1]) 

    def calc_key(self, s: Tuple[int, int]) -> Tuple[float, float]:
        #calculates the key for queue priority 
        min_g_rhs=min(self.g.get(s,float('inf')), self.rhs.get(s,float('inf'))) 
        return(min_g_rhs+self.heuristic(self.s_start,s)+self.km, min_g_rhs) 

    def update_vertex(self, u:Tuple[int,int]): 
#update the nodes

        if u !=self.goal: 
            min_cost=float('inf') 
            for dx, dy in [(-1,0), (1,0), (0,-1), (0,1)]:
                v = (u[0] + dx, u[1] + dy) 
                if v in self.grid and not self.grid[v]: 
                    cost=1+self.g.get(v,float('inf')) 
                    min_cost = min(min_cost, cost)
            self.rhs[u] = min_cost 

        self.U=[(k,s) for k, s in self.U if s!= u] 
        heapq.heapify(self.U)

        if self.g.get(u, float('inf')) != self.rhs.get(u, float('inf')):
            heapq.heappush(self.U, (self.calc_key(u), u)) 

    def compute_path(self):  
        
        heapq.heappush(self.U, (self.calc_key(self.goal), self.goal)) 
        self.g[self.goal] = float('inf')
        self.rhs[self.goal] = 0 


        while self.U and( 
            heapq.nsmallest(1,self.U)[0][0]<self.calc_key(self.s_start) or 
            self.rhs.get(self.s_start, float('inf')) !=self.g.get(self.s_start,float('inf'))

        ):
            k_old, u = heapq.heappop(self.U)
            
            if k_old < self.calc_key(u):
                heapq.heappush(self.U, (self.calc_key(u), u))
            elif self.g.get(u, float('inf')) > self.rhs.get(u, float('inf')):
                self.g[u] = self.rhs[u]
                for dx, dy in [(-1,0), (1,0), (0,-1), (0,1)]:
                    v = (u[0] + dx, u[1] + dy)
                    if v in self.grid:
                        self.update_vertex(v)
            else:
                self.g[u] = float('inf')
                self.update_vertex(u)
                for dx, dy in [(-1,0), (1,0), (0,-1), (0,1)]:
                    v = (u[0] + dx, u[1] + dy)
                    if v in self.grid:
                        self.update_vertex(v) 
#planner = DStarLite()

    def planner_handle_request(line, planner): 

        parts = line.strip().split()
        if len(parts) != 6:
            return "WAIT"

        try:
            x, y = int(parts[1]), int(parts[2])
            gx, gy = int(parts[4]), int(parts[5])
        except ValueError:
            return "WAIT"

        planner.s_start = (x, y)
        planner.goal = (gx, gy)

        planner.compute_path()

        # choose best neighbor
        moves = []
        for dx, dy, name in [(-1,0,"LEFT"), (1,0,"RIGHT"), (0,-1,"DOWN"), (0,1,"UP")]:
            v = (x+dx, y+dy)
            cost = planner.g.get(v, float("inf"))
            moves.append((cost, name))

        moves.sort()
        return moves[0][1]
