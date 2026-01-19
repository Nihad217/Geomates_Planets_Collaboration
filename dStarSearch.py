import socket 
import json 
import time 
import math 
from typing import List, Tuple, Dict 
import heapq

class DStarLite: 

    # implementing the dstarLine algorithm 
    
    def __init__(self,grid_size:int=20): 
        self.grid_size=grid_size
        self.rhs={} 
        self.g={} 
        self.U=[] 
        self.km=0 
        self.s_start=None 
        self.s_goal=None 
        self.grid={} 
    
    def heuristic(self,s1:Tuple[int,int],s2:Tuple[int,int])->float: 
        # manhattan distance as heuristic, could be used for our agent while approaching diamonds
        return abs(s1[0]-s2[0])+abs(s1[1]-s2[1]) 

    def calc_key(self,s:Tuple[int,int])->Tuple[float][float]:
        #calculates the key for queue priority 
        min_g_rhs=min(self.g.get(s,float('inf')), self.rhs.get(s,float('inf'))) 
        return(min_g_rhs+self.heuristic(self.s_start,s)+self.km, min_g_rhs) 
    def update_vertex(self, u:Tuple[int,int]): 
        #update the nodes
        
        if u !=self.s_goal: 
            min_cost=float('inf') 
            for dx,dy in [(-1,0),(1,0),(0,-1)(0,1)]: 
                v=(u[0]+dx+u[1]+dy) 
                if v in self.grid and not self.grid[v]: 
                    cost=1+self.g.get(v,float('inf')) 
                    min_cost = min(min_cost, cost)
            self.rhs[u] = min_cost 

        self.U=[(k,s) for k, s in self.U if s!= u] 
        heapq.heapify(self.U)

        if self.g.get(u, float('inf')) != self.rhs.get(u, float('inf')):
            heapq.heappush(self.U, (self.calculate_key(u), u)) 

    def compute_path(self): 


        while self.U and( 
            heapq.nsmallest(1,self.U)[0][0]<self.calculate_key(self.s_start) or 
            self.rhs.get(self.s_start, float('inf')) !=self.g.get(self.s_start,float('inf'))

        ):
            k_old, u = heapq.heappop(self.U)
            
            if k_old < self.calculate_key(u):
                heapq.heappush(self.U, (self.calculate_key(u), u))
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

class geomates: 

    def __init__(self,agent_type:str="disc"):
        
        self.agent_type=agent_type 
        self.socket=None 
        self.position=(0,0) 
        self.diamonds=[] 
        self.other_agent=None 
        self.dstar=DStarLite() 
        self.grid_cache={} 

"""    def connect(self,host:str="localhost",port:int=45678):

        # should connect to the actr game server 
        self.socket=socket.socket(socket.AF_INET,socket.SOCK_STREAM) 
        self.socket.connect((host,port)) 
        printf(f"connected as "{self.agent_type})  
        Abdu
""" 


    

    
