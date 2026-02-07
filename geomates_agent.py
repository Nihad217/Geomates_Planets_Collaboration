import socket 
from dStarSearch import DStarLite  
import RPC_socket


  
class geomatesDecision: 

    def __init__(self,agent_type:str="disc"):
        
        self.agent_type=agent_type 
        self.socket=None 
        self.position=(10,23) 
        self.diamonds=[(20,26)] 
        self.other_agent= None 
        self.dstar=DStarLite() 
        self.grid_cache={}  
	
    def decide_next_action(self):
    	# cost calculation
        self.dstar.s_start = self.position
        self.dstar.compute_path()  
         

        if not self.diamonds:
            return None
        self.dstar.goal = self.diamonds[0]  

        print("Start:", self.position, "Goal:", self.dstar.goal)


        # next_pos decision
        next_pos = self.next_move()

       
	
    def next_move(self):
        x, y = self.position
        best = None
        best_cost = float('inf')

        for dx, dy in [(-1,0), (1,0), (0,-1), (0,1)]:
            v = (x + dx, y + dy)
            cost = self.dstar.g.get(v, float('inf'))
            if cost < best_cost:
                best_cost = cost
                best = v

        return best
    

    


    