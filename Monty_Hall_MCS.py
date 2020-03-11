import numpy as np

class MontyHallProblem(object):

    def __init__(self):
        self.doors=np.zeros(3)

    def createRandomPermutation(self):
        car=np.random.randint(3, size=1)
        self.doors[car]=1

    def switch(self,picked_door,door_opened):

        if(door_opened==0):
            if(picked_door==1):
                return 2
            else:
                return 1

        if(door_opened==1):
            if(picked_door==2):
                return 0
            else:
                return 2
        
        if(door_opened==2):
            if(picked_door==1):
                return 0
            else:
                return 1

    def openDoor(self):
        for i in range(3):
            if self.doors[i]==1:
                if i==0:
                    return 1,2
                if i==1:
                    return 0,2
                if i==2:
                    return 0,1

    def simulateSwitch(self,iterations):

        success=0.0
        count=0.0

        for x in range(iterations):

            self.createRandomPermutation()

            goat1,goat2=self.openDoor()
           
            pick=np.random.randint(3, size=1)

            pickGoat=np.random.randint(2, size=1)

            if self.doors[pick]==0:
                if pick==goat1:
                    doorOpened=goat2

                elif pick==goat2:
                    doorOpened=goat1

            if self.doors[pick]==1:
                if pickGoat==0:
                    doorOpened=goat1
                else:
                    doorOpened=goat2


            pick=self.switch(pick,doorOpened)

            if self.doors[pick]==1:
                success=success+1
 
            count=count+1

            self.doors=np.zeros(3)
        
        return (success/count)


    def simulateStay(self,iterations):

        success=0.0
        count=0.0

        for x in range(iterations):

            self.createRandomPermutation()

            goat1,goat2=self.openDoor()
           
            pick=np.random.randint(3, size=1)

            pickGoat=np.random.randint(2, size=1)

            if self.doors[pick]==0:
                if pick==goat1:
                    doorOpened=goat2

                elif pick==goat2:
                    doorOpened=goat1

            if self.doors[pick]==1:
                if pickGoat==0:
                    doorOpened=goat1
                else:
                    doorOpened=goat2

            if self.doors[pick]==1:
                success=success+1
 
            count=count+1

            self.doors=np.zeros(3)
        
        return (success/count)


def main():

    game = MontyHallProblem()
    print("Probability of Winning Car when Switching Doors is: {}".format(game.simulateSwitch(500000)))
    print("Probability of Winning Car when Staying with same Doors is: {}".format(game.simulateStay(500000)))



if __name__=='__main__':
    main()


        



